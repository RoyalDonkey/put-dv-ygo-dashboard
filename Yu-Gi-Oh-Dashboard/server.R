#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(rjson)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(tidyr)
library(scales)
library(flexdashboard)

# Load data
cards <- read.csv('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cards/cards.csv')
load(url('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cache/cards_cache.Rdata'))
load(url('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cache/decks_contain.Rdata'))
load(url('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cache/decks_cards.Rdata'))
load(url('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cache/DECKS_CARDS_COUNT.Rdata'))
load(url('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cache/DECKS_COUNT.Rdata'))
load(url('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cache/deckbind.Rdata'))

# Function to obtain card metadata from its ID code in O(1) time
getCard <- function(id) {
  return(cards_cache[[paste0('c', id)]])
}

# Function to obtain the number of decks a card appears in in O(1) time
getCardDecks <- function(id) {
  return(decks_contain[[paste0('c', id)]])
}

# Unified theme for all plots
custom_theme <- theme(legend.title=element_text(size=15),
                      legend.text=element_text(size=14),
                      axis.title.x=element_text(size=13),
                      axis.title.y=element_text(size=13),
                      axis.text.x=element_text(size=14),
                      axis.text.y=element_text(size=14),
                      plot.title=element_text(size=16, hjust=0.5, vjust=0.5),
                      plot.subtitle=element_text(size=15, hjust=0.5, vjust=0.5))

# Split violin plot function
# source: https://stackoverflow.com/a/45614547/10792539
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# Returns the number a given card appears in all decks
decks_count <- function(card_id) {
  count <- decks_cards[decks_cards$id == card_id,]$N
  return(if (length(count) == 0) { 0 } else { count })
}

# Function for rendering gauge type 1: the number of card(s) out of all of them
gauge1 <- function(num) {
  maxNum <- DECKS_COUNT
  return(gauge(num, min=0, max=maxNum, label='CARDS'))
}

# Function for rendering gauge type 2: the number of decks contain at least
# 1 copy of at least 1 of the given cards.
gauge2 <- function(card_ids) {
  num <- 0
  for (card_id in card_ids) {
    dcount <- getCardDecks(card_id)
    if (dcount > num) {
      num <- dcount
    }
  }
  maxNum <- DECKS_COUNT
  return(gauge(num, min=0, max=maxNum, label='DECKS'))
}

# Function for rendering gauge type 3: the total number of copies of all
# given cards across all decks.
gauge3 <- function(card_ids) {
  num <- 0
  for (search_id in card_ids) {
    num <- num + decks_count(search_id)
  }
  maxNum <- DECKS_CARDS_COUNT
  return(gauge(num, min=0, max=maxNum, label='COPIES'))
}

# Cache Meme gauges data
potofgreed_id <- id <- cards[cards$name == 'Pot of Greed',]$id
blueeyeswhitedragon_id <- cards[cards$name == 'Blue-Eyes White Dragon',]$id
blueeyes <- dplyr::filter(cards, grepl('Blue-Eyes', name) |
                                 grepl('Blue-Eyes', desc))
blueeyes_ids <- c()
for (i in 1:nrow(blueeyes)) {
  blueeyes_ids[length(blueeyes_ids) + 1] <- blueeyes[i,]$id
}
exodia_ids <- c(cards[cards$name == 'Exodia the Forbidden One',]$id,
                cards[cards$name == 'Left Arm of the Forbidden One',]$id,
                cards[cards$name == 'Right Arm of the Forbidden One',]$id,
                cards[cards$name == 'Left Leg of the Forbidden One',]$id,
                cards[cards$name == 'Right Leg of the Forbidden One',]$id)
moreexodia <- dplyr::filter(cards, grepl('(Exodia|Forbidden One)', name) |
                                   grepl('(Exodia|Forbidden One)', desc))
moreexodia_ids <- c()
for (i in 1:nrow(moreexodia)) {
  moreexodia_ids[length(moreexodia_ids) + 1] <- moreexodia[i,]$id
}
darkmagician_id <- cards[cards$name == 'Dark Magician',]$id
moredarkmagician <- dplyr::filter(cards, grepl('Dark Magician', name) |
                                         grepl('Dark Magician', desc))
moredarkmagician_ids <- c()
for (i in 1:nrow(moredarkmagician)) {
  moredarkmagician_ids[length(moredarkmagician_ids) + 1] <- moredarkmagician[i,]$id
}
god_ids <- c(cards[cards$name == 'Slifer the Sky Dragon',]$id,
             cards[cards$name == 'The Winged Dragon of Ra',]$id,
             cards[cards$name == 'Obelisk the Tormentor',]$id)

# Define server logic
shinyServer(function(input, output) {

    output$PLT_PopularCards <- renderPlot({
      # Fetch user input
      type  <- input$CTL_PopularCards_TypeRadio
      count <- input$CTL_PopularCards_CountSlider
      
      # Filter the cards to show
      data <- NA
      if (type == 'monster') {
        data <- dplyr::filter(decks_cards, grepl('Monster', type))
      } else if (type == 'spell') {
        data <- dplyr::filter(decks_cards, grepl('^Spell Card$', type))
      } else if (type == 'trap') {
        data <- dplyr::filter(decks_cards, grepl('^Trap Card$', type))
      } else {
        print(paste('ERROR! Unknown type:', type))
      }
      data <- data[order(data$N, decreasing=TRUE),][count[1]:count[2]]
      
      # Convert to full card information
      data_full <- c()
      for (card_id in data$id) {
        data_full[length(data_full) + 1] <- list(getCard(card_id))
      }
      data_full <- cbind(as.data.frame(do.call(rbind, data_full)), count=data$N)
      rm(data)
      
      # Construct plot
      ggplot(data_full) +
        aes(x=reorder(name, count), y=count, fill=race) +
        geom_col() +
        coord_flip() +
        theme_minimal() +
        labs(title=paste0('Top ', count[1], '-', count[2], ' ', type, ' cards'),
             subtitle='Ranked by the total number of occurrences in all decks',
             y='no. occurrences') +
        custom_theme +
        theme(legend.title=element_blank(),
              axis.title.y=element_blank()) +
        geom_text(aes(label=count), hjust=1.1)
    })
    
    output$PLT_AtkDefStats <- renderPlot({
      type <- input$CTL_AtkDefStats_Type
      attr <- input$CTL_AtkDefStats_Attr
      lvl  <- input$CTL_AtkDefStats_Lvl
      format  <- input$CTL_AtkDefStats_Format
      status  <- input$CTL_AtkDefStats_Status
      
      data  <- dplyr::filter(cards, grepl('Monster', type))
      if (length(type) == 0 || length(attr) == 0 ||
          length(lvl)  == 0 || length(status) == 0) {
        data <- data[0,]
      }
      
      # Filter type, attribute and level
      regex <- paste0('(', paste(type, collapse='|'), ')')
      data  <- dplyr::filter(data, grepl(regex, type))
      regex <- paste0('^(', paste(attr, collapse='|'), ')$')
      data  <- dplyr::filter(data, grepl(regex, attribute))
      regex <- paste0('^(', paste(lvl,  collapse='|'), ')$')
      data  <- dplyr::filter(data, grepl(regex, level))
      
      # Replace empty string with "Unlimited" for convenience
      for (col in c('ban_tcg', 'ban_ocg', 'ban_goat')) {
        data[col][data[col] == ""] <- 'Unlimited'
      }
      
      # Filter TCG, OCG and Goat status
      if (format == 'TCG') {
        regex <- paste0('^(', paste(status, collapse='|'), ')$')
        data  <- dplyr::filter(data, grepl(regex, ban_tcg))
      } else if (format == 'OCG') {
        regex <- paste0('^(', paste(status, collapse='|'), ')$')
        data  <- dplyr::filter(data, grepl(regex, ban_ocg))
      } else if (format == 'Goat') {
        regex <- paste0('^(', paste(status, collapse='|'), ')$')
        data  <- dplyr::filter(data, grepl(regex, ban_goat))
      } else {
        print('ERROR! Unknown format!')
      }
      
      # Convert wide (ATK, DEF) to long (ATKorDEF, value)
      data <- rename(data, ATK=atk, DEF=def)
      data <- gather(data, ATKorDEF, value, ATK:DEF, factor_key=TRUE)
      
      # Construct plot
      ggplot(data) +
        aes(x=factor(level), y=value, fill=ATKorDEF) +
        geom_split_violin() +
        theme_minimal() +
        labs(title='ATK/DEF distributions', x='Stars') +
        custom_theme +
        theme(legend.title=element_blank(),
              axis.title.y=element_blank()) +
        scale_y_continuous(breaks=seq(0, 10000, 500))
    })
    
    output$PLT_DeckBreakdown <- renderPlotly({
      #Take input of which parameter was chosen to visualize
      type  <- input$CTL_DeckBreakdown_TypeSelector
      
      #Create data table from deckbind to visualize
      data <- NA
      if (type == 'Deck Type') {
        data <- setDF(deckbind[,1])
        data <- data %>%
          group_by(deck_type) %>%
          summarize(amount = n())
        data <- data[order(data$amount, decreasing = TRUE),]
      } else if (type == 'Deck Master') {
        data <- setDF(deckbind[,2])
        data <- data %>%
          group_by(deck_master) %>%
          summarize(amount = n())
        data <- data[order(data$amount, decreasing = TRUE),]
        data <- data[1:10,]
      } else if (type == 'Official/Trading Deck') {
        data <- setDF(deckbind[,3])
        data <- data %>%
          group_by(tcg_ocg) %>%
          summarize(amount = n())
        data <- data[order(data$amount, decreasing = TRUE),]
        data$amount[1] <- data$amount[1] + data$amount[2] + data$amount[5] + data$amount[7]
        data$amount[4] <- data$amount[4] + data$amount[8] + data$amount[9]
        data$amount[6] <- data$amount[6] + data$amount[10]
        data <- data[c(1, 3, 4, 6),]
      } else {
        print(paste('ERROR! Unknown type:', type))
      }
      
      #Change column names for convenience
      colnames(data) <- c('distinction','amount')
      
      #Display plotly pie chart
      plot_ly(data, labels = ~distinction, values = ~amount, type = 'pie',
              textposition = 'outside',
              textinfo = 'percent',
              hovertemplate = paste('%{label}', '<br>Amount: %{value}', '<extra></extra>'))%>%
      layout(legend = list(x = 10, y = 0.9))
    })
  
    output$DATA_CardExplorer <- renderDataTable({
      # Make from card names links to pictures
      data <- cards
      for (i in 1:length(data$name)) {
        data$id[i]  <- paste0('<a href="', data$image_url[i], '"><img src="',
                              data$image_url_small[i], '" height="100"/></a>')
      }
      
      # Create datatable with edited parameters
      datatable(data, 
                options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 20, 50, 100), pageLength = 5,
                               columnDefs = list(list(visible=FALSE, 
                                                      targets=c('linkval','linkmarkers', 'image_url',
                                                                'image_url_small','ban_tcg','ban_ocg',
                                                                'ban_goat')))
                               ),
                rownames = FALSE, colnames = c('Card', 'Name','Type','Description',
                                               'Attack','Defense','Level','Race',
                                               'Attribute','Scale','Archetype'),
                escape = FALSE
                )
    })
    
    # Memes: Pot of Greed
    output$PLT_Meme_PotOfGreed1 <- renderGauge({ gauge1(1) })
    output$PLT_Meme_PotOfGreed2 <- renderGauge({ gauge2(c(potofgreed_id)) })
    output$PLT_Meme_PotOfGreed3 <- renderGauge({ gauge3(c(potofgreed_id)) })
    
    # Memes: Blue-Eyes White Dragon
    output$PLT_Meme_BlueEyesWhiteDragon1 <- renderGauge({ gauge1(1) })
    output$PLT_Meme_BlueEyesWhiteDragon2 <- renderGauge({ gauge2(c(blueeyeswhitedragon_id)) })
    output$PLT_Meme_BlueEyesWhiteDragon3 <- renderGauge({ gauge3(c(blueeyeswhitedragon_id)) })
    
    # Memes: Blue-Eyes in name/description cards
    output$PLT_Meme_BlueEyes1 <- renderGauge({ gauge1(nrow(blueeyes)) })
    output$PLT_Meme_BlueEyes2 <- renderGauge({ gauge2(blueeyes_ids) })
    output$PLT_Meme_BlueEyes3 <- renderGauge({ gauge3(blueeyes_ids) })
    
    # Memes: Exodia the Forbidden One
    output$PLT_Meme_Exodia1 <- renderGauge({ gauge1(length(exodia_ids)) })
    output$PLT_Meme_Exodia2 <- renderGauge({ gauge2(exodia_ids) })
    output$PLT_Meme_Exodia3 <- renderGauge({ gauge3(exodia_ids) })
    
    # Memes: Exodia or Forbidden One in name/description
    output$PLT_Meme_MoreExodia1 <- renderGauge({ gauge1(nrow(moreexodia)) })
    output$PLT_Meme_MoreExodia2 <- renderGauge({ gauge2(moreexodia_ids) })
    output$PLT_Meme_MoreExodia3 <- renderGauge({ gauge3(moreexodia_ids) })
    
    # Memes: Dark Magician
    output$PLT_Meme_DarkMagician1 <- renderGauge({ gauge1(1) })
    output$PLT_Meme_DarkMagician2 <- renderGauge({ gauge2(c(darkmagician_id)) })
    output$PLT_Meme_DarkMagician3 <- renderGauge({ gauge3(c(darkmagician_id)) })
    
    # Memes: Blue-Eyes in name/description cards
    output$PLT_Meme_MoreDarkMagician1 <- renderGauge({ gauge1(nrow(moredarkmagician)) })
    output$PLT_Meme_MoreDarkMagician2 <- renderGauge({ gauge2(moredarkmagician_ids) })
    output$PLT_Meme_MoreDarkMagician3 <- renderGauge({ gauge3(moredarkmagician_ids) })
    
    # Memes: God cards
    output$PLT_Meme_GodCards1 <- renderGauge({ gauge1(length(god_ids)) })
    output$PLT_Meme_GodCards2 <- renderGauge({ gauge2(god_ids) })
    output$PLT_Meme_GodCards3 <- renderGauge({ gauge3(god_ids) })
})
