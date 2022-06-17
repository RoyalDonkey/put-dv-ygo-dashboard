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

# Load data
cards <- read.csv('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cards/cards.csv')
decks <- fromJSON(paste(readLines('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/decks/decks.json', warn=F), collapse=''))

# Function to obtain card metadata from its ID code in O(1) time
cards_cache <- new.env(hash=TRUE, parent=emptyenv())
for (i in 1:nrow(cards)) {
  card <- cards[i,]
  assign(paste0('c', card$id), card, envir=cards_cache)
}
rm(card)
getCard <- function(id) {
  return(cards_cache[[paste0('c', id)]])
}

# Cache list of all cards in all decks
# Build a single list of all cards across all decks
decks_cards <- c()
for (deck in decks) {
  for (card_id in deck$deck$main) {
    decks_cards[length(decks_cards) + 1] <- list(getCard(card_id)[c('id', 'type')])
  }
}
rm(deck)
decks_cards <- as.data.frame(do.call(rbind, decks_cards))
decks_cards <- setDT(decks_cards)[, .N, decks_cards]

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
    
    output$PLT_DeckBreakdown <- renderPlot({
      # TODO
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
    
    # TODO (Meme Counters)
})
