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
      # TODO
      insertUI(selector='#TITLE',
               where='afterEnd',
               ui=HTML('<p>example follow-up paragraph</p>'),
               immediate=T)
    })
    
    output$PLT_DeckBreakdown <- renderPlot({
      # TODO
    })
    
    # TODO (Meme Counters)
})
