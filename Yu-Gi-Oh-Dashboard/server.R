#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rjson)

# Load data
cards <- read.csv('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/cards/cards.csv')
decks <- fromJSON(paste(readLines('https://raw.githubusercontent.com/RoyalDonkey/put-dv-ygo-dashboard/main/data/decks/decks.json', warn=F), collapse=''))

# Function to obtain card metadata from its ID code
getCard <- function(id) {
  return(cards[cards$id == id,])
}

# Define server logic
shinyServer(function(input, output) {

    output$PLT_PopularCards <- renderPlot({
      # TODO
    })
    
    output$PLT_AtkDefStats <- renderPlot({
      # TODO
    })
    
    output$PLT_DeckBreakdown <- renderPlot({
      # TODO
    })
    
    # TODO (Meme Counters)
})
