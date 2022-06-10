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
      # Fetch user input
      type <- input$CTL_PopularCards_RadioButtons
      
      # Build a single list of all cards across all decks
      data <- c()
      for (deck in decks) {
        for (card in deck$deck$main) {
          data <- data(getCard(card))
        }
      }
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
