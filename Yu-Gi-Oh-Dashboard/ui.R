#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Constants
IMG_TITLE_SRC <- 'https://cdn3.iconfinder.com/data/icons/7-millennium-items/512/Milennium_Puzzle_Icon_Colored-512.png'

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Inline custom CSS
    tags$head(
      tags$style(HTML('
      /* Tweak alignment of the title text relative to title image */
      #TITLE {
          vertical-align: bottom;
      }
      
      /* Override navbar width and height */
      .col-sm-4 {
          width: auto;
          margin-right: 20px;
          height: 100vh;
      }
      '))
    ),
  
    # Dashboard title
    HTML('<h1>'),
    img(src=IMG_TITLE_SRC, width=48),
    span('Yu-Gi-Oh! Duel Monsters Dashboard', id='TITLE'),
    HTML('</h1>'),

    # Body
    navlistPanel(
        tabPanel('Home Page', value='NAV_HomePage', icon=icon('home'),
                 h2('Home Page'),
                 p('Welcome to the Yu-Gi-Oh! Duel Monsters Dashboard!')
                 ),
        tabPanel('Popular Cards', value='NAV_PopularCards', icon=icon('star'),
                 # TODO
                 h2('Popular Cards'),
                 p('<brief description of the plot and how to use it>'),
                 plotOutput('PLT_PopularCards')
                 ),
        tabPanel('ATK/DEF Stats', value='NAV_AtkDefStats', icon=icon('shield-alt'),
                 # TODO
                 h2('ATK/DEF Stats'),
                 p('<brief description of the plot and how to use it>'),
                 plotOutput('PLT_AtkDefStats')
                 ),
        tabPanel('Deck Breakdown', value='NAV_DeckBreakdown', icon=icon('chart-pie'),
                 # TODO
                 h2('Deck Breakdown'),
                 p('<brief description of the plot and how to use it>'),
                 plotOutput('PLT_DeckBreakdown')
                 ),
        tabPanel('Meme Counters', value='NAV_MemeCounters', icon=icon('ankh'),
                 # TODO
                 h2('Meme Counters')
                 # not sure what exactly will end up here yet
                 ),
        )
    )
)
