#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

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
      
      /* Layout and appearance of UI controls */
      .control_rack > div {
          display: inline-block;
          vertical-align: top;
          margin: 10px;
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
                 h2('Popular Cards'),
                 p('This page grants insight into the cards used the most by
                   Yu-Gi-Oh! players all around the world. You can filter by
                   card type and further explore the top 100 with the slider.'),
                 div(class='control_rack',
                  radioButtons('CTL_PopularCards_TypeRadio',
                                'Card Type',
                                choiceNames=c('Monster', 'Spell', 'Trap'),
                                choiceValues=c('monster', 'spell', 'trap'),
                                inline=TRUE),
                  sliderInput('CTL_PopularCards_CountSlider',
                              'Top Count',
                              min=1, max=100, value=c(1, 10), step=1),
                 ),
                 plotOutput('PLT_PopularCards')
                 ),
        tabPanel('ATK/DEF Stats', value='NAV_AtkDefStats', icon=icon('shield-alt'),
                 # TODO
                 h2('ATK/DEF Stats'),
                 p('This page allows you to view the distribution of ATK and DEF
                   values of Monster cards. You can filter by type, attribute,
                   level and even status according to the 3 popular Duel
                   Monsters formats (TCG, OCG and Goat).'),
                 p('All filter categories are conjunctive (e.g. "Normal" and
                   "DARK" will include Monsters which are both Normal and DARK),
                   while all options within a filter category are disjunctive
                   (e.g. "FIRE" and "WATER" will include Monsters which are of
                   either FIRE or WATER attribute).'),
                 div(class='control_rack',
                   checkboxGroupInput('CTL_AtkDefStats_Type',
                                      'Monster Types',
                                      c('Normal', 'Effect', 'Fusion', 'Link',
                                        'Pendulum', 'XYZ', 'Synchro', 'Tuner'),
                                      selected=c('Normal'),
                                      inline=TRUE),
                   checkboxGroupInput('CTL_AtkDefStats_Attr',
                                      'Attributes',
                                      c('DARK', 'WIND',  'EARTH',
                                        'FIRE', 'LIGHT', 'WATER',
                                        'DIVINE'),
                                      selected=c('DARK', 'WIND',  'EARTH',
                                                 'FIRE', 'LIGHT', 'WATER',
                                                 'DIVINE'),
                                      inline=TRUE),
                   checkboxGroupInput('CTL_AtkDefStats_Lvl',
                                      'Levels (Stars)',
                                      seq(0, 12),
                                      selected=seq(2, 8),
                                      inline=TRUE)
                 ),
                 div(class='control_rack',
                   selectInput('CTL_AtkDefStats_Format',
                               'Format',
                               choices=c('TCG', 'OCG', 'Goat'),
                               selected='TCG'),
                   checkboxGroupInput('CTL_AtkDefStats_Status',
                                      'Card Status',
                                      c('Unlimited', 'Semi-Limited', 'Limited', 'Banned'),
                                      selected=c('Unlimited', 'Semi-Limited','Limited'),
                                      inline=TRUE)
                 ),
                 plotOutput('PLT_AtkDefStats')
                 ),
        tabPanel('Deck Breakdown', value='NAV_DeckBreakdown', icon=icon('chart-pie'),
                 # TODO
                 h2('Deck Breakdown'),
                 p('<brief description of the plot and how to use it>'),
                 plotOutput('PLT_DeckBreakdown')
                 ),
        tabPanel('Card Explorer', value='NAV_CardExplorer', icon=icon('search'),
                 h2('Card Explorer'),
                 p('Search for specific cards by name, ATK/DEF or other attributes. 
                    Click on a card to view its fully-sized picture.'),
                 dataTableOutput("DATA_CardExplorer")
                 ),
        tabPanel('Meme Counters', value='NAV_MemeCounters', icon=icon('ankh'),
                 # TODO
                 h2('Meme Counters')
                 # not sure what exactly will end up here yet
                 ),
        )
    )
)
