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
library(flexdashboard)
library(plotly)

# Constants
IMG_TITLE_SRC <- 'https://cdn3.iconfinder.com/data/icons/7-millennium-items/512/Milennium_Puzzle_Icon_Colored-512.png'

# Function that, given a discord video ID, returns an HTML embed code.
discordVideo <- function(id) {
  return(paste0('<video class="video-2HW4jD" playsinline="" width="420" height="315" ',
                'poster="https://media.discordapp.net/attachments/', id,
                '?format=jpeg&amp;width=420&amp;height=315" preload="metadata" ',
                'width="364" src="https://cdn.discordapp.com/attachments/', id,
                '" controls></video>'))
}
youtubeVideo <- function(id) {
  return(paste0('<iframe width="560" height="315" ',
                'src="https://www.youtube.com/embed/', id, '" title="YouTube"',
                'frameborder="0" allow="accelerometer; autoplay; ',
                'clipboard-write; encrypted-media; gyroscope; ',
                'picture-in-picture" allowfullscreen></iframe>'))
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Inline custom CSS
    tags$head(
      tags$title('Yu-Gi-Oh! Duel Monsters Dashboard'),
      tags$head(tags$link(rel="shortcut icon", href=IMG_TITLE_SRC)),
      tags$style(HTML('
      /* Tweak alignment of the title text relative to title image */
      #TITLE {
          vertical-align: bottom;
      }
      
      /* Fix navlist */
      .col-sm-4 {
          position: fixed;
          z-index: 1;
          width: auto;
          margin-left: 10px;
      }
      
      /* Navlist colors */
      .nav-pills > li.active > a, .nav-pills > li.active > a:focus, .nav-pills > li.active > a:hover {
          color: #fff;
          background-color: #973c10;
      }
      .col-sm-4 a {
          color: #5e402e;
      }
      
      /* Main content panel */
      .col-sm-8 {
          overflow: hidden;
          margin-left: 220px;
          background-color: white;
          border-radius: 4px;
      }
      
      /* Title bar */
      .container-fluid > h1 {
          color: white;
          background-color: #0005;
          margin: 5px 5px 5px -5px;
          padding: 10px 10px 10px 10px;
          border-radius: 10px;
      }
      
      /* Add background image */
      body {
        background-image: url("https://wallpapercave.com/wp/wp4199668.jpg");
        background-attachment: fixed;
        background-size: cover;
        background-repeat: no-repeat;
      }
      
      /* Layout and appearance of UI controls */
      .control_rack > div {
          display: inline-block;
          vertical-align: top;
          margin: 10px;
      }
      
      /* Layout for gauge rows in Meme Counters */
      .gauge_row > div {
          display: inline;
      }
      .gauge_row > div > svg {
          width: 33%;
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
                 p('Welcome to the Yu-Gi-Oh! Duel Monsters Dashboard!'),
                 p('The dashboard derives all its data from these datasets:'),
                 HTML('<ul>
                      <li><a href="https://www.kaggle.com/datasets/ioexception/yugioh-cards">Yu-Gi-Oh! Cards</a></li>
                      <li><a href="https://www.kaggle.com/datasets/ioexception/yugioh-decks">Yu-Gi-Oh! Decks</a></li>
                      </ul>'),
                 p('These datasets comprise of data fetched using the',
                   a('ygoprodeck.com', href='https://ygoprodeck.com/'), 'API.'),
                 h3('Soundtrack'),
                 p('There are many things Yu-Gi-Oh! is known for, and it definitely
                   does not disappoint in terms of its soundtrack. Both the Japanese
                   originals and the American versions are very memorable.'),
                 p('Feel free to enhance your visit with some quality sounds:'),
                 HTML(paste0('<iframe width="560" height="315" ',
                             'src="https://www.youtube.com/embed/videoseries?list=PL4BE209DF75AAF47F" ',
                             'title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; ',
                             'clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                 HTML(paste0('<iframe width="560" height="315" ',
                             'src="https://www.youtube.com/embed/_dq0NCCN5Ms" ',
                             'title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; ',
                             'clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                 h3('Authors'),
                 p('This dashboard was made for a university project by Piotr Kaszubski and Andrii Kozar.'),
                 p('The source code is available at ',
                   a('https://github.com/RoyalDonkey/put-dv-ygo-dashboard'))
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
                 h2('Deck Breakdown'),
                 p('This page allows to choose one of 3 plots that demonstrates 
                   the variety of decks based on separate parameters. The measurement 
                   is amount of decks.'),
                 div(class='control_rack',
                     selectInput('CTL_DeckBreakdown_TypeSelector',
                                 label = 'Choose a parameter',
                                 choices = c('Deck Type','Deck Master','Official/Trading Deck'))
                 ),
                 plotlyOutput('PLT_DeckBreakdown')
                 ),
        tabPanel('Card Explorer', value='NAV_CardExplorer', icon=icon('search'),
                 h2('Card Explorer'),
                 p('This page allows to search for specific cards by name, 
                   ATK/DEF or other attributes. You can choose how many cards
                   to show at the same time.'),
                 dataTableOutput("DATA_CardExplorer")
                 ),
        tabPanel('Meme Counters', value='NAV_MemeCounters', icon=icon('ankh'),
                 h2('Meme Counters'),
                 p("We can't talk about Yu-Gi-Oh! without mentioning the many
                   iconic memes that it spawned, some originating from the 4Kids
                   dub, and some from ",
                   a("LittleKuriboh's Yu-Gi-Oh! The Abridged Series",
                     href='https://www.youtube.com/playlist?list=PLTagxffHmpfT765IfQj68dMmfFs3W7s1f')),
                 p('This page of the dashboard aims to serve some fun to a
                   "cultured" viewer.'),
                 
                 h3('Pot of Greed'),
                 p('What better way to start than with the classic.'),
                 HTML(youtubeVideo('wuH84iNLJ8U'),
                 HTML(discordVideo('802081474177794091/987387966940909588/pot-of-greed2.mp4'))),
                 p("Let's examine some stats. Below you will find 3 gauge meters.
                   The first displays the number of distinct cards we are looking at
                   (in this case 1). The second counts the number of decks that contain
                   at least 1 copy of said card(s). The last tallies up the total
                   number of copies used across all decks. This convention and layout
                   will continue throughout this page."),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_PotOfGreed1'),
                     gaugeOutput('PLT_Meme_PotOfGreed2'),
                     gaugeOutput('PLT_Meme_PotOfGreed3'),
                     ),
                 
                 h3('Blue-Eyes White Dragon'),
                 HTML(discordVideo('802081474177794091/987395403311874218/blue-eyes.mp4')),
                 HTML(discordVideo('802081474177794091/987398588889985064/blue-eyes-jet.mp4')),
                 p("Here's the same stats as before, but for the one and only (actually one
                 of four) Blue-Eyes White Dragon:"),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_BlueEyesWhiteDragon1'),
                     gaugeOutput('PLT_Meme_BlueEyesWhiteDragon2'),
                     gaugeOutput('PLT_Meme_BlueEyesWhiteDragon3'),
                 ),
                 p("This is nice and all, but like with many prominent cards,
                   \"Blue-Eyes\" is an entire family of various monsters, spells
                   and traps. Let's take a look at all cards that contain
                   \"Blue-Eyes\" in either the name or description (you are welcome to
                   use the Card Explorer tab if you wish to see which cards exactly
                   these are):"),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_BlueEyes1'),
                     gaugeOutput('PLT_Meme_BlueEyes2'),
                     gaugeOutput('PLT_Meme_BlueEyes3'),
                 ),
                 
                 h3('Exodia the Forbidden One'),
                 HTML(youtubeVideo('Lq77PpXgxVU')),
                 HTML(discordVideo('802081474177794091/987449528263278672/exodia-joey.mp4')),
                 p("As you probably know, Exodia consists of 5 distinct pieces:"),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_Exodia1'),
                     gaugeOutput('PLT_Meme_Exodia2'),
                     gaugeOutput('PLT_Meme_Exodia3'),
                 ),
                 p("Similarly to Blue-Eyes, let's now look at the different varieties
                   of Exodia-like cards."),
                 HTML(discordVideo('802081474177794091/987452249494126602/exodia-necros.mp4')),
                 p('This time we will search names and descriptions
                   for either "Exodia" or "Forbidden One":'),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_MoreExodia1'),
                     gaugeOutput('PLT_Meme_MoreExodia2'),
                     gaugeOutput('PLT_Meme_MoreExodia3'),
                 ),
                 
                 h3('Dark Magician'),
                 HTML(discordVideo('802081474177794091/987459602549440583/dark-magician.mp4')),
                 HTML(discordVideo('802081474177794091/987459603140849754/dark-magician2.mp4')),
                 p('Same deal as last time. First, the Dark Magician card on its own:'),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_DarkMagician1'),
                     gaugeOutput('PLT_Meme_DarkMagician2'),
                     gaugeOutput('PLT_Meme_DarkMagician3'),
                 ),
                 p('Now, for some spin-off cards we will look at which match "Black Magician"
                   somewhere in the name or description (e.g. Black Magician Girl):'),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_MoreDarkMagician1'),
                     gaugeOutput('PLT_Meme_MoreDarkMagician2'),
                     gaugeOutput('PLT_Meme_MoreDarkMagician3'),
                 ),
                 
                 h3('The 3 Egyptian God Cards'),
                 HTML(discordVideo('802081474177794091/987467091085000795/egyptian-gods.mp4')),
                 HTML(discordVideo('802081474177794091/987472506086629458/slifer.mp4')),
                 p('While having a big relevance in the show, these cards are generally considered
                   quite weak due to difficult summoning conditions. Also, their real-life card
                   game equivalents turned out to be severely nerfed versions of the originals as
                   depicted in the show, although that was both to be expected and necessary.'),
                 div(class='gauge_row',
                     gaugeOutput('PLT_Meme_GodCards1'),
                     gaugeOutput('PLT_Meme_GodCards2'),
                     gaugeOutput('PLT_Meme_GodCards3'),
                 ),
                 
                 hr(),
                 br(),
                 p('This concludes our report. We hope it was an enjoyable read,
                   despite the unfortunate delay.'),
                 HTML(discordVideo('802081474177794091/987396354265456700/homework.mp4'))
                 ),
        )
    )
)
