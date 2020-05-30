library(shinydashboard)
library(plotly) # plotlyOutupt()
library(shinyWidgets)
library(shinyBS) # shiny tooltips

YEAR <- 2020

dashboardPage(
  title = sprintf('BRAIAN%d', YEAR),
  header = dashboardHeader(title = sprintf('BRAIAN%d', YEAR), titleWidth = 200),
  sidebar = dashboardSidebar(disable=TRUE),
  body = dashboardBody(
    tags$head(tags$style(HTML('
        .myClass {
            font-size: 13px;
            line-height: 50px;
            text-align: left;
            font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
            font-style: italic;
            padding: 20px;
            overflow: hidden;
            color: white;
    }'
))),

    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Bardzo Rzetelny Asystent Imion Aplikowanych Noworodkom </span>\');
      })
     ')),

    fluidRow(
        radioGroupButtons('plec', label=NULL,
                       choiceNames = c('<br>chłopcy', '<br>dziewczynki'),
                       choiceValues = list('M','K'), status = "primary",
                       selected='K', justified=TRUE, checkIcon = list(
                        yes = icon("smile-beam", "fa-3x"),
                        no = icon("meh-blank", "fa-3x"))),
        verticalTabsetPanel(contentWidth=10, color='#357ca5',
             verticalTabPanel('RANKINGI', icon=icon('stats', 'fa-3x', lib='glyphicon'),
                    h3(textOutput('title')),
                    fluidRow(
                        column(width=10, offset=0,
                            selectInput('imiona', label=NULL, choices = c(), selected = c(), multiple=TRUE, width='100%')),
                        column(width=1, offset=0, splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "padding:0px"),
                                actionButton('clear', '', icon = icon('erase', lib='glyphicon')),
                                actionButton('opts', '', icon = icon('cog', lib='glyphicon'))
                        ))
                    ),
                    conditionalPanel(condition="input.opts % 2 == 1", wellPanel(
                        chooseSliderSkin("Round"),
                        setSliderColor(c("#357ca5","#357ca5","#357ca5","#357ca5","#357ca5","#357ca5"), 1:6),
                        sliderTextInput('rok', 'Ranking z roku:', choices = 2000:(YEAR-1), selected=YEAR-1, grid=TRUE, width='100%'),
                        sliderInput('range','miejsca w rankingu:', value = c(1,6), min=1, max=300, width='100%')
                    )),
                    conditionalPanel(condition="!input.switch", plotlyOutput("trendPlot", height='500px')),
                    conditionalPanel(condition="input.switch", plotlyOutput("areaPlot", height='500px')),
                    tags$br(),
                    materialSwitch('switch', label='warstwy', value=FALSE, status='primary')
             ),
             verticalTabPanel('MAPA IMION', icon=icon('map', 'fa-3x'),
                 tabsetPanel(
                    tabPanel('<',
                        h3(''),
                        splitLayout(cellWidths=c('93%','7%'),
                            selectInput('imie', label=NULL, selected=c(), choices=c(), multiple=FALSE),
                                                        actionButton('opts2', '', icon = icon('cog', lib='glyphicon'))
                        ),
                          tags$head(tags$style(HTML(".shiny-split-layout > div {
                                overflow: visible;
                        }"))),
                        conditionalPanel(condition="input.opts2 % 2 == 1", wellPanel(
                            sliderInput('n_sim', 'Liczba podobnych imion na mapie:', value=5, min=1, max=50, step=1, width='100%'),
                        )),
                        column(width=8, offset=2, style = "background-color:#9abfd4;border-radius: 10px;", align='center',
                                p(textOutput('names_recomm'), style="color: #000000; text-align:justify;")),
                        plotOutput("names_map", height='600px')
                    ),
                     tabPanel('>',
                         h3('Grupy imion podobnych'),
                         fluidRow(
                            column(width=10, offset=0,
                            searchInput(
                              inputId = "search", label = "",
                              placeholder = "znajdź imię na wykresie",
                              btnSearch = icon("search"),
                              btnReset = icon("remove"),
                              width = "300px"
                            )),
                        column(width=1, offset=0, splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "padding:0px"),
                                actionButton('opts3', '', icon = icon('cog', lib='glyphicon'))
                        ))),
                        conditionalPanel(condition="input.opts3 % 2 == 1", wellPanel(
                            sliderInput('nclust', 'Siła podobieństwa (luźno podobne <-> bardzo podobne)', value=25, min=2, max=50)
                        )),
                        plotOutput("dendro", height='1000px', width='100%'),

                     )
                 )
             ),
             verticalTabPanel('czytaj więcej', icon=icon('option-horizontal', 'fa-3x', lib='glyphicon'),
                      box(width=6, title = 'Najmodniejsze imiona i ich historia',
                        plotOutput("oneName", height='500px'),
                        checkboxInput('cum', 'Skumulowane sumy', value=FALSE),
                        sliderTextInput('rok2', '', choices = 2000:(YEAR-1), selected=YEAR-1, grid=TRUE, width='100%')),
                      box(width=6, title = "Zróżnicowanie imion w kolejnych latach", plotlyOutput("coveragePlot", height = '500px')),
                      box(width=12, tags$head(tags$style(HTML("#text text-align: right"))), uiOutput('coverageInfo'))

             )
      )
    ),
           tags$footer(h5(
           "Copyright © 2018-2020 ",
           tags$a(href='https://github.com/katsob/braian', 'Katarzyna Sobiczewska'), tags$br(),
           "Dane pochodzą ze zbiorów ",
           tags$a(href='https://dane.gov.pl/dataset/219', 'https://dane.gov.pl/'),
           align = "center"))
  )
)