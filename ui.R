library(shinydashboard)
library(plotly) # plotlyOutupt()
library(shinyWidgets)
library(shinyBS) # shiny tooltips
library(DT)


YEAR <- 2020

dashboardPage(
  title = sprintf('BRAIAN%d', YEAR),
  header = dashboardHeader(title = sprintf('BRAIAN%d', YEAR), titleWidth = '17%'),
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
        column(width=2),
        column(width=10, tags$div(title=HTML('Kliknij, by zmienić płeć   😉'), radioGroupButtons('plec', label=NULL,
                       choiceNames = c('<br>chłopcy', '<br>dziewczynki'),
                       choiceValues = list('M','K'), status = "primary",
                       selected='K', justified=TRUE, checkIcon = list(
                        yes = icon("smile-beam", "fa-3x"),
                        no = icon("meh-blank", "fa-3x")))))
    ),
        verticalTabsetPanel(contentWidth=10, color='#357ca5',
             verticalTabPanel('RANKINGI', icon=icon('stats', 'fa-3x', lib='glyphicon'),
                h3(textOutput('title')),
                box(width=12, title = tags$b(textOutput('opts1_title')), collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                    chooseSliderSkin("Round"),
                    setSliderColor(c("#357ca5","#357ca5","#357ca5","#357ca5","#357ca5","#357ca5"), 1:6),
                    sliderTextInput('rok', NULL,
                                choices = 2000:(YEAR-1), selected=YEAR-1, grid=TRUE, width='100%')
                ),
                 box(width=3, dataTableOutput('rank_table')),
                 box(width=9,
                     conditionalPanel(condition="input.opts4 % 2 == 1",
                        sliderInput('range','miejsca w rankingu:', value = c(1,6), min=1, max=300, width='100%')
                     ),
                     fluidRow(
                        column(width=10, offset=0, style='padding:0px;margin:0px',
                            tags$div(title="Do tej listy imion możesz dopisywać własne lub usuwać istniejące.",
                                selectInput('imiona', label=NULL, choices = c(), selected = c(), multiple=TRUE, width='100%'))
),
                        column(width=2, offset=0, style='padding:0px;margin:0px;',
                            splitLayout(cellWidths=c("28%","28%","44%"), style='margin:0px;padding:0px;',
                            tags$div(title='usuń wszystko', actionButton('clear', '', icon = icon('erase', lib='glyphicon'))),
                            tags$div(title='kliknij, by wybrać miejsca z rankingu', actionButton('opts4', '', icon = icon('cog', lib='glyphicon'))))
                        )
                    ),
                     plotlyOutput("Plot", height='500px'),
                    tags$br(),
                    switchInput('switch', value=FALSE, labelWidth='70px', onLabel='wyłącz warstwy', offLabel='włącz warstwy',
                        label='<i class=\"fas fa-chart-area\" style=\"font-size: 1.5em\"></i>') # materialSwitch, status='primary')
                 )
             ),
             verticalTabPanel('MAPA IMION', icon=icon('map', 'fa-3x'),
                 h3('Podobne imiona i rekomendacje'),
                 p('W tym miejscu możesz podać ulubione imię, a Asystent bardzo rzetelnie podpowie, jakie inne imiona mogą Ci się podobać.'),
                 searchInput(
                                  inputId = "search", label = '',
                                  placeholder = "znajdź imię na mapie",
                                  btnSearch = icon("search"),
                                  btnReset = icon("remove"),
                                  width = "300px"
                                ),
                 conditionalPanel(condition="!input.switch2", plotOutput("names_map", height='600px')),
                 conditionalPanel(condition="input.switch2", plotOutput("dendro", height='1000px', width='100%')),
                 fluidRow(column(width=3,
                 switchInput('switch2', labelWidth = "40px", width='100%', handleWidth='200px',
                            label='<i class=\"fa fa-eye\" style=\"font-size: 1.5em\"></i>',
                            onLabel = "<i class=\"fa fa-map\" style=\"font-size: 1.2em\"></i><br>widok mapy",
                            offLabel = '<i class=\"glyphicon glyphicon-tree-deciduous\" style=\"font-size: 1.2em\"></i><br>widok drzewa',
                            onStatus = 'primary', offStatus = 'primary',
                            value=FALSE)),column(width=4),column(width=5,
                 box(width = 12, status = "primary", solidHeader=T, collapsible=TRUE, collapsed=TRUE,
                            title = textOutput('opts2_title'),
                            conditionalPanel(condition="input.switch2", #wellPanel(width='50%',
                                    sliderInput('nclust', NULL, value=25, min=2, max=50, step=1)
                                ),
                            conditionalPanel(condition="!input.switch2", #wellPanel(width='50%',
                                 tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),

                                    sliderInput('n_sim', NULL, value=5, min=1, max=50, step=1, width='100%')
                                )
                 )))
             ),
             verticalTabPanel('czytaj więcej', icon=icon('option-horizontal', 'fa-3x', lib='glyphicon'),
                 column(width=6,
                    box(width=12, title = 'Najmodniejsze imiona w danym roku i ich historia',
                        plotOutput("oneName", height='500px'),
                        checkboxInput('cum', 'Skumulowane sumy', value=FALSE),
                        sliderTextInput('rok2', '', choices = 2000:(YEAR-1), selected=YEAR-1, grid=TRUE, width='100%')),
                    box(width=12, tags$head(tags$style(HTML("#text text-align: right"))), uiOutput('coverageInfo'))
                      ),
                 column(width=6,
                    box(width=12, title = "Zróżnicowanie imion w kolejnych latach", plotlyOutput("coveragePlot", height='1000px')),
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