library(shinydashboard)
library(plotly) # plotlyOutupt()
library(shinyWidgets)
library(shinyBS) # shiny tooltips
library(DT)


YEAR <- 2020
color1 <- '#deeaee'
color2 <- '#b1cbbb'
color3 <- '#eea29a'
color4 <- '#c94c4c'

dashboardPage('green',
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
    }
    .shiny-output-error { visibility: hidden; },
    .shiny-output-error:before { visibility: hidden; }'
))),

    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Barwny Retrospektywny Asystent Imion Aplikowanych Noworodkom </span>\');
      })
     ')),

        box(title='Witaj w aplikacji BRAIAN!', width='100%', collapsible=T, collapsed=F,
            p('Zastanawiasz się jakie wybrać imię dla chłopca czy też jakie było najmodniejsze
               imię dla dziewczynki? Albo jakie nietypowe imię wybrać dla swojego dziecka? A może ciekawi Cię ile
               dzieci otrzymuje teraz Twoje imię? Trafiłeś w odpowiednie miejsce. ', tags$br(),tags$br(),
                'Wybierz płeć dziecka poniżej. W zakładce',
                tags$b('Rankingi imion', style='color:green'), ' możesz porównać ze sobą statystyki imion z ostatnich
                20 lat. W łatwy sposób wyświetlisz najpopularnieszych 5, 10, 20 a nawet więcej imion.
                Zobaczysz ilu chłopców miało na imię Antek lub ile dziewczynek dostało imię Julia w ostatnich latach.',
                tags$br(), tags$br(), 'A może podoba Ci się jakieś imię, ale ktoś już', tags$i('je zajął'), ' albo jest zbyt
                powszechne? Wejdź w ', tags$b("Podobne imiona i rekomendacje", style='color:green'),
                ' aby znaleźć podobne imię w Twoim guście.', tags$br(), style='font-family:arial;')
    ),
    tags$div(title=HTML('Kliknij, by zmienić płeć   😉'),
        radioGroupButtons('plec', label=NULL, choiceNames = c('<br>chłopcy', '<br>dziewczynki'),
                       choiceValues = list('M','K'), status = "success",
                       selected='M', justified=TRUE, checkIcon = list(
                        yes = icon("smile-beam", "fa-3x"),
                        no = icon("meh-blank", "fa-3x")))),
        verticalTabsetPanel(contentWidth=10, color='#398439', #color='#357ca5',
             verticalTabPanel(p('Rankingi imion', style='font-family:Helvetica;'), icon=icon('stats', 'fa-3x', lib='glyphicon'),
                h3(textOutput('title'), style='font-family:Georgia;'),
                box(width=12, status='success', title = tags$b(textOutput('opts1_title')), collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                    chooseSliderSkin("Round"),
                    setSliderColor(c("#357ca5","#357ca5","#357ca5","#357ca5","#357ca5","#357ca5"), 1:6),
                    sliderTextInput('rok', NULL,
                                choices = 2000:(YEAR-1), selected=YEAR-1, grid=TRUE, width='100%')
                ),
                 box(width=3, dataTableOutput('rank_table')),
                 box(width=9,
                     conditionalPanel(condition="input.opts4 % 2 == 1",
                        sliderInput('range','Użyj suwaka, aby wybrać określone miejsca w rankingu. Najpopularniejsze imiona znajdują się na początku suwaka, najrzadsze na jego końcu.', value = c(1,6), min=1, max=300, width='100%')
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
                    switchInput('switch', value=FALSE, labelWidth='70px', onStatus=' ', offStatus=' ' ,
                            onLabel='wyłącz warstwy', offLabel='włącz warstwy',
                            label='<i class=\"fas fa-chart-area\" style=\"font-size: 1.5em\"></i>')
                 )
             ),
             verticalTabPanel(p('Podobne imiona\ni rekomendacje', style='font-family:Helvetica;'), icon=icon('project-diagram', 'fa-3x'),
                 h3('Podobne imiona i rekomendacje', style='font-family:Georgia;font-family:Georgia;'),
                 p('W tym miejscu możesz podać ulubione imię, a Asystent podpowie, jakie inne imiona mogą Ci się podobać.',  style='font-family:Arial;'),
                 uiOutput('search_imie'),
                 column(width=5, offset=3, align = 'center', style='color:white;background-color:#00a65a;border-radius:5px;',  # stary color = 4085ad
                        p(htmlOutput('names_recomm'), style='font-style:italic;font-family:Arial;')),
                 conditionalPanel(condition="!input.switch2", plotOutput("names_map", height='600px')),
                 conditionalPanel(condition="input.switch2", plotOutput("dendro", height='1000px', width='100%')),
                 fluidRow(column(width=3,
                         switchInput('switch2', handleWidth='85px',labelWidth = "20px",
                            label='<i class=\"glyphicon glyphicon-tree-deciduous\" style=\"font-size: 1.2em\"></i>',
                            onLabel = "widok mapy",
                            offLabel = 'widok drzewa',
                            onStatus = ' ', offStatus = ' '
                            )
                 ),column(width=4),column(width=5,
                     box(width = 12, status = "success", solidHeader=T, collapsible=TRUE, collapsed=TRUE,
                            title = textOutput('opts2_title'),
                            conditionalPanel(condition="input.switch2",
                                    sliderInput('nclust', NULL, value=25, min=2, max=50, step=1)
                                ),
                            conditionalPanel(condition="!input.switch2",
                                 tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                                    sliderInput('n_sim', NULL, value=5, min=1, max=50, step=1, width='100%')
                                )
                 )))
             ),
             verticalTabPanel(p('czytaj więcej', style='font-family:helvetica;'), icon=icon('option-horizontal', 'fa-3x', lib='glyphicon'),
                 column(width=6,
                    box(width=12, title = p('Najmodniejsze imiona w danym roku i ich historia', style='font-family:Georgia;'),
                        plotOutput("oneName", height='500px'),
                        checkboxInput('cum', 'Skumulowane sumy', value=FALSE),
                        sliderTextInput('rok2', '', choices = 2000:(YEAR-1), selected=YEAR-1, grid=TRUE, width='100%')),
                    box(width=12, tags$head(tags$style(HTML("#text text-align: right"))), p(uiOutput('coverageInfo'), style='font-famiy:TImes New Roman;'))
                      ),
                 column(width=6,
                    box(width=12, title = p("Zróżnicowanie imion w kolejnych latach", style='font-family:Georgia;'), plotlyOutput("coveragePlot", height='1000px')),
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