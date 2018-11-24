library(shinydashboard)
library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(ape)
library(TSclust)
## https://danepubliczne.gov.pl/dataset/najpopularniejsze-imiona-w-polsce

DATA <- read.csv('Najpopularniejsze-imiona-w-Polsce-w-latach-2000-2017.csv', sep=';', fileEncoding = "UTF-8")
dissNames_boys <- readRDS('dissNames_boys.rds')
dissNames_girls <- readRDS('dissNames_girls.rds')

## wyliczam % pokrycia urodzen najpopularniejszymi imionami
k <- split(DATA, DATA$Płeć)
k <- lapply(k, function(df)  dcast(Imię ~ Rok, data=df, value.var = 'Liczba'))
all_ <- sapply(k, function(i) colSums(i[,-1], na.rm=TRUE))
lapply(c('K','M'), function(plec){
  melt(sapply(1:100, function(p){
    apply(k[[plec]][,-1], 2, function(col) sum(tail(sort(col), p), na.rm = TRUE))/all_[,plec] * 100
  }))
}) -> k2
names(k2) <- c('Dziewczynki','Chłopcy')
k2 <- dplyr::bind_rows(k2, .id='Płeć')
colnames(k2) <- c('Płeć', 'Rok', 'Liczba imion', 'Pokrycie [%]')
df_coverage <- k2


ui <- dashboardPage(
  title = 'BRAIAN', 
  header = dashboardHeader(title = 'BRAIAN', titleWidth = 200),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    tags$head(
      tags$link(rel='stylesheet', type='text/css', href='custom.css')),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Barwny Retrospektywny Analizator Imion Aplikowanych Noworodkom </span>\');
      })
     ')),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 4, height=1600,
          sliderInput('rok','Rok', value = 2017, min=2000, max=2017, sep='', step=1),
          plotlyOutput("oneName"),
          checkboxInput('cum', 'Skumulowane sumy', value=FALSE),
          uiOutput('coverageInfo'),
          plotlyOutput("coveragePlot", height = '800px'),
          br(),br(),
          h3('Losuj imię:'),
          fluidRow(
            column(7, verbatimTextOutput("random")),
            column(2, actionButton("do", "Jeszcze raz")))
          ),
      box(width=8, height = 1600,
          sliderInput('range','Ranking popularności imion ... w roku ..., miejsca:',
                      value = c(1,15), min=1, max=250),
          helpText("(chwytając suwak na środku możesz przesunąć cały zakres)"),
          selectInput('imiona', label=NULL,
                      choices = c(), 
                      selected = c(),
                      multiple=TRUE),
          helpText("(do tej listy możesz dopisać inne imiona lub kasować istniejące)"),
          radioButtons('plec', label=NULL,
                       choiceNames = c('chłopcy', 'dziewczynki'),
                       choiceValues = list('M','K'),
                       selected='K', inline = TRUE),
      tabBox(width=15,
             tabPanel('Popularność imion', icon=icon('area-chart'),
                      plotlyOutput("trendPlot", height='900px')
             ),
             tabPanel('Grupy imion podobnych w czasie', icon=icon('project-diagram'),
                      helpText("Wykres ilustruje imiona o podobnych trendach. Możesz samodzielnie ustalić 
                                liczbę grup, a także zwiększyć zakres imion suwakiem na górze i dopisać imiona
                                samodzielnie."),
                      plotOutput("dendro", height='750px', width='100%'),
                      plotOutput("clust", height='300px'),
                      sliderInput('nclust', 'Liczba klastrów:', value=6, min=2, max=14))
      )),
      column(11,
        h5('Opracowanie,', tags$a(href='https://github.com/katsob/braian', 'scenariusz'), ', reżyseria: Katarzyna Sobiczewska, 2018', align='right'),
        h5('Dane pochodzą z', tags$a(href = 'https://dane.gov.pl/dataset/219', 'niniejszej strony'), '.', align='right')
      ),
      column(1,
             h5(),
             img(src='polska.png', align = "right", height='40px')
      )
      
    )
  )
)

server <- function(input, output, session) {

  sex_filter <- function(plec) DATA[(DATA[,'Płeć'] %in% strsplit(plec,'')[[1]]), c('Imię', 'Rok', 'Liczba')]
  preprocessing <- function(df, rok){
    df <- dcast(Imię ~ Rok, data=df, value.var = 'Liczba')
    df[is.na(df)] <- 0
    df <- df[order(-df[,as.character(rok)]),]
  }
  
  df_1000 <- reactive({
    d1 <- sex_filter(input$plec)
    d1['Liczba'] <- ave(d1$Liczba, d1$Rok, FUN=function(x) x/sum(x)*1000)
    preprocessing(d1, input$rok)
  })
  
  df_fraq <- reactive({
    d1 <- sex_filter(input$plec)
    d1['Liczba'] <- ave(d1$Liczba, d1$Rok, FUN=function(x) x/sum(x)*100)
    preprocessing(d1, input$rok)
  })
  
  names <- reactive({
      as.character(df_1000()[input$range[1]:input$range[2], 'Imię'])
  })
  
  odmiana <- reactive({
    ifelse(input$plec == 'K', 'dziewczynek', 'chłopców')
  })

  observe({
    updateSelectInput(session, 'imiona', label = NULL,
                      choices = df_1000()[,'Imię'],
                      selected = names())
  })
  observe({
    updateSliderInput(session, 'nclust',  value=6, min = 2, max = min(12, length(input$imiona)-1), step=1)
  })
  observe({
    updateSliderInput(session, 'range',
                      label = sprintf('Ranking popularności imion %s w roku %i, miejsca:', odmiana(), input$rok),
                      value = c(input$range[1],input$range[2]), min=1, max=250 )
  })
  
  output$coveragePlot <- renderPlotly({
    ggplot(data = df_coverage, aes(x=`Liczba imion`, y=`Pokrycie [%]`, color=Rok)) +
      geom_line() + facet_grid(Płeć~.) + 
      scale_color_gradient(high='#6A3D9A', low='#CAB2D6') +
      geom_line(data=df_coverage[df_coverage$Rok == input$rok,], color='red') +
      labs(title = 'Zróżnicowanie imion w kolejnych latach',
           y = '% pokrycia wszystkich dzieci') +
      theme(legend.position='none') -> gg
    gg <- ggplotly(gg)
    gg$x$layout$margin$l <- 0
    gg
  })

  output$coverageInfo <- renderText({
    n <- 30
    info1 <- df_coverage[df_coverage$Rok == input$rok & df_coverage$`Liczba imion` == n,]
    info <- split(df_coverage, df_coverage$Rok)
    lapply( split(info[[as.character(input$rok)]], info[[as.character(input$rok)]]$Płeć), function(df_plec){
      min_ind <- which.min(abs(df_plec$`Pokrycie [%]` - 50))
      df_plec[min_ind,]
    }) -> info2
    paste(
      sprintf('<b>W roku %s:</b>', input$rok),
      sprintf('<b>1.</b> %i najpopularniejszych imion nosiło %.2f%% dziewczynek i %.2f%% chłopców,',
               n, info1$`Pokrycie [%]`[1], info1$`Pokrycie [%]`[2]),
      sprintf('<b>2.</b> połowa urodzonych dziewczynek/chłopców otrzymała jedno z %i/%i najpopularniejszych imion.',
              info2$Dziewczynki$`Liczba imion`, info2$Chłopcy$`Liczba imion`),
      sep = '<br>'
    ) -> txt
    txt
  })
  
  output$trendPlot <- renderPlotly({
    k <- df_1000()
    k <- k[k$`Imię` %in% input$imiona,]
    
    validate(
      need(dim(k)[1] > 0, message = "ładuję...")
    )
    
    gg1 <- ggplot(data=melt(k), aes(x=variable, y=value, group=`Imię`, color=`Imię`)) + geom_line() +
      theme(axis.text.x = element_text(angle=45)) +
      labs(y=sprintf('Liczba nadań imienia na 1000 %s', odmiana()), x='', color='')
    gg1 <- ggplotly(gg1)
    
    gg1$x$layout$xaxis$showticklabels <- FALSE
    gg1$x$layout$yaxis$label
    gg1$x$layout$xaxis$showticks <- FALSE
    
    k <- df_fraq()
    k <- k[k$`Imię` %in% input$imiona,]
    gg2 <- ggplot(data=melt(k), aes(x=variable, y=value, fill=`Imię`, group=`Imię`)) +
      geom_area(stat = 'identity') +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(y=sprintf('%% wszystkich urodzonych w %s roku %s', input$rok, odmiana()), title='Popularność imion i pokrycie populacji',
           x='', fill='')
    gg2 <- ggplotly(gg2)
    
    for(i in seq_along(gg1$x$data)){
      gg1$x$data[[i]][['showlegend']] <- FALSE
    }
    
    subplot(gg1, gg2, nrows = 2, titleY = TRUE)
  })

  output$oneName <- renderPlotly({
    d <- DATA[DATA$Rok == input$rok,]
    name <- sapply(split(d, d$Płeć), function(df) df[,'Imię'][which.max(df[,'Liczba'])])
    d <-  DATA[DATA$`Imię` %in% name, ]
    d[d$Imię == name[1], 'Liczba'] <- -d[d$Imię == name[1], 'Liczba']
    if(input$cum){
      d <- d %>% group_by(Imię) %>% arrange(Rok) %>% mutate(Liczba = cumsum(Liczba))
    }
    limit <- max(abs(d$Liczba))
    limit <- plyr::round_any(limit, as.integer(10^(nchar(limit)-1)))
    step <- limit/5
    lbls <- paste0(as.character(c(seq(limit, 0, -step), seq(0+step, limit, step))/1000), 'k')
    brks <- seq(-limit, limit, length.out = length(lbls))

    ggplot(data = d,
           aes(y=Liczba, x=Rok, fill=`Imię`)) + geom_bar(width=.6, stat='identity') +
      scale_y_continuous(breaks = brks, labels = lbls) +
      coord_flip() +
      scale_fill_manual(values=c("#CAB2D6", "#6A3D9A")) +
      labs(title = sprintf('Najpopularniejsze imiona w %i', input$rok), fill='') +
      theme(legend.position = 'bottom') -> gg
    ggplotly(gg)
  })

  output$random <- renderPrint({
    input$do
    caly_zakres <- TRUE
    d <- DATA[(DATA$Płeć == input$plec),]
    as.character(sample(d$Imię, 1))
  })
  
  dissNms <- reactive({
    if(input$plec == 'M'){
      data <- dissNames_boys
    }else{
      data <- dissNames_girls
    }
    validate(
      need(all(input$imiona %in% rownames(data)), 'ładuję...')
    )
    data[input$imiona,input$imiona]
  })
  
  cols <- function(brewer_palette='Dark2'){
    if(input$nclust > 8){
      c <- RColorBrewer::brewer.pal(8, brewer_palette)
      c <- rep(c, length.out = input$nclust)
    }else{
      c <- RColorBrewer::brewer.pal(input$nclust, brewer_palette)
    }
    return(c)
  }
  
  output$clust <- renderPlot({
    # # precomputed
    # aa <- as.matrix( df_1000()[,-1])
    # rownames(aa) <-  df_1000()[,1]
    # dissNames <- TSclust::diss(aa, 'DTWARP')
    # dissNames <- as.matrix(dissNames)
    # saveRDS(dissNames, file='dissNames_boys.rds')
    # a <- readRDS('dissNames_boys.rds')
    # dim(a)

    hc <- hclust(as.dist(dissNms()), 'ward.D2')
    clust_k <- cutree(hc, k=input$nclust)
    clust <- data.frame('Imię' = input$imiona, 'klaster'= c(clust_k))
    ddd <- merge(melt(df_1000()), clust)
    ggplot(data=ddd, aes(x = variable, y=value, group=Imię, color=as.factor(klaster))) +
      geom_line() + scale_color_manual(values = cols()) +
      facet_wrap(~klaster, nrow = 2, scale='free_y') + 
      theme(legend.position='none', 
            axis.text.x = element_text(angle=45, hjust = 1)) +
      scale_x_discrete(breaks=c(2000, 2004, 2008, 2012, 2016)) +
      labs(x='Rok', y='Nadań na 1000 urodzeń')
  }, res=100)
  
  output$dendro <- renderPlot({
    validate(
      need(length(input$imiona) > 2, 'Wybierz co najmniej 3 imiona.')
    )
    hc <- hclust(as.dist(dissNms()), 'ward.D2')
    clust_k <- cutree(hc, k=input$nclust)
    par(mar = par()$mar * c(.1,1,0,1))
    plot(as.phylo(hc),
         type = "fan", 
         cex=.75, tip.color=cols()[clust_k])
    legend("bottomright", col = cols(), legend = 1:input$nclust, pch=16, horiz = T, title='Numer klastra', cex=.8)
  }, res=100)
  
}

shinyApp(ui, server)
# rsconnect::deployApp('katsob/')