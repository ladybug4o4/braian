library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(reshape2)
# devtools::install_version('plotly', version='4.7.0')
library(plotly)
library(ape)

source('data_preprocessing.R')


dissNames_boys <- readRDS('dissNames_boys2019.rds')
dissNames_girls <- readRDS('dissNames_girls2019.rds')
mds_boys <- readRDS('mds_boys2019.rds')
mds_girls <- readRDS('mds_girls2019.rds')
YEAR <- 2019


function(input, output, session) {

  df_1000 <- reactive({
      DF_1000_PER_PLEC[[input$plec]][input$imiona,]
  })

  popular_names <- reactive({
      df <- DF_1000_PER_PLEC[[input$plec]]
      df <- df[order(-df[, as.character(input$rok)]), ]
      rownames(df)[input$range[1]:input$range[2]]
  })

  names_rank <- reactive({
      df <- DF_1000_PER_PLEC[[input$plec]]
      rank <- order(-df[, as.character(input$rok)])
      # idx <- which(rownames(df) %in% NAMES_300[[input$plec]])
      out <- rownames(df)[rank]
      names(out) <- paste(seq_along(rank), rownames(df)[rank], sep='. ')
      out
  })

  names_300_rank <- reactive({
      nms_rank <- names_rank()
      idx <- which(nms_rank %in% NAMES_300[[input$plec]])
      return(nms_rank[idx])
  })

  odmiana <- reactive({
    ifelse(input$plec == 'K', 'dziewczynek', 'chłopców')
  })

  observe({
      updateSelectInput(session, 'imie', label = NULL,
                      choices = names_300_rank(),
                      selected = names_rank()[49])
      updateSliderInput(session, 'range',  min = 1, max = length(NAMES_300[[input$plec]]), step=1)

  })
    observeEvent( c(input$range, input$plec, input$rok), {
      updateSelectInput(session, 'imiona', label = NULL,
                      choices = names_rank(),
                      selected = popular_names())
    })
    observeEvent(input$clear, {
                updateSelectInput(session, 'imiona', label = NULL, choices = names_rank(), selected = NULL)
    })

  output$title <- renderText({
        sprintf('Ranking popularności imion %s w roku %s', odmiana(), input$rok)
    })

  output$trendPlot <- renderPlotly({
      df <- df_1000()
      df[,'Imię'] <- rownames(df)
      validate(need(dim(df)[1] > 0, message = "podaj imię..."))
      df <- melt(df)
      colnames(df) <- c('Imię', 'Rok', 'Wartość')
      gg1 <- ggplot(data=df, aes_string(x='Rok', y='Wartość', group='Imię', color='Imię')) +
          geom_line() +
          theme_bw() +
          theme(axis.text.x = element_text(angle=90)) +
          labs(y=sprintf('Liczba nadań imienia na 1000 %s', odmiana()), x='', color='', title='')
      ggplotly(gg1, tooltip=c('x','y','colour'))
  })

  output$areaPlot <- renderPlotly({
        df <- df_1000()/10
        df[,'Imię'] <- rownames(df)
        validate(need(dim(df)[1] > 0, message = "ładuję..."))
        df <- melt(df)
        colnames(df) <- c('Imię', 'Rok', 'Wartość')
        gg2 <- ggplot(data=df, aes_string(x='Rok', y='Wartość', fill='Imię', group='Imię')) +
            geom_area(stat = 'identity') +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(y=sprintf('%% wszystkich urodzonych w %s roku %s', input$rok, odmiana()),
                title='', x='', fill='')
        ggplotly(gg2, tooltip=c('x','y','fill'))
  })

    values_per_year <- function(plec, rok, cum){
        df <- DF_1000_PER_PLEC[[plec]]
        name <- rownames(df[order(-df[,rok]),])[1]
        values <- unlist(IMIE_ROK_COUNTS_PER_PLEC[[plec]][name,])
        if(cum) values <- cumsum(values)
        return(list(name=name, values=values))
    }

  output$oneName <- renderPlot({
      k <- as.data.frame(values_per_year('K', as.character(input$rok2), input$cum))
      k$rok <- rownames(k)
      k$values <- -k$values
      m <- as.data.frame(values_per_year('M', as.character(input$rok2), input$cum))
      m$rok <- rownames(m)
      df <- rbind(k,m)

      limit <- max(abs(df$value))
      limit <- plyr::round_any(limit, as.integer(10^(nchar(limit)-1)))
      step <- limit/10
      lbls <- paste0(as.character(c(seq(limit, 0, -step), seq(0+step, limit, step))/1000), 'k')
      brks <- seq(-limit, limit, length.out = length(lbls))

      ggplot(data = df,
             aes(y=values, x=rok, fill=name)) + geom_bar(width=.6, stat='identity') +
          scale_y_continuous(breaks = brks, labels = lbls) +
          coord_flip() +
          scale_fill_manual(values=c("#DE7A22", "#F4CC70")) +
          # labs(title = sprintf('Najpopularniejsze imiona w %s', input$rok2), fill='') +
          labs(title = NULL, fill='') +
          theme_bw() +
          theme(legend.position = 'top')
  }, res=100)

    ## wyznaczam % pokrycia urodzen najpopularniejszymi imionami
    #  head(df_coverage)
    #          Płeć  Rok Liczba imion Pokrycie [%]
    # 1 Dziewczynki 2000            1     6.150944
    # 2 Dziewczynki 2001            1     6.784998
    # 3 Dziewczynki 2002            1     6.761775
    # 4 Dziewczynki 2003            1     6.983534
    # 5 Dziewczynki 2004            1     7.791084

      lapply(c('K','M'), function(plec){
          melt(sapply(1:100, function(p){
              apply(IMIE_ROK_COUNTS_PER_PLEC[[plec]], 2, function(col)
                sum(tail(sort(col), p), na.rm = TRUE))/ROK_PLEC_COUNTS[,plec] * 100
          }))
      }) -> df_coverage
      names(df_coverage) <- c('K','M')
      df_coverage <- dplyr::bind_rows(df_coverage, .id='Płeć')
      colnames(df_coverage) <- c('Płeć', 'Rok', 'Liczba imion', 'Pokrycie [%]')

  output$coveragePlot <- renderPlotly({
      ggplot(data = df_coverage[df_coverage$`Płeć` == input$plec, ], aes(x=`Liczba imion`, y=`Pokrycie [%]`, color=Rok)) +
          geom_line() +
          scale_color_gradient(high="#DE7A22",low='#F4CC70') +
          scale_x_continuous(breaks=seq(0, 100, 10)) +
          scale_y_continuous(breaks=seq(0, 100, 10)) +
          labs(title = NULL, #'Zróżnicowanie imion w kolejnych latach',
               y = sprintf('%% wszystkich %s', odmiana())) +
          theme_bw() +
          theme(legend.position='none') -> gg
  })

  output$coverageInfo <- renderText({
    rok2_chr <- as.character(input$rok2)
    n <- 30
    info1 <- df_coverage[df_coverage$Rok == as.numeric(input$rok2) & df_coverage$`Liczba imion` == n,]
    info <- split(df_coverage, df_coverage$Rok)
    lapply( split(info[[rok2_chr]], info[[rok2_chr]]$Płeć), function(df_plec){
      min_ind <- which.min(abs(df_plec$`Pokrycie [%]` - 50))
      df_plec[min_ind,]
    }) -> info2
      jakub_ile <- cumsum(c(IMIE_ROK_COUNTS_PER_PLEC[['M']]['JAKUB',]))
      jakub_ile_procent <- jakub_ile / cumsum(ROK_PLEC_COUNTS[,'M']) * 100
      julia_ile <- cumsum(c(IMIE_ROK_COUNTS_PER_PLEC[['K']]['JULIA',]))
      julia_ile_procent <- julia_ile / cumsum(ROK_PLEC_COUNTS[,'K']) * 100

    paste(
      sprintf('<b>W roku %i:</b>', input$rok2),
      sprintf('<b>1.</b> %i najpopularniejszych imion nosiło %.2f%% dziewczynek i %.2f%% chłopców,',
               n, info1$`Pokrycie [%]`[1], info1$`Pokrycie [%]`[2]),
      sprintf('<b>2.</b> połowa urodzonych dziewczynek/chłopców otrzymała jedno z %i/%i najpopularniejszych imion.',
              info2$K$`Liczba imion`, info2$M$`Liczba imion`),
      sprintf('<b>3.</b> wszystkich urodzonych po 2000 roku chłopców o imieniu Jakub jest %i i jest to %.2f%% wszystkich urodzonych w tym czasie chłopców.',
                jakub_ile[rok2_chr], jakub_ile_procent[rok2_chr]),
      sprintf('<b>4.</b> wszystkich urodzonych po 2000 roku dziewcząt o imieniu Julia jest %i i jest to %.2f%% wszystkich urodzonych w tym czasie dziewcząt.',
                julia_ile[rok2_chr], julia_ile_procent[rok2_chr]),

      '<br><b>Ponadto:</b>',
      '<b>5.</b> w 2001 roku 1 na 10 chłopców otrzymuje imię Jakub.',
      '<b>6.</b> w 2000 roku imiona Antoni i Jarosław otrzymało ok. 0.2% chłopców. 17 lat później imię Jarosław wybierane jest 11 razy rzadziej, a Antoni 28 razy częściej.',
      '<b>7.</b> 10 najpopularniejszych imion dziewczynek w 2000 roku otrzymało 45% dziewczynek. 17 lat później te same imiona dostało 15% dziewczynek.',
      '<b>8.</b> z roku na rok imiona chłopców i dziewczynek są coraz bardziej różnorodne.',

      sep = '<br>'
    ) -> txt
    txt
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
    log(data+1)
  })

    mds_matrix <- reactive({
        if(input$plec == 'M'){
          fit <- mds_boys
        }else{
          fit <- mds_girls
        }
        fit
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

    similar_names <- reactive({

        fit <- mds_matrix()
        x <- fit$points[,1]
        y <- fit$points[,2]
        nms <- rownames(fit$points)

        x0 <- x[input$imie]
        y0 <- y[input$imie]
        r2 <- (x-x0)^2 + (y-y0)^2
        similar <- names(head(sort(r2), input$n_sim+1))
        groups <- ifelse(nms %in% input$imie, 0, ifelse(nms %in% similar, 1, 2))
        df <- data.frame(names = nms, x=x, y=y, groups = as.factor(groups), r2 = r2)
    })

    output$names_map <- renderPlot({
        df <- similar_names()
        ggplot(data = df, aes(x=x,y=y,label=names)) +
            geom_point(color='#787270') +
            geom_text(data=df[df$groups==2,], size=3, color='#6a6463', hjust=0, vjust=0) +
            scale_fill_brewer(palette='Set2') +
            geom_label_repel(data=df[df$groups!=2,], aes(fill=groups)) +
            theme_void() +
            coord_fixed(ratio = 1) +
            theme(legend.position='none')

    })
    output$names_recomm <- renderText({
        df <- similar_names()
        df <- df[order(df$r2), ]
        paste(sprintf('Jeśli podoba ci się imię %s, mogą spodobać ci się również: ', input$imie),
                paste0(as.character(df$names[2:6]), collapse=', '), '.')
    })

  output$dendro <- renderPlot({
    hc <- hclust(as.dist(dissNms()), 'ward.D2')
      if(toupper(input$search) %in% hc$labels){
          idx <- which(hc$labels == toupper(input$search))
          hc$labels[idx] <- paste('~~~~  ', hc$labels[idx], '  ~~~~')
      }
    clust_k <- cutree(hc, k=input$nclust)
    par(mar = par()$mar * c(.1,1,0,1))
    plot(as.phylo(hc),
         type = "fan",
         cex=.6, tip.color=cols()[clust_k])
  }, res=100)

  session$allowReconnect(FALSE)
}