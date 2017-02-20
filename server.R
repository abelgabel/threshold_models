library(shiny)
setwd('/home/alex/R/URPP/myapp')

shinyServer(function(input, output) {

   my.time <- eventReactive(input$go, {Sys.time()})
   n <- eventReactive(input$go, {input$n})
   M <- eventReactive(input$go, {input$M})
   plotfirst <- eventReactive(input$go, {input$plotfirst})
   distr <- eventReactive(input$go, {input$distr})
   distrParam <- eventReactive(input$go, {switch (input$distr,
                                                  '1' = list(tau = input$tau, rioters = input$rioters),
                                                  '2' = list(),
                                                  '3' = list(mean = input$mean, sd = input$sd))})
   graphType <- eventReactive(input$go, {input$graph})
   graphParam <- eventReactive(input$go, {switch (input$graph,
                                                  '1' = list(z = input$z),
                                                  '2' = list(gamma = input$gamma))})
   kars <- eventReactive(input$go, {input$spont})
   karsParam <- eventReactive(input$go, {switch (input$spont,
                                                 'true' = list(r = input$r, pn = input$pn),
                                                 'false = list()')})
   
   out <- eventReactive(input$go, {
     set.seed(as.numeric(my.time()))
     my.n <- as.numeric(n()); my.M <- M();
     if (graphType() == 1) my.graph = 'random' else my.graph = 'scalefree'
     my.graphParam <- lapply(graphParam(), as.numeric)
     if (distr() == '1') my.distr = 'const' 
     else if (distr() == '2') my.distr = 'unif' 
     else if (distr() == '3') my.distr = 'normal'
     my.distrParam <- lapply(distrParam(), as.numeric)
     if (kars()) my.kars = T else my.kars =F
     my.karsParam <- lapply(karsParam(), as.numeric)
     my.out <- my.sim(my.n, my.graph, my.graphParam, my.M, my.distr, my.distrParam, my.kars, my.karsParam)
     return(my.out)
   })
   
   sample.time <- eventReactive(input$show, {Sys.time()})
   my.sample <- eventReactive(input$show, {
     set.seed(sample.time())
     return(sample(1:M(), size=5))
   })
   
    output$message <- renderUI({
      HTML('Running simulations...<br/>')
      HTML(paste('n=', n(), '<br/>'))
      #my.out <- out()
      #HTML(paste(my.out$cascades, sep=' '))
      })
    
     library(igraph)
     source('my.sim.app.R')
     
    myplots <- function(n,graph, states, thresholds){
        set.seed(as.numeric(my.time()))
        colors <- rep('white', n)
        colors[states==1] <- 'goldenrod1'
        plot(graph, layout=layout.davidson.harel,
             vertex.size=15*log(thresholds+1) + 5, vertex.label=NA, vertex.color=colors, edge.arrow.size=0.0)
        }
    
    output$sim <- renderPlot({
      my.n <- as.numeric(n())
      my.out <- out()
      i <- input$myslider
      st <- nrow(my.out$states)
      thresholds <- my.out$thresholds[1,]
      myplots(my.n, my.out$graph, my.out$states[min(i, st), ], thresholds)
  })
    
    output$hist <- renderPlot({
      my.n <- as.numeric(n())
      my.out <- out()
      hist(my.out$cascades, breaks=2*ceiling(log(my.n)), freq=F, 
          main='Histogram of cascade sizes',
          xlab='Cascade size', col='darkblue')
    })
    
    output$network1 <- renderPlot({
      i <- my.sample()[1]
      my.out <- out()
      my.graph <- my.out$graphs[[i]]
      initial <- my.out$initial[i,]
      final <- my.out$final[i,]
      thresholds <- round(my.out$thresholds[i,], 2)
      size <- my.out$cascades[i]
      par(mfrow=c(1,2))
      
      set.seed(as.numeric(sample.time()))
      colors.init <- colors.final <- rep('white', n())
      colors.init[initial==1] <- 'goldenrod1'
       colors.final[final==1] <- 'goldenrod1'
       plot(my.graph, layout=layout.davidson.harel,
            vertex.size=15*log(thresholds+1) + 5, main='Initial states', vertex.color=colors.init, vertex.label=NA, edge.arrow.size=0.0)
       set.seed(as.numeric(sample.time()))
       plot(my.graph, layout=layout.davidson.harel,
            vertex.size=15*log(thresholds+1) + 5, main='Final states', vertex.label=NA, vertex.color=colors.final, edge.arrow.size=0.0)
       legend('topright', legend=paste('Cascade size', size, sep=' '), lty=c(), fill='white', border='white')
       
    })
    
    output$network2 <- renderPlot({
      i <- my.sample()[2]
      my.out <- out()
      my.graph <- my.out$graphs[[i]]
      initial <- my.out$initial[i,]
      final <- my.out$final[i,]
      thresholds <- round(my.out$thresholds[i,], 2)
      size <- my.out$cascades[i]
      par(mfrow=c(1,2))
      
      set.seed(as.numeric(sample.time()))
      colors.init <- colors.final <- rep('white', n())
      colors.init[initial==1] <- 'goldenrod1'
      colors.final[final==1] <- 'goldenrod1'
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Initial states', vertex.color=colors.init, vertex.label=NA, edge.arrow.size=0.0)
      set.seed(as.numeric(sample.time()))
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Final states', vertex.label=NA, vertex.color=colors.final, edge.arrow.size=0.0)
      legend('topright', legend=paste('Cascade size', size, sep=' '), lty=c(), fill='white', border='white')
      
    })
    
    output$network3 <- renderPlot({
      i <- my.sample()[3]
      my.out <- out()
      my.graph <- my.out$graphs[[i]]
      initial <- my.out$initial[i,]
      final <- my.out$final[i,]
      thresholds <- round(my.out$thresholds[i,], 2)
      size <- my.out$cascades[i]
      par(mfrow=c(1,2))
      
      set.seed(as.numeric(sample.time()))
      colors.init <- colors.final <- rep('white', n())
      colors.init[initial==1] <- 'goldenrod1'
      colors.final[final==1] <- 'goldenrod1'
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Initial states', vertex.color=colors.init, vertex.label=NA, edge.arrow.size=0.0)
      set.seed(as.numeric(sample.time()))
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Final states', vertex.label=NA, vertex.color=colors.final, edge.arrow.size=0.0)
      legend('topright', legend=paste('Cascade size', size, sep=' '), lty=c(), fill='white', border='white')
      
    })
    
    output$network4 <- renderPlot({
      i <- my.sample()[4]
      my.out <- out()
      my.graph <- my.out$graphs[[i]]
      initial <- my.out$initial[i,]
      final <- my.out$final[i,]
      thresholds <- round(my.out$thresholds[i,], 2)
      size <- my.out$cascades[i]
      par(mfrow=c(1,2))
      
      set.seed(as.numeric(sample.time()))
      colors.init <- colors.final <- rep('white', n())
      colors.init[initial==1] <- 'goldenrod1'
      colors.final[final==1] <- 'goldenrod1'
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Initial states', vertex.color=colors.init, vertex.label=NA, edge.arrow.size=0.0)
      set.seed(as.numeric(sample.time()))
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Final states', vertex.label=NA, vertex.color=colors.final, edge.arrow.size=0.0)
      legend('topright', legend=paste('Cascade size', size, sep=' '), lty=c(), fill='white', border='white')
      
    })
    
    output$network5 <- renderPlot({
      i <- my.sample()[5]
      my.out <- out()
      my.graph <- my.out$graphs[[i]]
      initial <- my.out$initial[i,]
      final <- my.out$final[i,]
      thresholds <- round(my.out$thresholds[i,], 2)
      size <- my.out$cascades[i]
      par(mfrow=c(1,2))
      
      set.seed(as.numeric(sample.time()))
      colors.init <- colors.final <- rep('white', n())
      colors.init[initial==1] <- 'goldenrod1'
      colors.final[final==1] <- 'goldenrod1'
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Initial states', vertex.color=colors.init, vertex.label=NA, edge.arrow.size=0.0)
      set.seed(as.numeric(sample.time()))
      plot(my.graph, layout=layout.davidson.harel,
           vertex.size=15*log(thresholds+1) + 5, main='Final states', vertex.label=NA, vertex.color=colors.final, edge.arrow.size=0.0)
      legend('topright', legend=paste('Cascade size', size, sep=' '), lty=c(), fill='white', border='white')
      
    })
    
})