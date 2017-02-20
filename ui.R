library(shiny)
setwd('/home/alex/R/URPP')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage('Navigation',  # Application title
             
             tabPanel('Input & Simulations', titlePanel("Threshold models simulation"),
                      
                      # Sidebar with a slider input for the number of bins
                      sidebarLayout(
                        sidebarPanel(
                          
                          textInput('n', label='Network size', value='100'),
                          textInput('M', label='Number of simulations', value='100'),
                          checkboxInput("plotfirst", label='Show first simulation on the graph'),
                          
                          fluidRow(
                            column(6,
                                   radioButtons("distr", label = "Threshold distribution",
                                                choices = list("Constant" = 1, "Uniform" = 2,
                                                               "Normal" = 3),selected = 1)),
                            column(6,
                                   conditionalPanel(condition = "input.distr == 1", 
                                                    textInput('tau', label='Threshold value', value=0.2),
                                                    textInput('rioters', label='Fraction of initial rioters', value=0.1)),
                                   conditionalPanel(condition = "input.distr == 3",
                                                    textInput('mean', label='Mean of the normal, [0,1]', value=0.5),
                                                    textInput('sd', label='SD of the normal', value=0.3))
                            )),
                          
                          fluidRow(
                            column(6,
                                   radioButtons("graph", label="Graph type",
                                                choices = list('Random' = 1, 'Scalefree' = 2),selected=1)),
                            column(6,
                                   conditionalPanel(condition = 'input.graph == 1',
                                                    textInput('z', label='Average degree z', value=1.98)),
                                   conditionalPanel(condition = 'input.graph == 2',
                                                    textInput('gamma', label='Power law degree gamma', value=2.5)))),
                          fluidRow(
                            column(6, 
                                   checkboxInput("spont", label='Spontaneous adoption')),
                            column(6, 
                                   conditionalPanel(condition = 'input.spont == true',
                                                    textInput('r', label = 'Fraction of never adopting nodes, [0,1)', value=0.3),
                                                    textInput('pn', label='Spontaneous adoption rate, [0,1]', value=0.01)))
                          ),
                          actionButton('go', 'Run!')
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    conditionalPanel(condition='input.go != 0',
                                     htmlOutput('message'),
                                     plotOutput('hist'),
                                     conditionalPanel(condition='input.plotfirst == true',
                                                      sliderInput('myslider', 'Steps', min=1, max=10, value=1, animate=animationOptions()),
                                                      plotOutput("sim", height='600px'))
                  )
                  )
                      )),
              tabPanel('Networks examples',
                       actionButton('show', 'Show me networks!'),
                       conditionalPanel(condition='input.show != 0'),
                       plotOutput('network1', height='600px'),
                       plotOutput('network2', height='600px'),
                       plotOutput('network3', height='600px'),
                       plotOutput('network4', height='600px'),
                       plotOutput('network5', height='600px')
                       )
     )
))
    