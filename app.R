library(shiny)
library(latex2exp)

ui <- fluidPage(
  
  titlePanel("The Central Limit Theorem Visualized!"),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      h4('Central limit theorem (CLT) states that the sampling distribution of the mean
        of any random variable will be approximately Normal, given that the 
        sample size is large.'),
      br(),
      p('This program gives an intuition of CLT by visualizing various 
         distributions and how their mean is distributed simulateneously.'),
      p('Choose a distribution(say Normal) from the drop down menu and change 
        the sample size gradually to observe how the distribution of the mean
        of x changes!'),
      selectInput('dist', 'Choose a distribution', 
                  c('Normal', 'Uniform', 'Poisson', 'Exponential', 'Beta', 'Weibull')),
      br(),
      sliderInput('sampleSize', 'Sample Size', 1, 100, 1),
      width = 4
    ),
    
    mainPanel(
      plotOutput('dist', height = 300),
      plotOutput('xbar', height = 300),
      br(),
      h4('It is observed that as the sample size increases, the distribution of the 
         mean of X becomes apporximately normal.'),
      br(),
      br()
    )
  )
)


server <- function(input, output) {
  
  output$dist <- renderPlot({
    n = 100000
    dists = c('Normal', 'Uniform', 'Poisson', 'Exponential', 'Beta', 'Weibull')
    x = list(rnorm(n), runif(n), rpois(n, 2), rexp(n, 2), rbeta(n, 1, 2), rweibull(n, 2))
    names(x) = dists
    hist(x[[input$dist]], breaks = 25, col = 'darkgray', border = 'white', main = paste(input$dist, 'distribution', sep = ' '), xlab = 'X')
  })
  
  output$xbar = renderPlot({
    dists = c('Normal', 'Uniform', 'Poisson', 'Exponential', 'Beta', 'Weibull')
    n = 100000
    x = list(rnorm(n), runif(n), rpois(n, 2), rexp(n, 2), rbeta(n, 1, 2), rweibull(n, 2))
    names(x) = dists
    xbars = c()
    for(i in 1:10000){
      xbars[i] = mean(sample(x[[input$dist]], input$sampleSize))
    }
    hist(xbars, col = 'darkgray',breaks = 25, border = 'white', main = TeX('Distribution of $\\bar{X}$'), xlab = TeX('$\\bar{X}'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

