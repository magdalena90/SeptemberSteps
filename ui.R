library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$script(src = 'javascript.js'),
  theme = 'styles.css',
  
  titlePanel('September Steps'),
  
  # Plot params
  sidebarLayout(
    div(class='filtros', 
        sidebarPanel(
          radioButtons('plot_type', label = h4('Select what you want to visualise:'),
                       choices = list('Daily step count' = 'daily', 'Cummulative step count' = 'cummulative'))
        )
    ),
    
    # Plot 
    mainPanel(
      div(class='winner', htmlOutput('winner')),
      div(class='plot', plotlyOutput('plot'))
    )
  )
)
