library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = 'styles.css',
  
  titlePanel('September Steps'),
  
  # Plot params
  sidebarLayout(
    div(class='filtros', 
        sidebarPanel(
          radioButtons('unit', label = h4('Select the level of aggregation of the data:'),
                       choices = list('By team' = 'Team', 'By person (top 10)' = 'Person')),
          radioButtons('plot_type', label = h4('Select what you want to visualise:'),
                       choices = list('Daily step count' = 'Steps', 'Cummulative step count' = 'Cummulative_Steps'))
        )
    ),
    
    # Plot 
    mainPanel(
      div(class='winner', htmlOutput('winner')),
      div(class='plot', plotlyOutput('plot'))
    )
  )
)
