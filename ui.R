library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = 'styles.css',
  
  titlePanel('Steptember Challenge'),
  
  # Plot params
  tabsetPanel(type = 'tabs',
              tabPanel('Step Counts',
                       
                       # Options
                       sidebarLayout(
                         div(class='filtros', 
                             sidebarPanel(
                               radioButtons('unit', label = h4('Select the level of aggregation of the data:'),
                                            choices = list('By team' = 'Team', 'By person (top 12)' = 'Person')),
                               radioButtons('plot_type', label = h4('Select what you want to visualise:'),
                                            choices = list('Daily step count' = 'Steps', 'Cummulative step count' = 'Cummulative_Steps')),
                               actionButton('refresh_data', 'Refresh data')
                             )
                         ),
                         
                         # Plot 
                         mainPanel(
                           div(class='winner', htmlOutput('winner')),
                           div(class='plot', plotlyOutput('plot'))
                         )
                       )),
              # tabPanel('General Trends', 
              #          div(plotlyOutput('trend_plot')),
              #          div(plotlyOutput('weekday_plot'))
              # )
              tabPanel('Comparison with last year',
                       div(plotlyOutput('then_vs_now_plot'), class="col-sm-12 col-md-6 col-lg-4")
                       )
  )
)
