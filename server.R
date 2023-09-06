
server <- function(input, output, clientData, session) {
  
  # LOAD LIBRARIES
  library(googlesheets4)
  library(dplyr)
  library(reshape2)
  library(plotly)
  library(shiny)
  
  # LOAD DATA FROM DRIVE

  # Run this the first time
  # options(gargle_oauth_cache = '.secrets')
  # gs4_auth()
  # gs4_deauth()
  
  # Only works after you've created the .secrets file with the code above
  gs4_auth(cache = ".secrets", email = "magdalena.nta@gmail.com")
  
  
  db = read_sheet('1qq5_I3ciLrmO6AHv_z-64wlzzw64zNlDnE8Rxpl-Ur0') %>% select_if(~ !any(is.na(.))) %>% 
    arrange(n) %>% dplyr::select(-n) %>% melt(id.vars=c('Team','Name')) %>% 
    mutate(variable=as.Date(variable, format='%d_%m')) %>% dplyr::rename('Date'=variable) 
  
  db_agg = db %>% dplyr::select(-Name) %>% group_by(Team, Date) %>% summarise(Steps=sum(value)) %>% 
           mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup
  
  # TEXT
  output$winner = renderUI({
    
    winner_team = db_agg %>% filter(Cummulative_Steps==max(db_agg['Cummulative_Steps'])) %>% pull(Team)
    winner_info = paste0('<h3>', winner_team, ' is in the lead!</h3><br>')
    
    HTML(winner_info)
  })
  
  # PLOT
  selected_plot_type = reactive({input$plot_type})
  
  output$plot = renderPlotly(
    if(selected_plot_type()=='daily'){
      ggplotly(db_agg %>% ggplot(aes(x=Date, y=Steps, color=Team)) + geom_line() + theme_minimal() +
               ggtitle('Daily Step Count')) %>% layout(legend = list(orientation = 'h', y = -0.3))
    } else {
      ggplotly(db_agg %>% ggplot(aes(x=Date, y=Cummulative_Steps, color=Team)) + geom_line() + 
               ylab('Cummulative Steps') + theme_minimal() + ggtitle('Cummulative Step Count')) %>% 
               layout(legend = list(orientation = 'h', y = -0.3))
    }
  )
}
