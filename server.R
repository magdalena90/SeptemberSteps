
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
  gs4_auth(cache = '.secrets', email = 'magdalena.nta@gmail.com')
  
  # TRANSFORM DATASET
  db = read_sheet('1qq5_I3ciLrmO6AHv_z-64wlzzw64zNlDnE8Rxpl-Ur0') %>% select_if(~ !any(is.na(.))) %>% 
       arrange(n) %>% dplyr::select(-n) %>% melt(id.vars=c('Team','Name')) %>% 
       mutate(variable=as.Date(variable, format='%d_%m')) %>% dplyr::rename('Date'=variable) 
  
  db_teams = db %>% dplyr::select(-Name) %>% group_by(Team, Date) %>% summarise(Steps=sum(value)) %>% 
             mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup
  
  db_people = db %>% dplyr::select(-Team) %>% group_by(Name, Date) %>% summarise(Steps=sum(value)) %>%
              mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup %>% dplyr::rename('Person'=Name)
  
  top_people = db_people %>% filter(Date == max(db_people$Date)) %>% arrange(desc(Cummulative_Steps)) %>% head(10)
  
  db_people = db_people %>% filter(Person %in% top_people$Person)
  
  # INPUT
  selected_plot_type = reactive({ input$plot_type })
  selected_aggregation = reactive({ input$unit })
  
  # TEXT
  output$winner = renderUI({
    
    if(selected_aggregation()=='Team'){
      winner = db_teams %>% filter(Cummulative_Steps==max(db_teams['Cummulative_Steps'])) %>% pull(Team)
    } else {
      winner = top_people %>% filter(Cummulative_Steps==max(db_people['Cummulative_Steps'])) %>% pull(Person)
    }
    
    winner_info = paste0('<h3>', winner, ' is in the lead!</h3><br>')
    
    HTML(winner_info)
  })
  
  # PLOT
  title_dict = list('Steps' = 'Daily Steps', 'Cummulative_Steps' = 'Cummulative Steps')
  
  db_dict = list('Team' = db_teams, 'Person' = db_people)

  
  output$plot = renderPlotly(
    
    ggplotly(db_dict[[selected_aggregation()]] %>% 
             ggplot(aes(x=Date, y=!!sym(selected_plot_type()), color=!!sym(selected_aggregation()))) + 
             geom_line() + theme_minimal() + ylab(title_dict[[selected_plot_type()]])) %>% 
             layout(legend=list(orientation='h', y=-0.3))
  
    )
}
