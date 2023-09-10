
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
  
  # Teams trends
  db_teams = db %>% dplyr::select(-Name) %>% group_by(Team, Date) %>% summarise(Steps=sum(value)) %>% 
             mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup
  
  # People trends
  db_people = db %>% dplyr::select(-Team) %>% group_by(Name, Date) %>% summarise(Steps=sum(value)) %>%
              mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup %>% dplyr::rename('Person'=Name)
  
  top_people = db_people %>% filter(Date == max(db_people$Date)) %>% arrange(desc(Cummulative_Steps)) %>% head(10)
  
  db_people = db_people %>% filter(Person %in% top_people$Person)
  
  # General trend
  db_mean = db %>% dplyr::select(Date, value) %>% group_by(Date) %>% summarise(Steps=mean(value), sd=sd(value))
  
  db_trend = db %>% mutate('color'='#35a4dc', 'Steps'=value, 'width'=0.5, 'alpha'=0.5, 'sd'=0) %>% 
    dplyr::select(Date, Name, Steps, color, width, alpha, sd) %>% 
    rbind(db_mean %>% mutate('Name'='Mean', 'color'='gray', 'width'=2, 'alpha'=1)) %>%
    arrange(Name) %>% mutate(Steps=as.integer(Steps))
  
  # Days of the week trend
  db_weekdays = db %>% mutate('Day'=weekdays(Date), 'Steps'=value) %>% filter(Date!='2023-09-03') %>% 
                mutate('Day'=factor(Day, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')))
  
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

  
  # STEPS PLOTS
  output$plot = renderPlotly(
    
    ggplotly(db_dict[[selected_aggregation()]] %>% 
             ggplot(aes(x=Date, y=!!sym(selected_plot_type()), color=!!sym(selected_aggregation()))) + 
             geom_line() + theme_minimal() + ylab(title_dict[[selected_plot_type()]])) %>% 
             layout(legend=list(orientation='h', y=-0.3))
    )
  
  # TREND PLOT
  output$trend_plot = renderPlotly(
    
  ggplotly(db_trend %>% ggplot(aes(x=Date, y=Steps, group=Name)) + 
               # geom_ribbon(aes(y=Steps, ymin=Steps-sd, ymax = Steps+sd), fill='gray', alpha=.25) +
               geom_line(color=db_trend$color, linewidth=db_trend$width, alpha=db_trend$alpha) + 
               theme_minimal() + ggtitle('General trend'),
          tooltip = 'Steps')
  )
  
  # WEEKDAYS PLOT
  output$weekday_plot = renderPlotly(
    
    ggplotly(db_weekdays %>% ggplot(aes(x=Day, y=Steps)) + geom_boxplot(fill='#35a4dc') + 
             theme_minimal() + theme(legend.position='none') + xlab('') + ggtitle('Weekday trends'))
    
  )
  
}



