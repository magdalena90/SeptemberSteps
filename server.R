
server <- function(input, output, clientData, session) {
  
  # LOAD LIBRARIES
  library(googlesheets4)
  library(dplyr)
  library(reshape2)
  library(plotly)
  library(shiny)
  
  # FUNCTIONS
  
  # Get colors from the ggplot palette
  gg_colour_hue = function(n) {
    hues = seq(15, 375, length = n+1)
    pal = hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  # Assign linestype and colour to teams/top people
  assign_lineplot_styles = function(df, id_col){
    
    n = df[[id_col]] %>% unique %>% length
    ltypes = c(rep('solid',min(n,10)), rep('twodash',max(0,n-10)))
    cols = c(gg_colour_hue(min(n,10)), gg_colour_hue(10))[1:n]
    
    styles_df = data.frame(id_col=df[[id_col]] %>% unique, color=cols, linetype=ltypes)
    colnames(styles_df)[1] = id_col
    
    df_with_styles = left_join(df, styles_df, by=id_col)
    
  }
  
  # LOAD DATA FROM DRIVE

  # Run this the first time
  # options(gargle_oauth_cache = '.secrets')
  # gs4_auth()
  # gs4_deauth()
  
  # Only works after you've created the .secrets file with the code above
  gs4_auth(cache = '.secrets', email = 'magdalena.nta@gmail.com')
  
  # TRANSFORM DATASET
  df_raw = read_sheet('1qq5_I3ciLrmO6AHv_z-64wlzzw64zNlDnE8Rxpl-Ur0')
  df = df_raw %>% select_if(~ !any(is.na(.))) %>% 
       arrange(n) %>% dplyr::select(-n) %>% melt(id.vars=c('Team','Name')) %>% 
       mutate(variable=as.Date(variable, format='%d_%m')) %>% dplyr::rename('Date'=variable) 
  
  # MEAN STEPS
  # This year
  mean_now = data.frame('Name'=df_raw$Name, 'mean_now'=round(rowMeans(df_raw[5:ncol(df_raw)], na.rm=TRUE)))
  
  # Last year
  last_year = as.character(as.numeric(format(Sys.Date(), "%Y"))-1)
  df_last = read_sheet('1qq5_I3ciLrmO6AHv_z-64wlzzw64zNlDnE8Rxpl-Ur0', sheet=last_year)
  mean_last = data.frame('Name'=df_last$Name, 'mean_then'=round(rowMeans(df_last[5:ncol(df_last)])))
  
  mean_then_vs_now = inner_join(mean_last, mean_now, by='Name') %>% mutate('difference'=mean_now-mean_then) %>%
    filter(difference>0) %>% arrange(-difference)
  
  # Teams trends
  df_teams = df %>% dplyr::select(-Name) %>% group_by(Team, Date) %>% summarise(Steps=sum(value)) %>% 
             mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup %>% filter(Team!='pending')
  
  df_teams = assign_lineplot_styles(df_teams, 'Team') 
  
  # People trends
  top_n = 15
  team_size = 5
  
  df_people = df %>% dplyr::select(-Team) %>% group_by(Name, Date) %>% summarise(Steps=sum(value)) %>%
              mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup %>% dplyr::rename('Person'=Name)
  
  top_people = df_people %>% filter(Date == max(df_people$Date)) %>% arrange(desc(Cummulative_Steps)) %>% head(top_n)
  
  df_people = df_people %>% filter(Person %in% top_people$Person) %>% 
              group_by(Person) %>% mutate(total_steps=max(Cummulative_Steps)) %>% ungroup %>% 
              arrange(desc(total_steps))
  
  df_people = assign_lineplot_styles(df_people, 'Person') %>% arrange(Person)
  
  # General trend
  df_mean = df %>% dplyr::select(Date, value) %>% group_by(Date) %>% summarise(Steps=mean(value), sd=sd(value))
  
  df_trend = df %>% mutate('color'='#35a4dc', 'Steps'=value, 'width'=0.5, 'alpha'=0.5, 'sd'=0) %>% 
    dplyr::select(Date, Name, Steps, color, width, alpha, sd) %>% 
    rbind(df_mean %>% mutate('Name'='Mean', 'color'='gray', 'width'=2, 'alpha'=1)) %>%
    arrange(Name) %>% mutate(Steps=as.integer(Steps))
  
  # Days of the week trend
  df_weekdays = df %>% mutate('Day'=weekdays(Date), 'Steps'=value) %>% filter(Date!='2023-09-03') %>% 
                mutate('Day'=factor(Day, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')))
  
  # INPUT
  selected_plot_type = reactive({ input$plot_type })
  selected_aggregation = reactive({ input$unit })
  
  # TEXT
  output$winner = renderUI({
    
    if(selected_aggregation()=='Team'){
      winner = df_teams %>% filter(Cummulative_Steps==max(df_teams['Cummulative_Steps'])) %>% pull(Team)
    } else {
      winner = top_people %>% filter(Cummulative_Steps==max(df_people['Cummulative_Steps'])) %>% pull(Person)
    }
    
    winner_info = paste0('<h3>', winner, ' is in the lead!</h3><br>')
    
    HTML(winner_info)
  })
  
  # PLOT
  title_dict = list('Steps' = 'Daily Steps', 'Cummulative_Steps' = 'Cummulative Steps')
  
  df_dict = list('Team' = df_teams, 'Person' = df_people)

  
  # STEPS PLOTS
  output$plot = renderPlotly(
    
    ggplotly(df_dict[[selected_aggregation()]] %>% 
             ggplot(aes(x=Date, y=!!sym(selected_plot_type()), color=!!sym(selected_aggregation()))) + 
             geom_line(linetype=df_dict[[selected_aggregation()]]$linetype) + 
             scale_color_manual(values=unique(df_dict[[selected_aggregation()]][c('linetype','color')])[['color']]) +
             theme_minimal() + ylab(title_dict[[selected_plot_type()]])) %>% 
             layout(legend=list(orientation='h', y=-0.3))
    )
  
  # df_teams %>% ggplot(aes(x=Date, y=Steps, group=Team)) +
  #   geom_line(color='gray', alpha=0.4) +
  #   geom_line(data=df_teams[df_teams$Team=='ARR We There Yet?',], aes(x=Date, y=Steps), color='#35a4dc') +
  #   theme_minimal() + ylab('Steps') + xlab('')
  
  df_people %>% ggplot(aes(x=Date, y=Steps, group=Person)) +
    geom_line(color='gray', alpha=0.4) +
    geom_line(data=df_people[df_people$Person=='Laura',], aes(x=Date, y=Steps), color='#35a4dc') +
    theme_minimal() + ylab('Steps') + xlab('')
  
  # COMPARISON WITH PREVIOUS YEAR PLOT
  
  # THEN VS NOW PLOT
  output$then_vs_now_plot = renderPlotly(
    
    ggplotly(mean_then_vs_now %>% ggplot(aes(x=reorder(Name, -difference), y=difference, fill=difference)) + 
               geom_bar(stat='identity') + theme_minimal() + 
      ggtitle('Improvement since last challenge') + scale_fill_viridis_c(option='C', end=0.8) + 
        theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab('') +
      ylab('Increase in Mean Daily Steps'), tooltip='fill'),
    
  )
  
  # TREND PLOT
  output$trend_plot = renderPlotly(
    
  ggplotly(df_trend %>% ggplot(aes(x=Date, y=Steps, group=Name)) + 
               # geom_ribbon(aes(y=Steps, ymin=Steps-sd, ymax = Steps+sd), fill='gray', alpha=.25) +
               geom_line(color=df_trend$color, linewidth=df_trend$width, alpha=df_trend$alpha) + 
               theme_minimal() + ggtitle('General trend'),
          tooltip = 'Steps')
  )
  
  # WEEKDAYS PLOT
  output$weekday_plot = renderPlotly(
    
    ggplotly(df_weekdays %>% ggplot(aes(x=Day, y=Steps)) + geom_boxplot(fill='#35a4dc') + 
             theme_minimal() + theme(legend.position='none') + xlab('') + ggtitle('Weekday trends'))
    
  )
  
}



