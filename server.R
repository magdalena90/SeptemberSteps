
server <- function(input, output, clientData, session) {
  
  # Data loading/wrangling lives in global.R. get_data() returns a cached copy that's
  # auto-refreshed from Google Sheets after DATA_MAX_AGE_SECS, or immediately via the
  # 'Refresh data' button below
  refresh_count = reactiveVal(0)
  
  observeEvent(input$refresh_data, {
    refresh_count(refresh_count() + 1)
  }, ignoreInit = TRUE)
  
  app_data = reactive({
    refresh_count()
    get_data(force = isolate(refresh_count()) > 0)
  })
  
  # INPUT
  selected_plot_type = reactive({ input$plot_type })
  selected_aggregation = reactive({ input$unit })
  
  # TEXT
  output$winner = renderUI({
    
    data = app_data()
    
    if(selected_aggregation()=='Team'){
      winner = data$df_teams %>% filter(Cummulative_Steps==max(data$df_teams['Cummulative_Steps'])) %>% pull(Team)
    } else {
      winner = data$top_people %>% filter(Cummulative_Steps==max(data$df_people['Cummulative_Steps'])) %>% pull(Person)
    }
    
    winner_info = paste0('<h3>', winner, ' is in the lead!</h3><br>')
    
    HTML(winner_info)
  })
  
  # STEPS PLOTS
  output$plot = renderPlotly({
    
    data = app_data()
    
    # use plain column names instead of tidy-eval symbols, which ggplotly() mishandles for labels
    plot_df = data$df_dict[[selected_aggregation()]]
    plot_df$y_val = plot_df[[selected_plot_type()]]
    plot_df$group_val = plot_df[[selected_aggregation()]]
    
    # custom tooltip labels/format: y_val -> 'Steps' (comma thousands), group_val -> 'Team'
    plot_df$tooltip_text = paste0('Date: ', format(plot_df$Date, '%Y-%m-%d'),
                                   '<br />Steps: ', format(round(plot_df$y_val), big.mark=',', trim=TRUE, scientific=FALSE),
                                   '<br />Team: ', plot_df$group_val)
    
    plot = plot_df %>% 
           # explicit group= keeps lines connected; the unique per-row tooltip_text would otherwise split them
           ggplot(aes(x=Date, y=y_val, color=group_val, group=group_val, text=tooltip_text)) + 
           geom_line(linetype=plot_df$linetype) + 
           scale_color_manual(values=unique(plot_df[c('linetype','color')])[['color']]) +
           theme_minimal() + ylab(data$title_dict[[selected_plot_type()]]) +
           labs(color=selected_aggregation()) +
           theme(axis.title=element_text(family='Quicksand'),
                 legend.title=element_text(family='Quicksand'),
                 legend.text=element_text(family='Quicksand'))
    
    # comma thousands separator on the y axis
    plot = plot + scale_y_continuous(labels=scales::comma)
    
    ggplotly(plot, tooltip='text') %>% 
             layout(legend=list(orientation='h', y=-0.3))
    })
  
  # COMPARISON WITH PREVIOUS YEAR PLOT
  
  # THEN VS NOW PLOT
  output$then_vs_now_plot = renderPlotly({
    
    mean_then_vs_now = app_data()$mean_then_vs_now
    
    validate(need(nrow(mean_then_vs_now) > 0, 'No one has beaten their average from last year yet \u2014 check back later!'))
    
    ggplotly(mean_then_vs_now %>% ggplot(aes(x=reorder(Name, -difference), y=difference, fill=difference)) + 
               geom_bar(stat='identity') + theme_minimal(base_family='Quicksand') + 
      ggtitle('Improvement since last challenge') + scale_fill_viridis_c(option='C', end=0.8, trans='log10') + 
        theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab('') +
      ylab('Increase in Mean Daily Steps') + scale_y_log10(labels=scales::comma), tooltip='fill')
    })
  
  # TREND PLOT
  output$trend_plot = renderPlotly({
    
    df_trend = app_data()$df_trend
    
    ggplotly(df_trend %>% ggplot(aes(x=Date, y=Steps, group=Name)) + 
                 # geom_ribbon(aes(y=Steps, ymin=Steps-sd, ymax = Steps+sd), fill='gray', alpha=.25) +
                 geom_line(color=df_trend$color, linewidth=df_trend$width, alpha=df_trend$alpha) + 
                 theme_minimal() + ggtitle('General trend'),
            tooltip = 'Steps')
  })
  
  # WEEKDAYS PLOT
  output$weekday_plot = renderPlotly({
    
    df_weekdays = app_data()$df_weekdays
    
    ggplotly(df_weekdays %>% ggplot(aes(x=Day, y=Steps)) + geom_boxplot(fill='#35a4dc') + 
             theme_minimal() + theme(legend.position='none') + xlab('') + ggtitle('Weekday trends'))
    
  })
  
}



