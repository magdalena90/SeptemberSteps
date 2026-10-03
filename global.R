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
  ltypes = c(rep('solid',min(n,6)), rep('twodash',max(0,n-6)))
  cols = c(gg_colour_hue(min(n,6)), gg_colour_hue(6))[1:n]
  
  styles_df = data.frame(id_col=df[[id_col]] %>% unique, color=cols, linetype=ltypes)
  colnames(styles_df)[1] = id_col
  
  df_with_styles = left_join(df, styles_df, by=id_col)
  
}

# AUTH
# Run this the first time
# options(gargle_oauth_cache = '.secrets')
# gs4_auth()
# gs4_deauth()

# Only works after you've created the .secrets cache with the code above. Done once per
# app process; the token doesn't need to be re-acquired every time the data is refreshed
gs4_auth(cache = '.secrets', email = 'magdalena.nta@gmail.com')

# LOAD DATA FROM DRIVE
# Fetches and wrangles the sheet data, returning everything the UI needs as a list
load_data = function(){
  
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
  df_teams = df %>% dplyr::select(-Name) %>% group_by(Team, Date) %>% summarise(Steps=sum(value), .groups='drop_last') %>% 
             mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup %>% filter(Team!='pending')
  
  df_teams = assign_lineplot_styles(df_teams, 'Team') 
  
  # People trends
  top_n = 12
  
  df_people = df %>% dplyr::select(-Team) %>% group_by(Name, Date) %>% summarise(Steps=sum(value), .groups='drop_last') %>%
              mutate(Cummulative_Steps=cumsum(Steps)) %>% ungroup %>% dplyr::rename('Person'=Name) %>%
              filter(Person != 'aux')
  
  top_people = df_people %>% filter(Date == max(df_people$Date)) %>% arrange(desc(Cummulative_Steps)) %>% head(top_n)
  
  df_people = df_people %>% filter(Person %in% top_people$Person) %>% 
              group_by(Person) %>% mutate(total_steps=max(Cummulative_Steps)) %>% ungroup %>% 
              arrange(desc(total_steps))
  
  df_people = assign_lineplot_styles(df_people, 'Person') %>% arrange(Person)
  
  # General trend
  df_mean = df %>% dplyr::select(Date, value) %>% group_by(Date) %>% summarise(Steps=mean(value), sd=sd(value), .groups='drop')
  
  df_trend = df %>% mutate('color'='#35a4dc', 'Steps'=value, 'width'=0.5, 'alpha'=0.5, 'sd'=0) %>% 
    dplyr::select(Date, Name, Steps, color, width, alpha, sd) %>% 
    rbind(df_mean %>% mutate('Name'='Mean', 'color'='gray', 'width'=2, 'alpha'=1)) %>%
    arrange(Name) %>% mutate(Steps=as.integer(Steps))
  
  # Days of the week trend
  df_weekdays = df %>% mutate('Day'=weekdays(Date), 'Steps'=value) %>% filter(Date!='2023-09-03') %>% 
                mutate('Day'=factor(Day, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')))
  
  title_dict = list('Steps' = 'Daily Steps', 'Cummulative_Steps' = 'Cummulative Steps')
  df_dict = list('Team' = df_teams, 'Person' = df_people)
  
  list(df=df, df_teams=df_teams, df_people=df_people, top_people=top_people, mean_then_vs_now=mean_then_vs_now,
       df_trend=df_trend, df_weekdays=df_weekdays, title_dict=title_dict, df_dict=df_dict)
}

# CACHE
# How long the cached data is considered fresh before it's re-fetched from Google Sheets
DATA_MAX_AGE_SECS = 15 * 60

.data_cache = new.env()

# Returns the cached data, transparently refreshing it from Google Sheets if it's stale or force=TRUE
get_data = function(force = FALSE){
  
  stale = force || is.null(.data_cache$loaded_at) ||
          difftime(Sys.time(), .data_cache$loaded_at, units='secs') > DATA_MAX_AGE_SECS
  
  if(stale){
    .data_cache$data = load_data()
    .data_cache$loaded_at = Sys.time()
  }
  
  .data_cache$data
}
