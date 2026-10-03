
# setwd('~/Otros/SI pedometer challenge/')
setwd('/Users/magda/SpaceIntelligence/Others/SI_September_steps')
source('global.R')
source('server.R')
source('ui.R')

# Run the application 
shinyApp(ui = ui, server = server)
