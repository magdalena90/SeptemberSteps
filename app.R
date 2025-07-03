
# setwd('~/Otros/SI pedometer challenge/')
setwd('~/Otros/SI_September_steps')
source('helper.R')
source('server.R')

# Run the application 
shinyApp(ui = ui, server = server)
