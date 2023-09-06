
setwd('~/Otros/SI pedometer challenge/')
source('helper.R')
source('server.R')

# Run the application 
shinyApp(ui = ui, server = server)
