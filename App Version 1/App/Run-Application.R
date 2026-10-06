source("Functions and Helpers/Packages-Themes.R")
source("Functions and Helpers/Helper-function.R")
source("Functions and Helpers/Summarize-Pert.R")
source("Functions and Helpers/Plotting-functions.R")
source("Data/demo_data.R")
source("App/App-UI.R")
source("App/App-Server.R")

shinyApp(ui, server)
