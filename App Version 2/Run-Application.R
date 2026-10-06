source("Functions and Helpers/Packages-Themes.R")
source("Functions and Helpers/Helper-function.R")
source("Functions and Helpers/Summarize-Pert-DOB.R")
source("Functions and Helpers/Plotting-functions-DOB.R")
source("Data/demo_data.R")

gs4_auth(email = "fish.rule.forever12@gmail.com")

source("App-UI-DOB.R")
source("App-Server-DOB.R")

shinyApp(ui, server)
