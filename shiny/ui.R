library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = judul,
                    tags$li(class = "dropdown", style = "padding: 8px;",
                            shinyjs::hidden(actionButton('logout',' ', class = "btn-danger",
                                                         style = "color: white;", icon("power-off"))))
    ),
    dashboardSidebar(
      sidebarMenuOutput("sidebarMenuReactive"),
      collapsed = TRUE
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$script(src = "js.cookie.js"),
      extendShinyjs(text = jsCode),
      uiOutput("ui")
    )
  ))