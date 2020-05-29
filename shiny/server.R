library(shiny)
library(shinydashboard)

shinyServer(function(session, input, output) {
    
    # Reactive value untuk menyimpan jika sudah login dan data user yang login
    sudah_login <- reactiveVal(value = FALSE)
    data_user <- reactiveVal(value = NULL)
    
    # Cek apakah ada cookie yang tersimpan dan sesuai dengan data yang tersimpan
    observe({
        js$getcookie()
        credentials <- readRDS("data/credentials.rds")
        session_cred <- credentials$session
        
        if (!is.null(input$jscookie) && input$jscookie!="" && input$jscookie %in% session_cred) {
            sudah_login(TRUE)
            row_email <- which(session_cred==input$jscookie)
            data_user(credentials[row_email,])
            shinyjs::show("logout")
        }
        
    })
    
    
    # Skrip untuk login dan logout ketika menekan tombol
    observeEvent(input$login, {
        withBusyIndicatorServer("login", {
            
            cred_user <- df_credentials %>%
                filter(email == input$email)
            
            if(nrow(cred_user)==0) {
                stop("Email not registered")
            }
            
            sudah_login(TRUE)
            data_user(cred_user)
            
            sessionid <- paste(collapse = '', sample(x = c(letters, LETTERS, 0:9), size = 64, replace = TRUE))
            js$setcookie(sessionid)
            update_user_session(user = input$email,sessionid)
            
            shinyjs::show("logout")
            
        })
    })
    
    observeEvent(input$logout, {
        shinyjs::hide("logout")
        sudah_login(FALSE)
        data_user(NULL)
        js$rmcookie()
    })
    
    # Skrip UI di server
    output$ui <- renderUI({
        if (!sudah_login()) {
            # Skrip UI login page, akan muncul ketika belum login
            fluidRow(
                column(width = 12, align='center',
                       br(), br(), br(), br(), br(),
                       h3("Silahkan Login untuk bisa melihat data"),
                       br(), br(),
                       textInput('email', '', placeholder = 'email'),
                       br(),
                       withBusyIndicatorUI(actionButton('login', 'Login',class = "btn-primary"))
                )
            )
        } else {
            # Skrip UI ketika sudah login
            tabItems(
                tabItem(tabName = "data",
                        box(width = 12,
                            uiOutput('text_user'),
                            br(),
                            plotOutput("box")
                        )
                ))
        }})
    
    output$sidebarMenuReactive <- renderMenu({
        if (!sudah_login()) {
            NULL
        } else {
            sidebarMenu(
                sidebarMenu(id="tabsMenu",
                            menuItem("Data", tabName = "data")
                )
            )
        }
    })
    
    # Mementukan apakah user itu supervisor atau bukan
    isSupervisor <- reactive({
        req(data_user())
        
        return(data_user()$species == "supervisor")    
    })
    
    # Output yang baru akan dibuat ketika user sudah login
    output$text_user <- renderUI({
        req(data_user())

        if (isSupervisor()){
            # Untuk supervisor akan ada dropdown untuk memilih species
            tagList(
                HTML(paste0("Hi <code>", data_user()$name, 
                            "</code>, kamu adalah <code>", data_user()$species ,"</code>.")),
                hr(),
                p("Sebagai supervisor, kamu bisa pilih cabang apapun"),
                selectInput("species", "", unique(iris$Species))
            )
        } else{
            # untuk user non supervisor tampilkan langsung sesuai species nya
            HTML(paste0("Hi <code>", data_user()$name, "</code>, kamu bisa melihat cabang <code>",
                        data_user()$species,"</code> saja."))
        }
    })

    species <- reactive({
        req(data_user()$species)
        
        species <- data_user()$species
        
        if (isSupervisor()){
            if (is.null(input$species)){
                return(iris$Species[1])
            }
            return(input$species)
        }
        
        return(species)
    })
    
    filteredData <- reactive({
        req(species())

        iris %>% 
            filter(Species == species()) %>% 
            select(-Species)
    })
    
    output$box <- renderPlot({    
        boxplot(
            filteredData()
        )
    })
    
})
