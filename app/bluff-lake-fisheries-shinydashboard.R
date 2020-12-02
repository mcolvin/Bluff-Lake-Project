######################################################
###------------------------------------------------###
###         BLUFF LAKE FISHERIES DASHBOARD         ###
###------------------------------------------------###
######################################################

# Install and load packages -----------------------------------------------

# Packages needed
packs <- c("shinydashboard")

# Install
# install.packages(packs)  # uncomment and run if packs are not installed

# Load
lapply(packs, library, character.only = TRUE)


# ui.R --------------------------------------------------------------------

dashboardPage(dashboardHeader(),
              dashboardSidebar(),
              dashboardBody())

# app.R -------------------------------------------------------------------

ui <- dashboardPage(
    # Header content
    dashboardHeader(title = "Bluff Lake Fisheries Dashboard"),
    
    # Sidebar content
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("dashboard")
        )
    )),
    
    # Body content
    dashboardBody(fluidRow(
        # Plot box
        box(plotOutput("histPlot", height = 250)),
        
        # Inputs box
        box(
            title = "Inputs",
            sliderInput(
                inputId = "nSlider",
                label = "N:",
                min = 10,
                max = 1000,
                value = 100,
                step = 10
            ),
            sliderInput(
                inputId = "meanSlider",
                label = "Mean:",
                min = 25,
                max = 75,
                value = 50
            ),
            sliderInput(
                inputId = "stdDevSlider",
                label = "SD:",
                min = 1,
                max = 15,
                value = 5
            ),
            sliderInput(
                inputId = "binSlider",
                label = "Bins:",
                min = 2,
                max = 20,
                value = 10
            )
        )
    ))
)

server <- function(input, output) {
    set.seed(20201201)
    
    output$histPlot <- renderPlot({
        # Simulate data
        normData <- rnorm(
            n = input$nSlider,
            mean = input$meanSlider,
            sd = input$stdDevSlider
        )
        
        # Build plot
        # histData <- normData[seq_len(input$binSlider)]
        hist(normData, 
             breaks = input$binSlider,
             xlim = c(0, 100))
    })
}

shinyApp(ui, server)
