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

# Header content
header <- dashboardHeader(title = "Bluff Lake Fisheries Dashboard")

# Sidebar content
sidebar <- dashboardSidebar(sidebarMenu(
    menuItem(
        "Overview and description",
        tabName = "overview",
        icon = icon("fish")
    ),
    menuItem(
        "Bathymetry",
        tabName = "bathymetry",
        icon = icon("atlas")
    ),
    menuItem(
        "Objectives",
        tabName = "objectives",
        icon = icon("bullseye")
    ),
    menuItem(
        "Water releases",
        tabName = "water_releases",
        icon = icon("water")
    )
))

# Body content
body <- dashboardBody(tabItems(
    # Overview tab
    tabItem(tabName = "overview",
            h2("Overview and description")),
    
    # Dashboard tab
    tabItem(tabName = "bathymetry",
            h2("Bathymetry")),
    
    # Objectives tab
    tabItem(tabName = "objectives",
            h2("Objectives")),
    
    # Bath tab
    tabItem(tabName = "water_releases",
            h2("Water releases"),
            fluidRow(
                
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
))

ui <- dashboardPage(header, sidebar, body)

# app.R -------------------------------------------------------------------

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
