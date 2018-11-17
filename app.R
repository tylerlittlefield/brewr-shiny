library(shiny)    # app framework
library(ggplot2)  # plotting
library(ggsci)    # color scales
library(forcats)  # reordering factors
library(jsonlite) # easy json to dataframe
library(brewr)    # homebrew json api wrapper
library(readr)    # convert datatypes easily
library(curl)     # need so shiny wont complain

# Define the UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
    
    # Use custom CSS for Source Code Pro font 
    includeCSS("style.css"),
    
    # Create title 
    headerPanel("Homebrew Analytics"),
    
    # Contruct the left panel with all its options 
    sidebarPanel(
        p("This shiny app is a companion tool for the", 
        a(href = "https://github.com/tyluRp/brewr", "brewr"),
        "package I developed. The bulk of the work is done using", 
        a(href = "https://github.com/jeroen/jsonlite", "jsonlite."), "The data 
        itself is provided by", 
        a(href = "https://formulae.brew.sh/analytics/", "Homebrews JSON API,"),
        "I encourage you to check it out if interested!"),
        br(),
        width = 4,
        column(8, selectInput(inputId = "category", 
                    label   = "Category:", 
                    choices = c("Install Events"            = "/analytics/install",
                                "Install On Request Events" = "/analytics/install-on-request",
                                "Build Error Events"        = "/analytics/build-error",
                                "macOS Versions for Events" = "/analytics/os-version"), 
                    width = 215)),
        column(4, selectInput(inputId = "days", 
                    label   = "Days:", 
                    choices = c("30 Days"  = "/30d",
                                "90 Days"  = "/90d",
                                "365 Days" = "/365d"),
                    width = 120)),
        column(12, sliderInput(inputId = "slider_range", 
                    label = "Slider Range:", 
                    min = 1, 
                    max = 100, 
                    value = c(1, 30))),
        div(DT::dataTableOutput("table"), style = "font-size:80%")
    ),
    
    # Store the plot on main panel 
    mainPanel(plotOutput("plot"))
)

# Define service logic
# ------------------------------------------------------------------------------
server <- function(input, output) {
    
    # Observe the endpoints for debugging purposes 
    observeEvent(c(input$category, input$days), {
        print(paste0("Endpoint: ", input$category, input$days))
    })
    
    # Render the datatable 
    output$table <- DT::renderDataTable(DT::datatable({
        fromJSON(brewr(paste0(input$category, input$days)))[["items"]]
    }, extensions = c("Scroller"), 
       options = list(scrollX = TRUE, 
                      deferRender = TRUE, 
                      scrollY = 320, 
                      dom = "lftp")))
    
    # Render the plot 
    output$plot <- renderPlot({
        df <- fromJSON(brewr(paste0(input$category, input$days)))[["items"]]
        df <- readr::type_convert(df)
        df <- df[input$slider_range[1]:input$slider_range[2], ]
        df <- stats::na.omit(df)
        ggplot(df, aes_string(paste0("fct_reorder(", names(df)[2], "," , names(df)[3], ")"), names(df)[3])) +
            geom_col(aes_string(fill = names(df)[3]), show.legend = FALSE) +
            scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
            scale_fill_material("blue-grey") +
            coord_flip() +
            theme_minimal() +
            labs(x = NULL,
                 y = NULL) +
            theme(text = element_text(family = "SourceCodePro-Regular"), # Lazy/not reproducible
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank())
    }, height = 700)
}

# Run the application 
shinyApp(ui, server)
