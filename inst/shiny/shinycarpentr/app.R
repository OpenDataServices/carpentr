library(shiny)

ui <- fluidPage(
    titlePanel("Carbon pricing revenue estimator (prototype)"),
    mainPanel(h3("Open Data Services"),
    p("Not currently operational. For development purposes only"),
    checkboxGroupInput("options", "Choose options:",
                       choiceNames =
                           list(icon("money-bill"),
                                icon("leaf"),
                                icon("road"),
                                icon("hospital")),
                       choiceValues =
                           list("lower taxes",
                                "nature conservation",
                                "funding infrastructure",
                                "funding services")
    ),
    textOutput("txt"))
)

server <- function(input, output, session) {
    output$txt <- renderText({
        options <- paste(input$options, collapse = ", ")
        paste("You are interested in", options)
    })
}

shinyApp(ui, server)
