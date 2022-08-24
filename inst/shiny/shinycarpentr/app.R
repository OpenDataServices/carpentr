library(shiny)
library(bslib)
library(carpentr)

ui <- fluidPage(
    theme = "bootstrap.css",
    titlePanel("Carbon pricing revenue estimator (prototype)"),
    br(),
    mainPanel(
    h5("Not currently operational. For development purposes only.",),
    br(),
    h3("Step 1: fill in questionnaire"),
    br(),
    h4("On a scale from 1-9, where 1 is 'strongly disagree' and 9 is 'strongly agree', please answer the following:"),
    br(),
    # Input: Slider example ----
    sliderInput(inputId = "carbon_pricing_belief",
                label = h5("I believe that carbon pricing is good for my local economy"),
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    sliderInput(inputId = "gov_trust",
                label = h5("I trust my government to spend money generated from carbon taxes on local infrastructure"),
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    sliderInput(inputId = "personal_concern",
                label = h5("I am concerned about the impacts of carbon pricing on myself, my friends, or my family"),
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    br(),
    h5(radioButtons(inputId = "carbon_price",
                 label = h4("I'd like to see results for:"),
                 choices = c("High carbon pricing" = 50,
                   "Medium carbon pricing" = 35,
                   "Low carbon pricing" = 20),
                 inline = TRUE,
                 width = "100%")),
    br(),
    h3("Step 2: view results"),
    br(),
    h4(htmlOutput("carbonpricetxt")),
    br(),
    h4(htmlOutput("usrtxt")),
    br(),
    width = 12
    )
)

server <- function(input, output, session) {

    output$carbonpricetxt <- renderText({
        rev <- round(eiti_oilgas_revenue("SN",2017,as.numeric(input$carbon_price)))
        paste0("Based on EITI data from Senegal in 2017, there would be <b>$",
               rev,
               " USD</b> generated from carbon pricing at your selected price")
    })

    output$usrtxt <- renderText({
        carbon_pricing_belief <- ifelse(input$carbon_pricing_belief >5,"high","low")
        gov_trust <- ifelse(input$gov_trust >5,"high","low")
        personal_concern <- ifelse(input$personal_concern >5,"high","low")

        paste("You have <b>",
              carbon_pricing_belief,
              "</b>belief in carbon pricing and <b>",
              gov_trust,
              "</b>trust in government and<b>",
              personal_concern,
              "</b>personal concern about carbon pricing")
    })
}

shinyApp(ui, server)
