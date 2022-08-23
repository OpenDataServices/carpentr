library(shiny)
library(carpentr)

ui <- fluidPage(
    titlePanel("Carbon pricing revenue estimator (prototype)"),
    mainPanel(
    h3("Not currently operational. For development purposes only."),
    br(),
    h3("Step 1: fill in questionnaire"),
    br(),
    p("On a scale from 1-9, where 1 is 'strongly disagree and 9 is strongly agree, please answer the following:"),

    # Input: Slider example ----
    sliderInput(inputId = "carbon_pricing_belief",
                label = "I believe that carbon pricing is good for my local economy",
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    sliderInput(inputId = "gov_trust",
                label = "I trust my government to spend money generated from carbon taxes on local infrastructure",
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    sliderInput(inputId = "personal_concern",
                label = "I am concerned about the impacts of carbon pricing on myself, my friends, or my family",
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    br(),
    radioButtons(inputId = "carbon_price",
                 label = "I'd like to see results for:",
                 choices = c("High carbon pricing" = 50,
                   "Medium carbon pricing" = 35,
                   "Low carbon pricing" = 20),
                 inline = TRUE,
                 width = "100%"),
    br(),
    h3("Step 2: view results"),
    br(),
    textOutput("carbonpricetxt"),
    br(),
    textOutput("usrtxt"),
    br()
    )
)

server <- function(input, output, session) {

    output$carbonpricetxt <- renderText({
        rev <- round(eiti_oilgas_revenue("SN",2017,as.numeric(input$carbon_price)))
        paste0("Based on EITI data from Senegal in 2017, there would be $",
               rev,
               " USD generated from carbon pricing at your selected price")
    })

    output$usrtxt <- renderText({
        carbon_pricing_belief <- ifelse(input$carbon_pricing_belief >5,"high","low")
        gov_trust <- ifelse(input$gov_trust >5,"high","low")
        personal_concern <- ifelse(input$personal_concern >5,"high","low")

        paste("You have",
              carbon_pricing_belief,
              "belief in carbon pricing and",
              gov_trust,
              "trust in government and",
              personal_concern,
              "personal concern about carbon pricing")
    })
}

shinyApp(ui, server)
