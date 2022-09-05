library(shiny)
library(bslib)
library(carpentr)

indicator_survey_matrix<- read.csv('../../../data-raw/indicators_survey_matrix.csv',row.names = 1,check.names = FALSE)
indicator_survey_matrix <- as.matrix(indicator_survey_matrix)
indicator_values <- get_wb_indicators(2017,data_dir = '../../../data-raw')


qs <- colnames(indicator_survey_matrix)
inds <- rownames(indicator_survey_matrix)

ui <- fluidPage(
    theme = "bootstrap.css",
    titlePanel("Carbon pricing revenue estimator (prototype)"),
    br(),
    mainPanel(
    h5("Not currently operational. For development purposes only."),
    br(),
    h3("Step 1: fill in questionnaire"),
    br(),
    h4("On a scale from 1-9, where 1 is 'strongly disagree' and 9 is 'strongly agree', please answer the following:"),
    br(),
    # Input: Slider example ----
    sliderInput(inputId = 'q1',
                label = h5(qs[1]),
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    sliderInput(inputId = 'q2',
                label = h5(qs[2]),
                min = 1,
                max = 9,
                value = 1,
                width = '100%'
    ),
    sliderInput(inputId = 'q3',
                label = h5(qs[3]),
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
    h4("Based on your responses, this carbon pricing data could be spent on:"),
    h5(htmlOutput("ind1")),
    h5(htmlOutput("ind2")),
    h5(htmlOutput("ind3")),
    br(),
    width = 12
    )
)

server <- function(input, output, session) {

    rev <- reactive({round(eiti_oilgas_revenue("SN",
                                               2017,
                                               as.numeric(input$carbon_price)))})

    output$carbonpricetxt <- renderText({
        paste0("Based on EITI data from Senegal in 2017, there would be <b>$",
               rev(),
               " USD</b> generated from carbon pricing at your selected price")
    })

    dist <- reactive({
        inputs <- c(input$q1,input$q2,input$q3)
        apply(indicator_survey_matrix,1,euclidean,inputs)
    })


    output$ind1 <- renderText({
        nam <- inds[which(order(dist()) == 1)]
        val <- indicator_values[indicator_values$name == nam,'value']
        paste0(nam," ",
              round(rev()/val*100),"%")
    })

    output$ind2 <- renderText({
        nam <- inds[which(order(dist()) == 2)]
        val <- indicator_values[indicator_values$name == nam,'value']
        paste0(nam," ",
               round(rev()/val*100),"%")    })

    output$ind3 <- renderText({
        nam <- inds[which(order(dist()) == 3)]
        val <- indicator_values[indicator_values$name == nam,'value']
        paste0(nam," ",
               round(rev()/val*100),"%")    })
}

shinyApp(ui, server)
