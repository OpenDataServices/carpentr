library(shiny)
library(shinyWidgets)
library(bslib)
library(carpentr)
library(tidyverse)

indicator_survey_matrix<- read.csv('data-raw/indicators_survey_matrix.csv',row.names = 1,check.names = FALSE)
indicator_survey_matrix <- as.matrix(indicator_survey_matrix)
indicator_values <- get_wb_indicators(2000:2017,data_dir = 'data-raw') %>%
    mutate(value = value/1000000)
indicator_raw <- read_csv('data-raw/indicators.csv')

qs <- colnames(indicator_survey_matrix)
inds <- rownames(indicator_survey_matrix)

ui <- fluidPage(title = "Carbon pricing and the energy transition",
                tags$style(type = "text/css", "a{color: #044278;}"),
                setSliderColor(rep('#696969',3),c(1,2,3)),
    #theme = "bootstrap.css",
    titlePanel(h1("Carbon pricing and the energy transition",style = "color:#cd2973")),
    mainPanel(
        br(),
        tabsetPanel(
            id = "inTabset",
            tabPanel(title = "1. Fill in questionnaire",
                     value = 'p1',
                     br(),
                     p("On a scale from 1-9, where 1 is 'strongly disagree' and 9 is 'strongly agree', please answer the following:"),
                     br(),
                     sliderInput(inputId = 'q1',
                                 label = h5(qs[1],style = "color:#044278"),
                                 min = 1,
                                 max = 9,
                                 value = 5,
                                 width = '100%'
                     ),
                     sliderInput(inputId = 'q2',
                                 label = h5(qs[2],style = "color:#044278"),
                                 min = 1,
                                 max = 9,
                                 value = 5,
                                 width = '100%'
                     ),
                     sliderInput(inputId = 'q3',
                                 label = h5(qs[3],style = "color:#044278"),
                                 min = 1,
                                 max = 9,
                                 value = 5,
                                 width = '100%'
                     ),
                     br(),
                     h5(radioButtons(inputId = "carbon_price",
                                     label = h4("I'd like to see results for:",style = "color:#044278"),
                                     choices = c("Low carbon pricing" = 10,
                                                 "Medium carbon pricing" = 30,
                                                 "High carbon pricing" = 50),
                                     inline = TRUE,
                                     width = "100%")
                     ),
                     br(),
                     actionButton('jumpToResults', 'See results')
            ),
            tabPanel(title = "2. View results",
                     value = 'p2',
                     br(),
                     h4(htmlOutput("carbonpricetxt")),
                     br(),
                     h4("Based on your responses, this carbon pricing data could be used for:"),
                     h5(htmlOutput("ind1")),
                     plotOutput("p1"),
                     h5(htmlOutput("ind2")),
                     plotOutput("p2"),
                     h5(htmlOutput("ind3")),
                     plotOutput("p3"),
                     br()
            ),
            tabPanel(title = "3. Share",
                     value = 'p3',
                     h5('Your indicators'),
                     br(),
                     tableOutput('resultsTab'),
                     downloadButton('downloadData', 'Download data')
                     ),
            tabPanel(title = "About",
                     value = 'p4',
                     h5("About this app"),
                     HTML("<p>This app is a prototype and estimates of carbon revenue should be considered crude estimates at best. Source code and information about data sources is available on <a  href = 'https://github.com/OpenDataServices/carpentr'>Github</a>.</p>"),
                     HTML("<p>Developed by <a href = 'https://opendataservices.coop/'>Open Data Services</a>, as part of the <a href = 'https://eiti.org/events/datathon-innovative-solutions-data-driven-energy-transition'>EITI Datathon 2022</a>.</p>")
                     )
        )
    )
)

server <- function(input, output, session) {

    carbonrev <- reactive({round(eiti_oilgas_carbon_revenue("SN",
                                               2017,
                                               as.numeric(input$carbon_price)/1000000))})

    oilgasrev <- reactive({
      read_eiti('SN',2017,'revenue') %>%
        mutate(value = round(as.numeric(value)/1000000)) %>%
        pull(value) %>%
        sum()})

    output$carbonpricetxt <- renderText({
        paste0("Based on EITI data from Senegal in 2017, there would be <span style = 'color:#cd2973'>$",
               carbonrev(),
               " million USD</span> generated from carbon pricing at your selected price. This is <span style = 'color:#cd2973'>",
               ifelse(carbonrev()<oilgasrev(),
                      paste0(round(carbonrev()/oilgasrev()*100), '%</span> of'),
                      paste0('$',carbonrev() - oilgasrev(),' million more</span> than')),
               " the entire government revenue generated from the extractives industries.")
    })

    dist <- reactive({
        inputs <- c(input$q1,input$q2,input$q3)
        apply(indicator_survey_matrix,1,euclidean,inputs)
    })

    userdata <- reactive({
        tibble(name = inds[order(dist())][1:3]) %>%
            left_join(indicator_raw) %>%
            mutate(Rank = c(1:3))
    })


    output$ind1 <- renderText({
        userdata()$description[1]
    })

    output$p1 <- renderPlot({
        toplot <- indicator_values[indicator_values$name == userdata()$name[1],]
        plot_indicator(toplot,carbonrev(),2017)
    })

    output$ind2 <- renderText({
        userdata()$description[2]
    })
    output$p2 <- renderPlot({
        toplot <- indicator_values[indicator_values$name == userdata()$name[2],]
        plot_indicator(toplot,carbonrev(),2017)
    })


    output$ind3 <- renderText({
        userdata()$description[3]
      })
    output$p3 <- renderPlot({
        toplot <- indicator_values[indicator_values$name == userdata()$name[3],]
        plot_indicator(toplot,carbonrev(),2017)
    })

    output$resultsTab <- renderTable({
        userdata() %>%
            select(Indicator = description,
                   Rank)
        })

    observeEvent(input$jumpToResults, {
        updateTabsetPanel(session,
                          "inTabset",
                          selected = "p2")
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(        userdata() %>%
                              select(Indicator = description,
                                     Rank), con)
      }
    )
}

shinyApp(ui, server)
