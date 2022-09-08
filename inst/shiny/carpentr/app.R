library(shiny)
library(shinyWidgets)
library(bslib)
library(carpentr)
library(tidyverse)
library(cowplot)

indicator_survey_matrix<- read.csv('data-raw/indicators_survey_matrix.csv',row.names = 1,check.names = FALSE)
indicator_survey_matrix <- as.matrix(indicator_survey_matrix)
indicator_values <- get_wb_indicators(2000:2017,data_dir = 'data-raw') %>%
  mutate(value = value/1000000)
indicator_raw <- read_csv('data-raw/indicators.csv')

qs <- colnames(indicator_survey_matrix)
inds <- rownames(indicator_survey_matrix)

ui <- navbarPage("Carbon pricing and the energy transition",collapsible = TRUE,
                 id = "inTabset",
                 tags$style(HTML("
      .navbar-default .navbar-brand { display: none;}
  ")),                 selected = "1. Fill in questionnaire",
                 tabPanel("1. Fill in questionnaire",
                          fluidPage(tags$style(type = "text/css", "a{color: #044278;}"),
                                    setSliderColor(rep('#044278',3),c(1,2,3)),
                                    #theme = "bootstrap.css",
                                    title = h4("1. Fill in questionnaire"),
                                    value = 'p1',
                                    h1('Carbon pricing and the energy transition',style = 'color: #009966'),
                                    br(),
                                    h4(HTML("<span style = 'color:#044278'>Putting a price on carbon helps to bring down emissions and raises money for investment. But <span style = 'color:#009966'>how much money</span> , and <span style = 'color:#009966'>what could it be spent on?</span> First, tell us what you think about carbon pricing...</span>")),
                                    br(),
                                    sliderInput(inputId = 'q1',
                                                label = h5(qs[1]),
                                                min = 1,
                                                max = 9,
                                                value = 5,
                                                width = '100%'
                                    ),
                                    sliderInput(inputId = 'q2',
                                                label = h5(qs[2]),
                                                min = 1,
                                                max = 9,
                                                value = 5,
                                                width = '100%'
                                    ),
                                    sliderInput(inputId = 'q3',
                                                label = h5(qs[3]),
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
                                                    width = "100%"),
                                    br(),
                                    actionButton('viewResults', 'Go to results')
                                    ))),
                 tabPanel("2. View results",
                          value = 'p2',
                          fluidPage(align = 'center',
                                    h1(align = 'left','Carbon pricing and the energy transition',style = 'color: #009966'),
                                    br(),
                                    h4(htmlOutput("carbonpricetxt"),align = 'left'),
                                    br(),
                                    plotOutput("plots",width = "500px",height = "500px"),
                                    br()
                          )),
                 tabPanel("3. Share",
                          fluidPage(
                            value = 'p3',
                            h1(align = 'left','Carbon pricing and the energy transition',style = 'color: #009966'),
                            br(),
                            h4('Your indicators',style = 'color:#044278'),
                            br(),
                            tableOutput('resultsTab'),
                            downloadButton('downloadData', 'Download data')
                          )),
                 tabPanel("4. About",
                          fluidPage(
                            value = 'p4',
                            h1(align = 'left','Carbon pricing and the energy transition',style = 'color: #009966'),
                            h4('About this app',style = 'color:#044278'),
                            HTML("<p>This app is a prototype and estimates of carbon revenue should be considered crude estimates at best. Source code and information about data sources is available on <a  href = 'https://github.com/OpenDataServices/carpentr'>Github</a>, where you can also report issues.</p>"),
                            HTML("<p>Developed by <a href = 'https://opendataservices.coop/'>Open Data Services</a>, as part of the <a href = 'https://eiti.org/events/datathon-innovative-solutions-data-driven-energy-transition'>EITI Datathon 2022</a>.</p>")
                          )))


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
    paste0("<span style = 'color:#044278'>There could have been </span><span style = 'color:#009966'>$",
           carbonrev(),
           " million</span><span style = 'color:#044278'> generated from carbon pricing of oil and gas production in Senegal in 2017. This is </span><span style = 'color:#009966'>",
           ifelse(carbonrev()<oilgasrev(),
                  paste0(round(carbonrev()/oilgasrev()*100), "%</span><span style = 'color:#044278'> of"),
                  paste0('$',carbonrev() - oilgasrev()," million more</span><span style = 'color:#044278'> than")),
           " the entire government revenue generated from the extractives industries. This carbon pricing revenue could have been used for...</span>")
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


  output$plots <- renderPlot({

    toplot <- indicator_values[indicator_values$name == userdata()$name[1],] %>%
      bind_rows(indicator_values[indicator_values$name == userdata()$name[2],]) %>%
      bind_rows(indicator_values[indicator_values$name == userdata()$name[3],]) %>%
      mutate(lab = rep(userdata()$description,
                       c(nrow(indicator_values[indicator_values$name == userdata()$name[1],]),
                         nrow(indicator_values[indicator_values$name == userdata()$name[2],]),
                         nrow(indicator_values[indicator_values$name == userdata()$name[3],]))
      )
      )

    plot_indicator(toplot,carbonrev(),2017)+
      facet_wrap(~lab,scales = 'free',ncol = 2) +
      theme(legend.position = c(1, 0.22),
            legend.justification = c(1, 0),
            legend.text = element_text(size = 15))

  })

  output$resultsTab <- renderTable({
    userdata() %>%
      select(Indicator = description,
             Rank)
  })

  observeEvent(input$viewResults, {
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
