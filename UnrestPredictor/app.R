library(shiny)
library(plotly)
library(DT)

shinyApp(
    ui = navbarPage("Testing the saving to dataframes",
                    tabPanel("Plot",
                             
                             mainPanel(
                                 numericInput("month", "Month",value = 1, min = 1, max = 12, step = 1),
                                 numericInput("week", "During which week of the month did the event start?",value = 1, min = 1, max = 5, step = 1),
                                 selectInput("region", "Region",
                                             c("Sub-Saharan Africa" = "1",
                                               "Asia" = "2",
                                               "Europe" = "3",
                                               "Latin America & Caribbean" = "4",
                                               "North America" = "5",
                                               "Oceania" ="6",
                                               "Northern Africa" = "7",
                                               "Middle East" = "8")),
                                 selectInput("loc_type", "Location Type",
                                             c('Whole Country'= '0',
                                               'Sparsely Populated Area'= '1',
                                               'Densely Populated Area' = '2',
                                               'Border Area' = '3',
                                               'Road' = '4',
                                               'Checkpoint' = '5',
                                               'Bridge' = '6',
                                               'Railway' = '7',
                                               'Harbor' = '8',
                                               'Residential Property' = '9',
                                               'Shopping Area' = '10',
                                               'Hotel' = '11',
                                               'Ground Transportation Center' = '12',
                                               'Airport' = '13',
                                               'Industrial Property' = '14',
                                               'Office Complex' = '15',
                                               'Religious Site' = '16',
                                               'Medical Facility' = '17',
                                               'School Site' = '18',
                                               'Recreational Site' = '19',
                                               'Public Monument' = '20',
                                               'Government Facility' = '21',
                                               'Military Facility' = '22',
                                               'Embassy/Consulate' = '23',
                                               'Air Space' = '24',
                                               'Water Space' = '25',
                                               'Rebel Stronghold' = '26',
                                               'Unspecified Location' = '27',
                                               'Other Location' = '28')),
                                 selectInput("ev_type", "Type of Event",
                                             c("Political Expression" = "1",
                                               "Political Attacks" = "2",
                                               "Disruptive State Acts" = "4",
                                               "Political Reconfigurations" = "5")),
                                 numericInput("n_killed_a", "Number of Fatalities",value = 0, min = 0, step = 1),
                                 selectInput("know_ini", "Is the Initiator of the event known?",
                                             c("Initiator Unknown" = "0",
                                               "Initiator Known" = "1",
                                               "Initiator Ambiguous" = "2",
                                               "Initiator Suspected" = "5")),
                                 sliderInput('chanceofsale',"Probability of Sale", min=1, max=10,value=5),
                                 actionButton("submit", ("Submit"))
                             )
                             
                    ),
                    tabPanel("Table",
                             
                             "Testing Table",
                             DT::dataTableOutput("Test_Table"),
                             hr(),
                             hr(),
                             "Combined Table",
                             DT::dataTableOutput("Combined_table")
                    )
    ),
    
    server = function(input, output) {
        
        #Testdata <- data.frame("month" = "month", "week" = "week",
                            #   "region" = "region", "chanceofsale" = "5")
        
        #Initial Dataframe
        FinalData = eventReactive(input$submit,{
            Testdata = data.frame("month" = input$month, "week" = input$week,
                                        "region" = input$region,
                                        "chanceofsale" = as.character(input$chanceofsale))
        })
        
        #Initial Table
        
        #Combined Table
        output$Combined_table = renderDataTable(FinalData())
        
    }#Server End
)