library(shiny)
library(plotly)
library(DT)

shinyApp(
    ui = navbarPage("Testing the saving to dataframes",
                    tabPanel("Predition",
                             
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
                                 selectInput("know_ini", "Is the Initiator (individual or group) of the event known?",
                                             c("Initiator Unknown" = "0",
                                               "Initiator Known" = "1",
                                               "Initiator Ambiguous" = "2",
                                               "Initiator Suspected" = "5")),
                                 selectInput("weapon", "Type of Weapon Used",
                                             c("No Weapon Used" = '1',
                                               "Fake weapon Used" = '2',
                                               "Body Parts" = '3',
                                               "Animal" = '4',
                                               "Vehicles" = '5',
                                               "Computer" = '6',
                                               "Blunt Instrument" = '7',
                                               "Tear gas, Mace, etc." = '8',
                                               "Knives/sharp Instrument" = '9',
                                               "IED" = '10',
                                               "Letter Bomb" = '11',
                                               "Fire" = '12',
                                               "Non-lethal Projectiles" = '13',
                                               "Small Arms" = '14',
                                               "Light Weapon" = '15',
                                               "Incendiary Device" = '16',
                                               "Land Mine" = '17',
                                               "Explosives, Grenade" = '18',
                                               "Car Bomb" = '19',
                                               "Tanks/armored Vehicles" = '20',
                                               "Field Artillery" = '21',
                                               "Missile/rocket" = '22',
                                               "Aircraft Munitions" = '23',
                                               "Naval Power" = '24',
                                               "Biochemical Weapons" = '25',
                                               "Unspecified" = '26',
                                               "Other weapon" = '27')),
                                 selectInput("sc_animosity", "Socio-cultural Animosity",
                                             c("True" = TRUE,
                                               "False" = FALSE)),
                                 selectInput("anti_gov_sentmnts", "Anti-government sentiments",
                                             c("True" = TRUE,
                                               "False" = FALSE)),
                                 actionButton("submit", ("Submit"))
                             )
                             
                    ),
                    tabPanel("Table",
                             
                             "Testing Table",
                             DT::dataTableOutput("table"),
                             hr(),
                             hr(),
                             "Combined Table",
                             DT::dataTableOutput("Combined_table")
                    )
    ),
    
    server = function(input, output) {
        
        new_event <- reactive({ data.frame("month" = factor(input$month), "week" = factor(input$week),
                                "region" = factor(input$region), "loc_type" = factor(input$loc_type),
                                "ev_type" = factor(input$ev_type), "n_killed_a" = factor(input$n_killed_a),
                                "know_ini" = factor(input$know_ini), "weapon" = factor(input$weapon),
                                "sc_animosity" = input$sc_animosity, "anti_gov_sentmnts" = input$anti_gov_sentmnts)})
       
        #Initial Dataframe
        pred = eventReactive(input$submit,{
            new_event
            print(str(new_event))
        })
        
        #Table
        output$table = renderDataTable(pred())
        
    }#Server End
)