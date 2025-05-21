# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Recreational Fisheries Decision Support Tool"),
  #### Regulation Selection ####
  tabsetPanel(
    tabPanel( "Regulation Selection",
              strong(div("REMINDER: (1) select state(s)  (2) Make selections below (3) click run me and then the `Results` tab to run model", style = "color:blue")), # Warning for users
              shinyWidgets::awesomeCheckboxGroup( # Select which state(s) to run
                inputId = "state", 
                label = "State", 
                choices = c("MA", "RI", "CT", "NY", "NJ", "NJSplit", "DE",  "MD", "VA", "NC"),
                inline = TRUE,
                status = "danger"),
              
              #Run Button
              actionButton("runmeplease", "Run Me"), 
              textInput("Run_Name", "Please name this using your initials and the number of the run (ex. AB1)."),
              
              # Add UI code for each state
              uiOutput("addMA"),
              uiOutput("addRI"),
              uiOutput("addCT"), 
              uiOutput("addNY"),
              uiOutput("addNJ"), 
              uiOutput("addNJ_Split"),
              uiOutput("addDE"),
              uiOutput("addMD"),
              uiOutput("addVA"), 
              uiOutput("addNC")),
    #### Results ####
    tabPanel("Results", 
             
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Calculating...This will take ~15-20 min per state selected.",id="loadmessage")), #Warning for users
             ## KB - ADD PROGRESS BUTTON HERE
             
             downloadButton(outputId = "downloadData", "Download"),
             # Add table outputs
             ## KB - Make tables DTs - should fix RMD documentation issue
             tableOutput(outputId = "regtableout"),
             tableOutput(outputId = "welfare_tableout"),
             tableOutput(outputId = "keep_tableout"),
             tableOutput(outputId = "releaseout"),
             tableOutput(outputId = "ntrips_tableout"), 
             plotOutput(outputId = "fig")), 

  ))

####### Start Server ###################
server <- function(input, output, session) {
  
  library(magrittr) 
  
  ### Percent Change Approach
  sf_percent_change <- .72
  #bsb_percent_change <- 0
  scup_percent_change <- .9
  
  #### Toggle extra seasons on UI ####
  # Allows for extra seasons to show and hide based on click
  shinyjs::onclick("SFMAaddSeason",
                   shinyjs::toggle(id = "SFmaSeason2", anim = TRUE))
  shinyjs::onclick("BSBMAaddSeason",
                   shinyjs::toggle(id = "BSBmaSeason2", anim = TRUE))
  shinyjs::onclick("SCUPMAaddSeason",
                   shinyjs::toggle(id = "SCUPmaSeason2", anim = TRUE))
  
  shinyjs::onclick("SFRIaddSeason",
                   shinyjs::toggle(id = "SFriSeason2", anim = TRUE))
  shinyjs::onclick("BSBRIaddSeason",
                   shinyjs::toggle(id = "BSBriSeason3", anim = TRUE))
  shinyjs::onclick("SCUPRIaddSeason",
                   shinyjs::toggle(id = "SCUPriSeason2", anim = TRUE))
  
  shinyjs::onclick("SFCTaddSeason",
                   shinyjs::toggle(id = "SFctSeason3", anim = TRUE))
  shinyjs::onclick("BSBCTaddSeason",
                   shinyjs::toggle(id = "BSBctSeason2", anim = TRUE))
  shinyjs::onclick("SCUPCTaddSeason",
                   shinyjs::toggle(id = "SCUPctSeason2", anim = TRUE))
  
  shinyjs::onclick("SFNYaddSeason",
                   shinyjs::toggle(id = "SFnySeason3", anim = TRUE))
  shinyjs::onclick("BSBNYaddSeason",
                   shinyjs::toggle(id = "BSBnySeason3", anim = TRUE))
  shinyjs::onclick("SCUPNYaddSeason",
                   shinyjs::toggle(id = "SCUPnySeason2", anim = TRUE))
  
  shinyjs::onclick("SFNJaddSeason",
                   shinyjs::toggle(id = "SFnjSeason2", anim = TRUE))
  shinyjs::onclick("BSBNJaddSeason",
                   shinyjs::toggle(id = "BSBnjSeason5", anim = TRUE))
  shinyjs::onclick("SCUPNJaddSeason",
                   shinyjs::toggle(id = "SCUPnjSeason2", anim = TRUE))
  
  shinyjs::onclick("SFNJsplitaddSeason",
                   shinyjs::toggle(id = "SFnjsplitSeason2", anim = TRUE))
  shinyjs::onclick("BSBNJsplitaddSeason",
                   shinyjs::toggle(id = "BSBnjsplitSeason5", anim = TRUE))
  shinyjs::onclick("SCUPNJsplitaddSeason",
                   shinyjs::toggle(id = "SCUPnjsplitSeason2", anim = TRUE))
  
  shinyjs::onclick("SFDEaddSeason",
                   shinyjs::toggle(id = "SFdeSeason3", anim = TRUE))
  shinyjs::onclick("BSBDEaddSeason",
                   shinyjs::toggle(id = "BSBdeSeason3", anim = TRUE))
  shinyjs::onclick("SCUPDEaddSeason",
                   shinyjs::toggle(id = "SCUPdeSeason2", anim = TRUE))
  
  shinyjs::onclick("SFMDaddSeason",
                   shinyjs::toggle(id = "SFmdSeason3", anim = TRUE))
  shinyjs::onclick("BSBMDaddSeason",
                   shinyjs::toggle(id = "BSBmdSeason3", anim = TRUE))
  shinyjs::onclick("SCUPMDaddSeason",
                   shinyjs::toggle(id = "SCUPmdSeason2", anim = TRUE))
  
  shinyjs::onclick("SFVAaddSeason",
                   shinyjs::toggle(id = "SFvaSeason3", anim = TRUE))
  shinyjs::onclick("BSBVAaddSeason",
                   shinyjs::toggle(id = "BSBvaSeason3", anim = TRUE))
  shinyjs::onclick("SCUPVAaddSeason",
                   shinyjs::toggle(id = "SCUPvaSeason2", anim = TRUE))
  
  shinyjs::onclick("SFNCaddSeason",
                   shinyjs::toggle(id = "SFncSeason2", anim = TRUE))
  shinyjs::onclick("BSBNCaddSeason",
                   shinyjs::toggle(id = "BSBncSeason3", anim = TRUE))
  shinyjs::onclick("SCUPNCaddSeason",
                   shinyjs::toggle(id = "SCUPncSeason2", anim = TRUE))
  
  #### Output$addSTATE ####
  ############## MASSACHUSETTS ###########################################################
  output$addMA <- renderUI({
    if(any("MA" == input$state)){
      fluidRow( 
        style = "background-color: #FBB4AE;",
        column(4,
               titlePanel("Summer Flounder - MA"),
               sliderInput(inputId = "SFmaFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        sliderInput(inputId = "SFmaFH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 17.5, step = .5))),
               sliderInput(inputId = "SFmaPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        sliderInput(inputId = "SFmaPR_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 17.5, step = .5))),
               sliderInput(inputId = "SFmaSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        sliderInput(inputId = "SFmaSH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 16.5, step = .5))),
               
               actionButton("SFMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmaSeason2",
                                    sliderInput(inputId = "SFmaFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SFmaFH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SFmaPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SFmaPR_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SFmaSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SFmaSH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MA"),
               
               selectInput("BSB_MA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBmaMode"),
               
               
               actionButton("BSBMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmaSeason2",
                                    sliderInput(inputId = "BSBmaFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmaFH_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBmaPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmaPR_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBmaSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmaSH_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MA"),
               sliderInput(inputId = "SCUPmaFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("06-30","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaFH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPmaFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("07-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaFH_2_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))), 
               
               
               sliderInput(inputId = "SCUPmaPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaPR_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))),
               sliderInput(inputId = "SCUPmaSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaSH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 9.5, step = .5))),
               
               actionButton("SCUPMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmaSeason2",
                                    sliderInput(inputId = "SCUPmaFH_seas3", label ="For Hire Open Season 3", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmaFH_3_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPmaPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmaPR_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPmaSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmaSH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5)))))))
    }})
  
  
  ############# MA Breakout by mode ######################################
  output$SFmaMode <- renderUI({
    if (is.null(input$SF_MA_input_type))
      return()
    
    switch(input$SF_MA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFma_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFma_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 5)),
                                        column(6,
                                               sliderInput(inputId = "SFma_1_len", label ="Min Length",
                                                           min = 5, max = 25, value = 16.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFmaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              sliderInput(inputId = "SFmaFH_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "SFmaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              sliderInput(inputId = "SFmaPR_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "SFmaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              sliderInput(inputId = "SFmaSH_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5)))))
  })
  
  
  output$BSBmaMode <- renderUI({
    if (is.null(input$BSB_MA_input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component. i.e. when all modes combined is selected only one
    switch(input$BSB_MA_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "BSBma_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBma_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 4)), 
                                        column(6,
                                               sliderInput(inputId = "BSBma_1_len", label ="Min Length",
                                                           min = 3, max = 25, value = 16.5, step = .5)))),
           
           
           "Seperated By Mode" = div(sliderInput(inputId = "BSBmaFH_seas1", label =" For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              sliderInput(inputId = "BSBmaFH_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5))),
                                     sliderInput(inputId = "BSBmaPR_seas1", label ="Private/Rental Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              sliderInput(inputId = "BSBmaPR_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5))),
                                     sliderInput(inputId = "BSBmaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              sliderInput(inputId = "BSBmaSH_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5)))))
    
  })

  
  ############## RHODE ISLAND ###########################################################
  output$addRI <- renderUI({
    if(any("RI" == input$state)){
      fluidRow( 
        style = "background-color: #B3CDE3;",
        column(4,
               titlePanel("Summer Flounder - RI"),
               
               selectInput("SF_RI_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFriMode"),
               
               actionButton("SFRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFriSeason2",
                                    sliderInput(inputId = "SFriFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFriFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))), 
                                    sliderInput(inputId = "SFriPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFriPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))), 
                                    sliderInput(inputId = "SFriSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFriSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - RI"),
               
               sliderInput(inputId = "BSBriFH_seas1", label ="For Hire Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("06-18","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        sliderInput(inputId = "BSBriFH_1_len", label ="Min Length",
                                    min = 11, max =18, value = 16, step = .5))),
               sliderInput(inputId = "BSBriFH_seas2", label ="For Hire Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 6)),
                 column(6,
                        sliderInput(inputId = "BSBriFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBriPR_seas1", label ="Private Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        sliderInput(inputId = "BSBriPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               sliderInput(inputId = "BSBriPR_seas2", label ="Private Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 3)),
                 column(6,
                        sliderInput(inputId = "BSBriPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               sliderInput(inputId = "BSBriSH_seas1", label ="Shore Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        sliderInput(inputId = "BSBriSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               sliderInput(inputId = "BSBriSH_seas2", label ="Shore Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 3)),
                 column(6,
                        sliderInput(inputId = "BSBriSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               actionButton("BSBRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBriSeason2",
                                    sliderInput(inputId = "BSBriFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBriFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    sliderInput(inputId = "BSBriPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBriPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBriSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBriSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - RI"),
               sliderInput(inputId = "SCUPriFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPriFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPriFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               sliderInput(inputId = "SCUPriFH_seas3", label ="For Hire Open Season 3", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               sliderInput(inputId = "SCUPriPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               sliderInput(inputId = "SCUPriSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPriSeason2",
                                    sliderInput(inputId = "SCUPriFH_seas4", label ="For Hire Open Season 4", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPriFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPriPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPriPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPriSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPriSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# RI Breakout by mode ######################################
  output$SFriMode <- renderUI({
    if (is.null(input$SF_RI_input_type))
      return()
    
    switch(input$SF_RI_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFri_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFri_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               sliderInput(inputId = "SFri_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFriFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "SFriFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFriPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "SFriPR_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFriSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "SFriSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5)))))
  })
  
  ############## CONNECTICUT ###########################################################
  output$addCT <- renderUI({
    if(any("CT" == input$state)){
      fluidRow( 
        style = "background-color: #CCEBC5;",
        column(4,
               titlePanel("Summer Flounder - CT"),
               
               selectInput("SF_CT_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFctMode"),
               
               actionButton("SFCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFctSeason3",
                                    sliderInput(inputId = "SFctFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFctFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFctPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFctPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFctSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFctSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - CT"),
               
               sliderInput(inputId = "BSBctFH_seas1", label ="For Hire Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-18","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctFH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               sliderInput(inputId = "BSBctFH_seas2", label ="For Hire Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 7)),
                 column(6,
                        sliderInput(inputId = "BSBctFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctPR_seas1", label ="Private Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-18","%m-%d"),as.Date("06-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctPR_seas2", label ="Private Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("07-08","%m-%d"),as.Date("11-28","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctSH_seas1", label ="Shore Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-18","%m-%d"),as.Date("06-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctSH_seas2", label ="Shore Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("07-08","%m-%d"),as.Date("11-28","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               actionButton("BSBCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBctSeason2",
                                    sliderInput(inputId = "BSBctFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBctFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    sliderInput(inputId = "BSBctPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBctPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    sliderInput(inputId = "BSBctSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBctSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - CT"),
               sliderInput(inputId = "SCUPctFH_seas1", label ="For Hire Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPctFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPctFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               sliderInput(inputId = "SCUPctFH_seas3", label ="For Hire Open Season 3", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               sliderInput(inputId = "SCUPctPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               sliderInput(inputId = "SCUPctSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPctSeason2",
                                    sliderInput(inputId = "SCUPctFH_seas4", label ="For Hire Open Season 4", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPctFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPctPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPctPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPctSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPctSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# CT Breakout by mode ######################################
  output$SFctMode <- renderUI({
    if (is.null(input$SF_CT_input_type))
      return()
    
    switch(input$SF_CT_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFct_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFct_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      sliderInput(inputId = "SFct_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFct_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFctFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFctFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFctPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFctPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFctSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFctSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     sliderInput(inputId = "SFctFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFctFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFctPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFctPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFctSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 2)),
                                       column(6,
                                              sliderInput(inputId = "SFctSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
          
  })
  
  
  
  ############# NEW YORK #######################
  output$addNY <- renderUI({
    if(any("NY" == input$state)){
      fluidRow( 
        style = "background-color: #DECBE4;",
        column(4,
               titlePanel("Summer Flounder - NY"),
               
               selectInput("SF_NY_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFnyMode"),
               
               actionButton("SFNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnySeason3",
                                    sliderInput(inputId = "SFnyFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnyFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFnyPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnyPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFnySH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnySH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NY"),
               
               selectInput("BSB_NY_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBnyMode"),
               
               
               actionButton("BSBNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBnySeason3",
                                    sliderInput(inputId = "BSBnyFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnyFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBnyPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnyPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBnySH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnySH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NY"),
               sliderInput(inputId = "SCUPnyFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPnyFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               sliderInput(inputId = "SCUPnyFH_seas3", label ="For Hire Open Season 3", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               sliderInput(inputId = "SCUPnyPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               sliderInput(inputId = "SCUPnySH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnySH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnySH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnySeason2",
                                    sliderInput(inputId = "SCUPnyFH_seas4", label ="For Hire Open Season 4", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPnyFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnyPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPnyPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnySH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnySH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPnySH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  ############# NY Breakout by mode ######################################
  output$SFnyMode <- renderUI({
    if (is.null(input$SF_NY_input_type))
      return()
    
    switch(input$SF_NY_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFny_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFny_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      sliderInput(inputId = "SFny_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFny_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFnyFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFnyPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFnySH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnySH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     sliderInput(inputId = "SFnyFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFnyPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFnySH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnySH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
  })
  
  
  output$BSBnyMode <- renderUI({
    if (is.null(input$BSB_NY_input_type))
      return()
    
    switch(input$BSB_NY_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBny_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-23","%m-%d"),as.Date("8-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "BSBny_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBny_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               sliderInput(inputId = "BSBny_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBnyFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnyPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnySH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "BSBnySH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBnyFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnyPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnySH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "BSBnySH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5)))))
  })
  
  ############## NEW JERSEY ############################################################
  output$addNJ <- renderUI({
    if(any("NJ" == input$state)){
      fluidRow( 
        style = "background-color: #FED9A6;",
        column(4,
               titlePanel("Summer Flounder - NJ"),
               
               selectInput("SF_NJ_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFnjMode"),
               
               
               actionButton("SFNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnjSeason2",
                                    sliderInput(inputId = "SFnjFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),#)),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 7, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))),
                                    sliderInput(inputId = "SFnjPR_seas2", label ="Private/Rental Open Season 2",  
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5))),
                                    sliderInput(inputId = "SFnjSH_seas2", label ="Shore Open Season 2",  
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NJ"),
               
               selectInput("BSB_NJ_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBnjMode"),
               
               actionButton("BSBNJaddSeason", "Add Season"), 
               #Season 5
               shinyjs::hidden( div(ID = "BSBnjSeason5",
                                    sliderInput(inputId = "BSBnjFH_seas5", label =" For Hire Open Season 5", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjFH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjFH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjPR_seas5", label ="Private/Rental Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjPR_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjPR_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjSH_seas5", label ="Shore Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjSH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjSH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5)))))),
        
        
        
        
        column(4, 
               titlePanel("Scup - NJ"),
               
               selectInput("SCUP_NJ_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPnjMode"),
               
               actionButton("SCUPNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnjSeason2",
                                    sliderInput(inputId = "SCUPnjFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnjPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnjSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }
    
  })
  
  ############# NJ Breakout by mode ######################################
  output$SFnjMode <- renderUI({
    if (is.null(input$SF_NJ_input_type))
      return()
    
    switch(input$SF_NJ_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFnj_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4, 
                                               numericInput(inputId = "SFnj_1_bag", label ="Bag Limit", 
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFnj_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 18, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFnjFH_seas1", label ="For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjFH_1_bag", label ="Bag Limit", 
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnjFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5)), 
                                     sliderInput(inputId = "SFnjPR_seas1", label ="Private/Rental Open Season 1",  
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnjPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5))),
                                     sliderInput(inputId = "SFnjSH_seas1", label ="Shore Open Season 1",  
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)), 
                                       column(6,
                                              sliderInput(inputId = "SFnjSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5))))))
  })
  
  
  output$BSBnjMode <- renderUI({
    if (is.null(input$BSB_NJ_input_type))
      return()
    
    switch(input$BSB_NJ_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "BSBnj_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 2
                                      sliderInput(inputId = "BSBnj_seas2", label ="Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 1)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 3
                                      sliderInput(inputId = "BSBnj_seas3", label ="Open Season 3", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-01","%m-%d"),as.Date("10-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 4
                                      sliderInput(inputId = "BSBnj_seas4", label ="Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 15)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5)))),
           
           "Seperated By Mode" = div(sliderInput(inputId = "BSBnjFH_seas1", label =" For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas1", label ="Private/Rental Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 2
                                     sliderInput(inputId = "BSBnjFH_seas2", label =" For Hire Open Season 2", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas2", label ="Private/Rental Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 3
                                     sliderInput(inputId = "BSBnjFH_seas3", label =" For Hire Open Season 3", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas3", label ="Private/Rental Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas3", label ="Shore Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 4
                                     sliderInput(inputId = "BSBnjFH_seas4", label =" For Hire Open Season 4", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas4", label ="Private/Rental Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas4", label ="Shore Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5)))))
  })
  
  output$SCUPnjMode <- renderUI({
    if (is.null(input$SCUP_NJ_input_type))
      return()

    switch(input$SCUP_NJ_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "SCUPnj_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnj_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPnj_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 10, step = .5)))),
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPnjFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5)))))
  })
  
 
  output$SF_NJ_input_type_text <- renderText({
    input$SF_NJ_input_type
  })
  output$BSB_NJ_input_type_text <- renderText({
    input$BSB_NJ_input_type
  })
  output$SCUP_NJ_input_type_text <- renderText({
    input$SCUP_NJ_input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
  

  
  
  ############## NEW JERSEY  Split ############################################################
  output$addNJ_Split <- renderUI({
    if(any("NJSplit" == input$state)){
      
      ################################ New Jersey North ##############################################
      fluidRow( 
        style = "background-color: #FED9A6;",
        column(4,
               titlePanel("Summer Flounder - NJ North"),
               
               selectInput("SF_NJN_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFnjnMode"),
               
               
               actionButton("SFNJNaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnjnSeason2",
                                    sliderInput(inputId = "SFnjnFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),#)),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjnFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 7, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjnFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))),
                                    sliderInput(inputId = "SFnjnPR_seas2", label ="Private/Rental Open Season 2",  
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjnPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjsPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5))),
                                    sliderInput(inputId = "SFnjnSH_seas2", label ="Shore Open Season 2",  
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjnSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjnSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NJ North"),
               
               selectInput("BSB_NJN_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBnjnMode"),
               
               actionButton("BSBNJNaddSeason", "Add Season"), 
               #Season 5
               shinyjs::hidden( div(ID = "BSBnjsSeason5",
                                    sliderInput(inputId = "BSBnjnFH_seas5", label =" For Hire Open Season 5", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjnFH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjnFH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjnPR_seas5", label ="Private/Rental Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjnPR_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjnPR_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjnSH_seas5", label ="Shore Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjnSH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjnSH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5)))))),
        
        
        
        
        column(4, 
               titlePanel("Scup - NJ North"),
               
               selectInput("SCUP_NJN_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPnjnMode"),
               
               actionButton("SCUPNJNaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnjnSeason2",
                                    sliderInput(inputId = "SCUPnjnFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjnFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjnFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnjnPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjnPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjnPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnjnSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjnSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjnSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))),

  
  

  ########################## NJ South ###################################################################
  fluidRow(
        style = "background-color: #FED9A6;",
        column(4,
               titlePanel("Summer Flounder - NJ South"),

               selectInput("SF_NJS_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFnjsMode"),


               actionButton("SFNJSaddSeason", "Add Season"),
               shinyjs::hidden( div(ID = "SFnjsSeason2",
                                    sliderInput(inputId = "SFnjsFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),#)),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjsFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 7, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjsFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))),
                                    sliderInput(inputId = "SFnjsPR_seas2", label ="Private/Rental Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjsPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjsPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5))),
                                    sliderInput(inputId = "SFnjsSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjsSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjsSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5)))))),

        column(4,
               titlePanel("Black Sea Bass - NJ South"),

               selectInput("BSB_NJS_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBnjsMode"),

               actionButton("BSBNJSaddSeason", "Add Season"),
               #Season 5
               shinyjs::hidden( div(ID = "BSBnjsSeason5",
                                    sliderInput(inputId = "BSBnjsFH_seas5", label =" For Hire Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjsFH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnjsFH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjsPR_seas5", label ="Private/Rental Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjsPR_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnjsPR_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjsSH_seas5", label ="Shore Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjsSH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnjsSH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5)))))),




        column(4,
               titlePanel("Scup - NJ South"),

               selectInput("SCUP_NJS_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPnjsMode"),

               actionButton("SCUPNJSaddSeason", "Add Season"),
               shinyjs::hidden( div(ID = "SCUPnjsSeason2",
                                    sliderInput(inputId = "SCUPnjsFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjsFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjsFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))),
                                    sliderInput(inputId = "SCUPnjsPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjsPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjsPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))),
                                    sliderInput(inputId = "SCUPnjsSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")),
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjsSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjsSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }
    
  
  })
  
  
  ############# NJ North Breakout by mode ######################################
  output$SFnjnMode <- renderUI({
    if (is.null(input$SF_NJN_input_type))
      return()
    
    switch(input$SF_NJN_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFnjn_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4, 
                                               numericInput(inputId = "SFnjn_1_bag", label ="Bag Limit", 
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFnjn_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 18, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFnjnFH_seas1", label ="For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjnFH_1_bag", label ="Bag Limit", 
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnjnFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5)), 
                                       sliderInput(inputId = "SFnjnPR_seas1", label ="Private/Rental Open Season 1",  
                                                   min = as.Date("01-01","%m-%d"),
                                                   max = as.Date("12-31","%m-%d"),
                                                   value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                   timeFormat = "%m-%d", ticks = FALSE),
                                       fluidRow(
                                         column(4, 
                                                numericInput(inputId = "SFnjnPR_1_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 3)),
                                         column(6,
                                                sliderInput(inputId = "SFnjnPR_1_len", label ="Min Length",
                                                            min = 14, max = 21, value = 18, step = .5))),
                                       sliderInput(inputId = "SFnjnSH_seas1", label ="Shore Open Season 1",  
                                                   min = as.Date("01-01","%m-%d"),
                                                   max = as.Date("12-31","%m-%d"),
                                                   value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                   timeFormat = "%m-%d", ticks = FALSE),
                                       fluidRow(
                                         column(4, 
                                                numericInput(inputId = "SFnjnSH_1_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 3)), 
                                         column(6,
                                                sliderInput(inputId = "SFnjnSH_1_len", label ="Min Length",
                                                            min = 14, max = 21, value = 18, step = .5))))))
  })
  
  
  output$BSBnjnMode <- renderUI({
    if (is.null(input$BSB_NJN_input_type))
      return()
    
    switch(input$BSB_NJN_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "BSBnjn_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjn_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjn_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 2
                                      sliderInput(inputId = "BSBnjn_seas2", label ="Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjn_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 1)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjn_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 3
                                      sliderInput(inputId = "BSBnjn_seas3", label ="Open Season 3", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-01","%m-%d"),as.Date("10-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjn_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjn_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 4
                                      sliderInput(inputId = "BSBnjn_seas4", label ="Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjn_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 15)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjn_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5)))),
           
           "Seperated By Mode" = div(sliderInput(inputId = "BSBnjnFH_seas1", label =" For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnPR_seas1", label ="Private/Rental Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 2
                                     sliderInput(inputId = "BSBnjnFH_seas2", label =" For Hire Open Season 2", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnPR_seas2", label ="Private/Rental Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 3
                                     sliderInput(inputId = "BSBnjnFH_seas3", label =" For Hire Open Season 3", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnFH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnFH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnPR_seas3", label ="Private/Rental Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnPR_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnPR_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnSH_seas3", label ="Shore Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnSH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnSH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 4
                                     sliderInput(inputId = "BSBnjnFH_seas4", label =" For Hire Open Season 4", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnFH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnFH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnPR_seas4", label ="Private/Rental Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnPR_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnPR_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjnSH_seas4", label ="Shore Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjnSH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjnSH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5)))))
  })
  
  output$SCUPnjnMode <- renderUI({
    if (is.null(input$SCUP_NJN_input_type))
      return()
    
    switch(input$SCUP_NJN_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "SCUPnjn_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnjn_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPnjn_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 10, step = .5)))),
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPnjnFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjnFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjnFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjnPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjnPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjnPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjnSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjnSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjnSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5)))))
  })
  
  ############# NJ South  Breakout by mode ######################################
  output$SFnjsMode <- renderUI({
    if (is.null(input$SF_NJS_input_type))
      return()
    
    switch(input$SF_NJS_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFnjs_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4, 
                                               numericInput(inputId = "SFnjs_1_bag", label ="Bag Limit", 
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFnjs_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 18, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFnjsFH_seas1", label ="For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjsFH_1_bag", label ="Bag Limit", 
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnjsFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5)), 
                                       sliderInput(inputId = "SFnjsPR_seas1", label ="Private/Rental Open Season 1",  
                                                   min = as.Date("01-01","%m-%d"),
                                                   max = as.Date("12-31","%m-%d"),
                                                   value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                   timeFormat = "%m-%d", ticks = FALSE),
                                       fluidRow(
                                         column(4, 
                                                numericInput(inputId = "SFnjsPR_1_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 3)),
                                         column(6,
                                                sliderInput(inputId = "SFnjsPR_1_len", label ="Min Length",
                                                            min = 14, max = 21, value = 18, step = .5))),
                                       sliderInput(inputId = "SFnjsSH_seas1", label ="Shore Open Season 1",  
                                                   min = as.Date("01-01","%m-%d"),
                                                   max = as.Date("12-31","%m-%d"),
                                                   value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                   timeFormat = "%m-%d", ticks = FALSE),
                                       fluidRow(
                                         column(4, 
                                                numericInput(inputId = "SFnjsSH_1_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 3)), 
                                         column(6,
                                                sliderInput(inputId = "SFnjsSH_1_len", label ="Min Length",
                                                            min = 14, max = 21, value = 18, step = .5))))))
  })
  
  
  output$BSBnjsMode <- renderUI({
    if (is.null(input$BSB_NJS_input_type))
      return()
    
    switch(input$BSB_NJS_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "BSBnjs_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjs_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjs_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 2
                                      sliderInput(inputId = "BSBnjs_seas2", label ="Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjs_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 1)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjs_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 3
                                      sliderInput(inputId = "BSBnjs_seas3", label ="Open Season 3", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-01","%m-%d"),as.Date("10-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjs_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjs_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 4
                                      sliderInput(inputId = "BSBnjs_seas4", label ="Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjs_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 15)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnjs_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5)))),
           
           "Seperated By Mode" = div(sliderInput(inputId = "BSBnjsFH_seas1", label =" For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsPR_seas1", label ="Private/Rental Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 2
                                     sliderInput(inputId = "BSBnjsFH_seas2", label =" For Hire Open Season 2", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsPR_seas2", label ="Private/Rental Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 3
                                     sliderInput(inputId = "BSBnjsFH_seas3", label =" For Hire Open Season 3", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsFH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsFH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsPR_seas3", label ="Private/Rental Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsPR_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsPR_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsSH_seas3", label ="Shore Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsSH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsSH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 4
                                     sliderInput(inputId = "BSBnjsFH_seas4", label =" For Hire Open Season 4", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsFH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsFH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsPR_seas4", label ="Private/Rental Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsPR_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsPR_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjsSH_seas4", label ="Shore Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjsSH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjsSH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5)))))
  })
  
  output$SCUPnjsMode <- renderUI({
    if (is.null(input$SCUP_NJS_input_type))
      return()
    
    switch(input$SCUP_NJS_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "SCUPnjs_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnjs_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPnjs_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 10, step = .5)))),
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPnjsFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjsFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjsFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjsPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjsPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjsPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjsSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjsSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjsSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5)))))
  })
  
  

  
  
  
  
  
  
  
  
  
  output$SF_NJN_input_type_text <- renderText({
    input$SF_NJN_input_type
  })
  output$BSB_NJN_input_type_text <- renderText({
    input$BSB_NJN_input_type
  })
  output$SCUP_NJN_input_type_text <- renderText({
    input$SCUP_NJN_input_type
  })
  
  
  output$SF_NJS_input_type_text <- renderText({
    input$SF_NJS_input_type
  })
  output$BSB_NJS_input_type_text <- renderText({
    input$BSB_NJS_input_type
  })
  output$SCUP_NJS_input_type_text <- renderText({
    input$SCUP_NJS_input_type
  })
  
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
  
  
  
  
  
  ############## DELEWARE ###########################################################
  output$addDE <- renderUI({
    if(any("DE" == input$state)){
      fluidRow( 
        style = "background-color: #FFFFCC;",
        column(4,
               titlePanel("Summer Flounder - DE"),
               
               selectInput("SF_DE_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFdeMode"),
               
               actionButton("SFDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFdeSeason3",
                                    sliderInput(inputId = "SFdeFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFdeFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFdePR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFdePR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFdeSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFdeSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - DE"),
               
               selectInput("BSB_DE_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBdeMode"),
               
               
               actionButton("BSBDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBdeSeason3",
                                    sliderInput(inputId = "BSBdeFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBdeFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBdePR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBdePR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBdeSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBdeSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - DE"),
               
               selectInput("SCUP_DE_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPdeMode"),
               
               actionButton("SCUPDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPdeSeason2",
                                    sliderInput(inputId = "SCUPdeFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPdeFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPdePR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdePR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPdePR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPdeSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPdeSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## DE breakout by mode ############################
  
  output$SFdeMode <- renderUI({
    if (is.null(input$SF_DE_input_type))
      return()
    
    switch(input$SF_DE_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFde_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFde_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      sliderInput(inputId = "SFde_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFde_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFdeFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFdePR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdePR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFdeSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     
                                     sliderInput(inputId = "SFdeFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFdePR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdePR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFdeSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBdeMode <- renderUI({
    if (is.null(input$BSB_DE_input_type))
      return()
    
    switch(input$BSB_DE_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBde_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBde_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBde_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBde_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBdeFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBdePR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdePR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBdeSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBdeFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBdePR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdePR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBdeSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPdeMode <- renderUI({
    if (is.null(input$SCUP_DE_input_type))
      return()
    
    switch(input$SCUP_DE_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPde_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPde_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPdeFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPdeFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPdePR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPdePR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPdeSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPdeSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ############## MARYLAND ###########################################################
  output$addMD <- renderUI({
    if(any("MD" == input$state)){
      fluidRow( 
        style = "background-color: #E5D8BD;",
        column(4,
               titlePanel("Summer Flounder - MD"),
               
               selectInput("SF_MD_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFmdMode"),
               
               actionButton("SFMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmdSeason3",
                                    sliderInput(inputId = "SFmdFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFmdFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFmdPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFmdPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFmdSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFmdSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MD"),
               
               selectInput("BSB_MD_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBmdMode"),
               
               
               actionButton("BSBMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmdSeason3",
                                    sliderInput(inputId = "BSBmdFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmdFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBmdPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmdPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBmdSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmdSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MD"),
               
               selectInput("SCUP_MD_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPmdMode"),
               
               actionButton("SCUPMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmdSeason2",
                                    sliderInput(inputId = "SCUPmdFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmdFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPmdPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmdPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPmdSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmdSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## MD breakout by mode ############################
  
  output$SFmdMode <- renderUI({
    if (is.null(input$SF_MD_input_type))
      return()
    
    switch(input$SF_MD_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFmd_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFmd_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      sliderInput(inputId = "SFmd_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFmd_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFmdFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFmdPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFmdSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     sliderInput(inputId = "SFmdFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFmdPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFmdSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))
                                     ))
  })
  
  
  output$BSBmdMode <- renderUI({
    if (is.null(input$BSB_MD_input_type))
      return()
    
    switch(input$BSB_MD_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBmd_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBmd_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBmd_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBmd_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBmdFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBmdPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBmdSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBmdFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBmdPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBmdSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPmdMode <- renderUI({
    if (is.null(input$SCUP_MD_input_type))
      return()
    
    switch(input$SCUP_MD_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPmd_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 40)),
                                        column(6,
                                               sliderInput(inputId = "SCUPmd_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPmdFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 40)),
                                       column(6,
                                              sliderInput(inputId = "SCUPmdFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPmdPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 40)),
                                       column(6,
                                              sliderInput(inputId = "SCUPmdPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPmdSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 40)),
                                       column(6,
                                              sliderInput(inputId = "SCUPmdSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ############## VIRGINIA ###########################################################
  output$addVA <- renderUI({
    if(any("VA" == input$state)){
      fluidRow( 
        style = "background-color: #FDDAEC;",
        column(4,
               titlePanel("Summer Flounder - VA"),
               
               selectInput("SF_VA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFvaMode"),
               
               actionButton("SFVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFvaSeason3",
                                    sliderInput(inputId = "SFvaFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFvaFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFvaPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFvaPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFvaSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFvaSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - VA"),
               
               selectInput("BSB_VA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBvaMode"),
               
               
               actionButton("BSBVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBvaSeason3",
                                    sliderInput(inputId = "BSBvaFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBvaFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBvaPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBvaPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBvaSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBvaSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - VA"),
               
               selectInput("SCUP_VA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPvaMode"),
               
               actionButton("SCUPVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPvaSeason2",
                                    sliderInput(inputId = "SCUPvaFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPvaFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPvaPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPvaPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPvaSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPvaSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## VA breakout by mode ############################
  
  output$SFvaMode <- renderUI({
    if (is.null(input$SF_VA_input_type))
      return()
    
    switch(input$SF_VA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFva_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFva_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      sliderInput(inputId = "SFva_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFva_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFvaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFvaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFvaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))),
                                     sliderInput(inputId = "SFvaFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFvaPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFvaSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBvaMode <- renderUI({
    if (is.null(input$BSB_VA_input_type))
      return()
    
    switch(input$BSB_VA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBva_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBva_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBva_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBva_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBvaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBvaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBvaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBvaFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBvaPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBvaSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPvaMode <- renderUI({
    if (is.null(input$SCUP_VA_input_type))
      return()
    
    switch(input$SCUP_VA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPva_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPva_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPvaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPvaFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPvaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPvaPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPvaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPvaSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  ############## NORTH CAROLINA ###########################################################
  output$addNC <- renderUI({
    if(any("NC" == input$state)){
      fluidRow( 
        style = "background-color: #F2F2F2;",
        column(4,
               titlePanel("Summer Flounder - NC"),
               
               selectInput("SF_NC_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFncMode"),
               
               actionButton("SFNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFncSeason2",
                                    sliderInput(inputId = "SFncFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFncFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    sliderInput(inputId = "SFncPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFncPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    sliderInput(inputId = "SFncSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFncSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NC"),
               
               selectInput("BSB_NC_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBncMode"),
               
               
               actionButton("BSBNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBncSeason3",
                                    sliderInput(inputId = "BSBncFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBncFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBncPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBncPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBncSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBncSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NC"),
               
               selectInput("SCUP_NC_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPncMode"),
               
               actionButton("SCUPNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPncSeason2",
                                    sliderInput(inputId = "SCUPncFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPncFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPncPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPncPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPncSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPncSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## NC breakout by mode ############################
  
  output$SFncMode <- renderUI({
    if (is.null(input$SF_NC_input_type))
      return()
    
    switch(input$SF_NC_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFnc_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 1)),
                                        column(6,
                                               sliderInput(inputId = "SFnc_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 15, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFncFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId = "SFncFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     sliderInput(inputId = "SFncPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId = "SFncPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     sliderInput(inputId = "SFncSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId = "SFncSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5)))))
  })
  
  
  output$BSBncMode <- renderUI({
    if (is.null(input$BSB_NC_input_type))
      return()
    
    switch(input$BSB_NC_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBnc_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBnc_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBnc_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBnc_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBncFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBncPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBncSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBncFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBncPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBncSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPncMode <- renderUI({
    if (is.null(input$SCUP_NC_input_type))
      return()
    
    switch(input$SCUP_NC_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPnc_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPnc_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPncFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPncFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPncPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPncPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPncSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPncSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ##### Start of output processing #####
  #### PREDICTIONS_1 ##############################
  predictions_1 <- eventReactive(input$runmeplease,{
    
    predictions_1 <- NULL
    
    #if(any( )) will run all selected check boxes on UI-regulations selection tab
    if(any("MA" == input$state)){
      source(here::here(paste0("recDST/model_run_MA.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("RI" == input$state)){
      source(here::here(paste0("recDST/model_run_RI.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("CT" == input$state)){
      source(here::here(paste0("recDST/model_run_CT.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("NY" == input$state)){
      source(here::here(paste0("recDST/model_run_NY.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("NJ" == input$state)){
      source(here::here(paste0("recDST/model_run_NJ.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("DE" == input$state)){
      source(here::here(paste0("recDST/model_run_DE.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("MD" == input$state)){
      source(here::here(paste0("recDST/model_run_MD.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("VA" == input$state)){
      source(here::here(paste0("recDST/model_run_VA.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    if(any("NC" == input$state)){
      source(here::here(paste0("recDST/model_run_NC.R")), local = TRUE)
      predictions_1 <- predictions_1 %>% rbind(predictions)
    }
    
    predictions_coastwide<- predictions_1 %>% 
      dplyr::filter(draw == "Summary") %>% 
      dplyr::group_by(Statistic, Mode, Species, draw) %>%
      dplyr::summarise(`Status-quo value (median)` = sum(as.numeric(`Status-quo value (median)`)), 
                       `Alternative option value` = sum(as.numeric(`Alternative option value`))) %>% 
      dplyr::mutate(State = "All selected", 
                    `% difference from status-quo outcome (median)` = dplyr::case_when(Statistic != "CV" ~ (as.numeric(`Alternative option value`) - as.numeric(`Status-quo value (median)`))/ as.numeric(`Status-quo value (median)`), 
                                                                                     TRUE ~ as.numeric(`Alternative option value`) - as.numeric(`Status-quo value (median)`)),
                    `% under harvest target (out of 100 simulations)` = "NA", 
                    `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
                    `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
                    `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
  
    predictions_1 <- predictions_1 %>% rbind(predictions_coastwide)
    
    return(predictions_1)
  
  })
  
  #### keep ####
  keep <- reactive({
    keep_output<- predictions_1() %>% 
      dplyr::filter(draw %in% c("Summary", "All selected"), 
                    Statistic %in% c("harvest pounds", "harvest numbers")) %>% 
      dplyr::arrange(factor(Statistic, levels = c("harvest pounds", "harvest numbers"))) %>% 
      dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", scientific = FALSE),
                     `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
                     `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
    return(keep_output)
  })
  
  #### release ####
  release<- reactive({
    release_output<- predictions_1() %>% 
      dplyr::filter(draw %in% c("Summary", "All selected"),
                    Statistic %in% c("release pounds", "dead release pounds", 
                                     "release numbers", "dead release numbers")) %>% 
      dplyr::select(! "% under harvest target (out of 100 simulations)") %>% 
      dplyr::arrange(factor(Statistic, levels = c("release pounds", "release numbers", "dead release pounds", "dead release numbers"))) %>% 
      dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", scientific = FALSE),
                     `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
                     `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
    return(release_output)
  })
  
  #### angler welfare ####
  welfare<- reactive({
    welfare_output<- predictions_1() %>% 
      dplyr::filter(draw %in% c("Summary", "All selected"),
                    Statistic %in% c("CV")) %>% 
      dplyr::mutate(Statistic = dplyr::recode(Statistic, "CV" = "Change in angler satisfaction ($)")) %>% 
      dplyr::rename( "Difference relative to status-quo 2024 (median)" = "% difference from status-quo outcome (median)" ) %>% 
      dplyr::select(!c("% under harvest target (out of 100 simulations)","Status-quo value (median)","Alternative option value" )) %>% 
      #dplyr::mutate(`Difference relative to status-quo 2024 (median)` = dplyr::case_when(draw != "All selected" ~ as.numeric(`Difference relative to status-quo 2024 (median)`), TRUE ~ `Difference relative to status-quo 2024 (median)`)) %>% 
      dplyr::mutate( `Difference relative to status-quo 2024 (median)` = prettyNum(`Difference relative to status-quo 2024 (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE)) 
      
    return(welfare_output)
  })
  
  #### estimate trips ####
  ntrips<- reactive({
    ntrips_output<- predictions_1() %>% 
      dplyr::filter(draw %in% c("Summary", "All selected"),
                    Statistic %in% c( "ntrips")) %>% 
      dplyr::mutate(Statistic = dplyr::recode(Statistic, "ntrips" = "Total estimate trips")) %>% 
      dplyr::select(!c("% under harvest target (out of 100 simulations)","Status-quo value (median)","% difference from status-quo outcome (median)")) %>% 
      #dplyr::mutate(`Alternative option value` = dplyr::case_when(draw != "All selected" ~ as.numeric(`Alternative option value`), TRUE ~`Alternative option value`)) %>% 
      dplyr::mutate(`Alternative option value` = prettyNum(as.numeric(`Alternative option value`), big.mark = ",", format = "f", digits = 2,  scientific = FALSE)) 
      
    return(ntrips_output)
  })
  
  ## ALL DRAWS
  #### keep ####
  keep_draws <- reactive({
    keep_draws_output<- predictions_1() %>% 
      dplyr::filter(draw != "Summary", 
                    Statistic %in% c("harvest pounds", "harvest numbers")) %>% 
      dplyr::arrange(factor(Statistic, levels = c("harvest pounds", "harvest numbers"))) %>% 
      dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
                     `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
                     `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
    return(keep_draws_output)
  })
  
  #### release ####
  release_draws<- reactive({
    release_draws_output<- predictions_1() %>% 
      dplyr::filter(draw != "Summary",
                    Statistic %in% c("release pounds", "dead release pounds", 
                                     "release numbers", "dead release numbers")) %>% 
      dplyr::select(! "% under harvest target (out of 100 simulations)") %>% 
      dplyr::arrange(factor(Statistic, levels = c("release pounds", "release numbers", "dead release pounds", "dead release numbers"))) %>% 
      dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
                     `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
                     `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
    return(release_draws_output)
  })
  
  #### angler welfare ####
  welfare_draws<- reactive({
    welfare_draws_output<- predictions_1() %>% 
      dplyr::filter(draw != "Summary",
                    Statistic %in% c("CV")) %>% 
      dplyr::mutate(Statistic = dplyr::recode(Statistic, "CV" = "Change in angler satisfaction ($)")) %>% 
      dplyr::rename( "Difference relative to status-quo 2024 (median)" = "% difference from status-quo outcome (median)" ) %>%
      dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
                     `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
                     `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE)) %>% 
      dplyr::select(! c("% under harvest target (out of 100 simulations)","Status-quo value (median)","Alternative option value" ))
    return(welfare_draws_output)
  })
  
  #### estimate trips ####
  ntrips_draws<- reactive({
    ntrips_draws_output<- predictions %>% 
      dplyr::filter(draw != "Summary",
                    Statistic %in% c( "ntrips")) %>% 
      dplyr::mutate(Statistic = dplyr::recode(Statistic, "ntrips" = "Total estimate trips")) %>% 
      dplyr::mutate(`Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", format = "f", digits = 2, scientific = FALSE)) %>% 
      dplyr::select(! c("% under harvest target (out of 100 simulations)","Status-quo value (median)","% difference from status-quo outcome (median)"))
    return(ntrips_draws_output)
  })
   
  
  #### Regulations ####
  regulations <- eventReactive(input$runmeplease,{
    
    dat <- NULL
    #### MA regs ####
    if(any("MA" == input$state)){  
      SFmaFHseason1 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFmaFH_seas1[1], "-", input$SFmaFH_seas1[2]),
                                    BagLimit = paste(input$SFmaFH_1_bag),
                                    Length = paste(input$SFmaFH_1_len))
      SFmaPRseason1 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFmaPR_seas1[1], "-", input$SFmaPR_seas1[2]),
                                    BagLimit = paste(input$SFmaPR_1_bag),
                                    Length = paste(input$SFmaPR_1_len))
      SFmaSHseason1 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFmaSH_seas1[1], "-", input$SFmaSH_seas1[2]),
                                    BagLimit = paste(input$SFmaSH_1_bag),
                                    Length = paste(input$SFmaSH_1_len))
      
      SFmaFHseason2 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFmaFH_seas2[1], "-", input$SFmaFH_seas2[2]),
                                    BagLimit = paste(input$SFmaFH_2_bag),
                                    Length = paste(input$SFmaFH_2_len))
      SFmaPRseason2 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFmaPR_seas2[1], "-", input$SFmaPR_seas2[2]),
                                    BagLimit = paste(input$SFmaPR_2_bag),
                                    Length = paste(input$SFmaPR_2_len))
      SFmaSHseason2 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFmaSH_seas2[1], "-", input$SFmaSH_seas2[2]),
                                    BagLimit = paste(input$SFmaSH_2_bag),
                                    Length = paste(input$SFmaSH_2_len))

      
      SFma <- rbind(SFmaFHseason1, SFmaFHseason2, SFmaPRseason1,  SFmaPRseason2, SFmaSHseason1,  SFmaSHseason2)
      
      if(input$BSB_MA_input_type == "All Modes Combined"){
        BSBmaseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBma_seas1[1], "-", input$BSBma_seas1[2]),
                                   BagLimit = paste(input$BSBma_1_bag),
                                   Length = paste(input$BSBma_1_len))
        BSBmaFHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmaFH_seas2[1], "-", input$BSBmaFH_seas2[2]),
                                     BagLimit = paste(input$BSBmaFH_2_bag),
                                     Length = paste(input$BSBmaFH_2_len))
        BSBmaPRseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmaPR_seas2[1], "-", input$BSBmaPR_seas2[2]),
                                     BagLimit = paste(input$BSBmaPR_2_bag),
                                     Length = paste(input$BSBmaPR_2_len))
        BSBmaSHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmaSH_seas2[1], "-", input$BSBmaSH_seas2[2]),
                                     BagLimit = paste(input$BSBmaSH_2_bag),
                                     Length = paste(input$BSBmaSH_2_len))
        
        BSBma<- rbind(BSBmaseason1, BSBmaFHseason2, BSBmaPRseason2, BSBmaSHseason2)
      }else{
        
        BSBmaFHseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmaFH_seas1[1], "-", input$BSBmaFH_seas1[2]),
                                     BagLimit = paste(input$BSBmaFH_1_bag),
                                     Length = paste(input$BSBmaFH_1_len))
        BSBmaFHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmaFH_seas2[1], "-", input$BSBmaFH_seas2[2]),
                                     BagLimit = paste(input$BSBmaFH_2_bag),
                                     Length = paste(input$BSBmaFH_2_len))
        
        
        BSBmaPRseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmaPR_seas1[1], "-", input$BSBmaPR_seas1[2]),
                                     BagLimit = paste(input$BSBmaPR_1_bag),
                                     Length = paste(input$BSBmaPR_1_len))
        BSBmaPRseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmaPR_seas2[1], "-", input$BSBmaPR_seas2[2]),
                                     BagLimit = paste(input$BSBmaPR_2_bag),
                                     Length = paste(input$BSBmaPR_2_len))
        
        
        BSBmaSHseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmaSH_seas1[1], "-", input$BSBmaSH_seas1[2]),
                                     BagLimit = paste(input$BSBmaSH_1_bag),
                                     Length = paste(input$BSBmaSH_1_len))
        BSBmaSHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmaSH_seas2[1], "-", input$BSBmaSH_seas2[2]),
                                     BagLimit = paste(input$BSBmaSH_2_bag),
                                     Length = paste(input$BSBmaSH_2_len))
        
        BSBma<- rbind(BSBmaFHseason1, BSBmaFHseason2, BSBmaPRseason1, BSBmaPRseason2,BSBmaSHseason1, BSBmaSHseason2)
      }
      
      
      
      
      SCUPmaFHseason1 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPmaFH_seas1[1], "-", input$SCUPmaFH_seas1[2]),
                                    BagLimit = paste(input$SCUPmaFH_1_bag),
                                    Length = paste(input$SCUPmaFH_1_len))
      SCUPmaPRseason1 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPmaPR_seas1[1], "-", input$SCUPmaPR_seas1[2]),
                                    BagLimit = paste(input$SCUPmaPR_1_bag),
                                    Length = paste(input$SCUPmaPR_1_len))
      SCUPmaSHseason1 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPmaSH_seas1[1], "-", input$SCUPmaSH_seas1[2]),
                                    BagLimit = paste(input$SCUPmaSH_1_bag),
                                    Length = paste(input$SCUPmaSH_1_len))
      
      SCUPmaFHseason2 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPmaFH_seas2[1], "-", input$SCUPmaFH_seas2[2]),
                                    BagLimit = paste(input$SCUPmaFH_2_bag),
                                    Length = paste(input$SCUPmaFH_2_len))
      SCUPmaPRseason2 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPmaPR_seas2[1], "-", input$SCUPmaPR_seas2[2]),
                                    BagLimit = paste(input$SCUPmaPR_2_bag),
                                    Length = paste(input$SCUPmaPR_2_len))
      SCUPmaSHseason2 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPmaSH_seas2[1], "-", input$SCUPmaSH_seas2[2]),
                                    BagLimit = paste(input$SCUPmaSH_2_bag),
                                    Length = paste(input$SCUPmaSH_2_len))
      
      SCUPmaFHseason3 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPmaFH_seas3[1], "-", input$SCUPmaFH_seas3[2]),
                                    BagLimit = paste(input$SCUPmaFH_3_bag),
                                    Length = paste(input$SCUPmaFH_3_len))
      
      SCUPma <- rbind(SCUPmaFHseason1, SCUPmaFHseason2, SCUPmaFHseason3 ,SCUPmaPRseason1,  SCUPmaPRseason2,SCUPmaSHseason1,  SCUPmaSHseason2)
      
      dat <- dat %>% rbind(SFma, BSBma, SCUPma)
      
    }
    
    #### RI regs ####
    if(any("RI" == input$state)){  
      if(input$SF_RI_input_type == "All Modes Combined"){
        
        SFriseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFri_seas1[1], "-", input$SFri_seas1[2]),
                                  BagLimit = paste(input$SFri_1_bag),
                                  Length = paste(input$SFri_1_len))
        SFriFHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFriFH_seas2[1], "-", input$SFriFH_seas2[2]),
                                    BagLimit = paste(input$SFriFH_2_bag),
                                    Length = paste(input$SFriFH_2_len))
        SFriPRseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFriPR_seas2[1], "-", input$SFriPR_seas2[2]),
                                    BagLimit = paste(input$SFriPR_2_bag),
                                    Length = paste(input$SFriPR_2_len))
        SFriSHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFriSH_seas2[1], "-", input$SFriSH_seas2[2]),
                                    BagLimit = paste(input$SFriSH_2_bag),
                                    Length = paste(input$SFriSH_2_len))
        SFri <- rbind(SFriseason1, SFriFHseason2, SFriPRseason2, SFriSHseason2)
      } else {
        SFriFHseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFriFH_seas1[1], "-", input$SFriFH_seas1[2]),
                                    BagLimit = paste(input$SFriFH_1_bag),
                                    Length = paste(input$SFriFH_1_len))
        SFriPRseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFriPR_seas1[1], "-", input$SFriPR_seas1[2]),
                                    BagLimit = paste(input$SFriPR_1_bag),
                                    Length = paste(input$SFriPR_1_len))
        SFriSHseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFriSH_seas1[1], "-", input$SFriSH_seas1[2]),
                                    BagLimit = paste(input$SFriSH_1_bag),
                                    Length = paste(input$SFriSH_1_len))
        SFriFHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFriFH_seas2[1], "-", input$SFriFH_seas2[2]),
                                    BagLimit = paste(input$SFriFH_2_bag),
                                    Length = paste(input$SFriFH_2_len))
        SFriPRseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFriPR_seas2[1], "-", input$SFriPR_seas2[2]),
                                    BagLimit = paste(input$SFriPR_2_bag),
                                    Length = paste(input$SFriPR_2_len))
        SFriSHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFriSH_seas2[1], "-", input$SFriSH_seas2[2]),
                                    BagLimit = paste(input$SFriSH_2_bag),
                                    Length = paste(input$SFriSH_2_len))
        
        SFri <- rbind(SFriFHseason1,SFriFHseason2, SFriPRseason1,SFriPRseason2, SFriSHseason1,SFriSHseason2)
      }
      
      
      BSBriFHseason1 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(input$BSBriFH_seas1[1], "-", input$BSBriFH_seas1[2]),
                                   BagLimit = paste(input$BSBriFH_1_bag),
                                   Length = paste(input$BSBriFH_1_len))
      BSBriFHseason2 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(input$BSBriFH_seas2[1], "-", input$BSBriFH_seas2[2]),
                                   BagLimit = paste(input$BSBriFH_2_bag),
                                   Length = paste(input$BSBriFH_2_len))
      BSBriFHseason3 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(input$BSBriFH_seas3[1], "-", input$BSBriFH_seas3[2]),
                                   BagLimit = paste(input$BSBriFH_3_bag),
                                   Length = paste(input$BSBriFH_3_len))
      
      BSBriPRseason1 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(input$BSBriPR_seas1[1], "-", input$BSBriPR_seas1[2]),
                                   BagLimit = paste(input$BSBriPR_1_bag),
                                   Length = paste(input$BSBriPR_1_len))
      BSBriPRseason2 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(input$BSBriPR_seas2[1], "-", input$BSBriPR_seas2[2]),
                                   BagLimit = paste(input$BSBriPR_2_bag),
                                   Length = paste(input$BSBriPR_2_len))
      BSBriPRseason3 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(input$BSBriPR_seas3[1], "-", input$BSBriPR_seas3[2]),
                                   BagLimit = paste(input$BSBriPR_3_bag),
                                   Length = paste(input$BSBriPR_3_len))
      
      BSBriSHseason1 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(input$BSBriSH_seas1[1], "-", input$BSBriSH_seas1[2]),
                                   BagLimit = paste(input$BSBriSH_1_bag),
                                   Length = paste(input$BSBriSH_1_len))
      BSBriSHseason2 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(input$BSBriSH_seas2[1], "-", input$BSBriSH_seas2[2]),
                                   BagLimit = paste(input$BSBriSH_2_bag),
                                   Length = paste(input$BSBriSH_2_len))
      BSBriSHseason3 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(input$BSBriSH_seas3[1], "-", input$BSBriSH_seas3[2]),
                                   BagLimit = paste(input$BSBriSH_3_bag),
                                   Length = paste(input$BSBriSH_3_len))
      
      BSBri<- rbind(BSBriFHseason1, BSBriFHseason2, BSBriFHseason3,BSBriPRseason1, BSBriPRseason2,BSBriPRseason3,BSBriSHseason1, BSBriSHseason2, BSBriSHseason3)
      
      
      
      
      
      SCUPriFHseason1 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPriFH_seas1[1], "-", input$SCUPriFH_seas1[2]),
                                    BagLimit = paste(input$SCUPriFH_1_bag),
                                    Length = paste(input$SCUPriFH_1_len))
      SCUPriPRseason1 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPriPR_seas1[1], "-", input$SCUPriPR_seas1[2]),
                                    BagLimit = paste(input$SCUPriPR_1_bag),
                                    Length = paste(input$SCUPriPR_1_len))
      SCUPriSHseason1 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPriSH_seas1[1], "-", input$SCUPriSH_seas1[2]),
                                    BagLimit = paste(input$SCUPriSH_1_bag),
                                    Length = paste(input$SCUPriSH_1_len))
      SCUPriFHseason2 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPriFH_seas2[1], "-", input$SCUPriFH_seas2[2]),
                                    BagLimit = paste(input$SCUPriFH_2_bag),
                                    Length = paste(input$SCUPriFH_2_len))
      SCUPriPRseason2 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPriPR_seas2[1], "-", input$SCUPriPR_seas2[2]),
                                    BagLimit = paste(input$SCUPriPR_2_bag),
                                    Length = paste(input$SCUPriPR_2_len))
      SCUPriSHseason2 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPriSH_seas2[1], "-", input$SCUPriSH_seas2[2]),
                                    BagLimit = paste(input$SCUPriSH_2_bag),
                                    Length = paste(input$SCUPriSH_2_len))
      SCUPriFHseason3 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPriFH_seas3[1], "-", input$SCUPriFH_seas3[2]),
                                    BagLimit = paste(input$SCUPriFH_3_bag),
                                    Length = paste(input$SCUPriFH_3_len))
      SCUPriFHseason4 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPriFH_seas4[1], "-", input$SCUPriFH_seas4[2]),
                                    BagLimit = paste(input$SCUPriFH_4_bag),
                                    Length = paste(input$SCUPriFH_4_len))
      
      SCUPri <- rbind(SCUPriFHseason1, SCUPriFHseason2, SCUPriFHseason3, SCUPriFHseason4,  SCUPriPRseason1,  SCUPriPRseason2,SCUPriSHseason1,  SCUPriSHseason2)
      
      dat <- dat %>% rbind(SFri, BSBri, SCUPri)
      
    }
    
    #### CT regs ####
    if(any("CT" == input$state)){  
      if(input$SF_CT_input_type == "All Modes Combined"){
        
        SFctseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFct_seas1[1], "-", input$SFct_seas1[2]),
                                  BagLimit = paste(input$SFct_1_bag),
                                  Length = paste(input$SFct_1_len))
        SFctseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFct_seas2[1], "-", input$SFct_seas2[2]),
                                  BagLimit = paste(input$SFct_2_bag),
                                  Length = paste(input$SFct_2_len))
        SFctFHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFctFH_seas3[1], "-", input$SFctFH_seas3[2]),
                                    BagLimit = paste(input$SFctFH_3_bag),
                                    Length = paste(input$SFctFH_3_len))
        SFctPRseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFctPR_seas3[1], "-", input$SFctPR_seas3[2]),
                                    BagLimit = paste(input$SFctPR_3_bag),
                                    Length = paste(input$SFctPR_3_len))
        SFctSHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFctSH_seas3[1], "-", input$SFctSH_seas3[2]),
                                    BagLimit = paste(input$SFctSH_3_bag),
                                    Length = paste(input$SFctSH_3_len))
        SFct <- rbind(SFctseason1, SFctseason2, SFctFHseason3, SFctPRseason3, SFctSHseason3)
      } else {
        SFctFHseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFctFH_seas1[1], "-", input$SFctFH_seas1[2]),
                                    BagLimit = paste(input$SFctFH_1_bag),
                                    Length = paste(input$SFctFH_1_len))
        SFctPRseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFctPR_seas1[1], "-", input$SFctPR_seas1[2]),
                                    BagLimit = paste(input$SFctPR_1_bag),
                                    Length = paste(input$SFctPR_1_len))
        SFctSHseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFctSH_seas1[1], "-", input$SFctSH_seas1[2]),
                                    BagLimit = paste(input$SFctSH_1_bag),
                                    Length = paste(input$SFctSH_1_len))
        SFctFHseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFctFH_seas2[1], "-", input$SFctFH_seas2[2]),
                                    BagLimit = paste(input$SFctFH_2_bag),
                                    Length = paste(input$SFctFH_2_len))
        SFctPRseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFctPR_seas2[1], "-", input$SFctPR_seas2[2]),
                                    BagLimit = paste(input$SFctPR_2_bag),
                                    Length = paste(input$SFctPR_2_len))
        SFctSHseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFctSH_seas2[1], "-", input$SFctSH_seas2[2]),
                                    BagLimit = paste(input$SFctSH_2_bag),
                                    Length = paste(input$SFctSH_2_len))
        SFctFHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFctFH_seas3[1], "-", input$SFctFH_seas3[2]),
                                    BagLimit = paste(input$SFctFH_3_bag),
                                    Length = paste(input$SFctFH_3_len))
        SFctPRseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFctPR_seas3[1], "-", input$SFctPR_seas3[2]),
                                    BagLimit = paste(input$SFctPR_3_bag),
                                    Length = paste(input$SFctPR_3_len))
        SFctSHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFctSH_seas3[1], "-", input$SFctSH_seas3[2]),
                                    BagLimit = paste(input$SFctSH_3_bag),
                                    Length = paste(input$SFctSH_3_len))
        
        SFct <- rbind(SFctFHseason1,SFctFHseason2,SFctFHseason3, SFctPRseason1,SFctPRseason2,SFctPRseason3, SFctSHseason1,SFctSHseason2, SFctSHseason3)
      }
      
      
      BSBctFHseason1 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(input$BSBctFH_seas1[1], "-", input$BSBctFH_seas1[2]),
                                   BagLimit = paste(input$BSBctFH_1_bag),
                                   Length = paste(input$BSBctFH_1_len))
      BSBctFHseason2 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(input$BSBctFH_seas2[1], "-", input$BSBctFH_seas2[2]),
                                   BagLimit = paste(input$BSBctFH_2_bag),
                                   Length = paste(input$BSBctFH_2_len))
      BSBctFHseason3 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                   Season = paste(input$BSBctFH_seas3[1], "-", input$BSBctFH_seas3[2]),
                                   BagLimit = paste(input$BSBctFH_3_bag),
                                   Length = paste(input$BSBctFH_3_len))
      
      BSBctPRseason1 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(input$BSBctPR_seas1[1], "-", input$BSBctPR_seas1[2]),
                                   BagLimit = paste(input$BSBctPR_1_bag),
                                   Length = paste(input$BSBctPR_1_len))
      BSBctPRseason2 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(input$BSBctPR_seas2[1], "-", input$BSBctPR_seas2[2]),
                                   BagLimit = paste(input$BSBctPR_2_bag),
                                   Length = paste(input$BSBctPR_2_len))
      BSBctPRseason3 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                   Season = paste(input$BSBctPR_seas3[1], "-", input$BSBctPR_seas3[2]),
                                   BagLimit = paste(input$BSBctPR_3_bag),
                                   Length = paste(input$BSBctPR_3_len))
      
      BSBctSHseason1 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(input$BSBctSH_seas1[1], "-", input$BSBctSH_seas1[2]),
                                   BagLimit = paste(input$BSBctSH_1_bag),
                                   Length = paste(input$BSBctSH_1_len))
      BSBctSHseason2 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(input$BSBctSH_seas2[1], "-", input$BSBctSH_seas2[2]),
                                   BagLimit = paste(input$BSBctSH_2_bag),
                                   Length = paste(input$BSBctSH_2_len))
      BSBctSHseason3 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                   Season = paste(input$BSBctSH_seas3[1], "-", input$BSBctSH_seas3[2]),
                                   BagLimit = paste(input$BSBctSH_3_bag),
                                   Length = paste(input$BSBctSH_3_len))
      
      BSBct<- rbind(BSBctFHseason1, BSBctFHseason2, BSBctFHseason3,BSBctPRseason1, BSBctPRseason2,BSBctPRseason3,BSBctSHseason1, BSBctSHseason2, BSBctSHseason3)
      
      
      
      
      
      SCUPctFHseason1 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPctFH_seas1[1], "-", input$SCUPctFH_seas1[2]),
                                    BagLimit = paste(input$SCUPctFH_1_bag),
                                    Length = paste(input$SCUPctFH_1_len))
      SCUPctPRseason1 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPctPR_seas1[1], "-", input$SCUPctPR_seas1[2]),
                                    BagLimit = paste(input$SCUPctPR_1_bag),
                                    Length = paste(input$SCUPctPR_1_len))
      SCUPctSHseason1 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPctSH_seas1[1], "-", input$SCUPctSH_seas1[2]),
                                    BagLimit = paste(input$SCUPctSH_1_bag),
                                    Length = paste(input$SCUPctSH_1_len))
      SCUPctFHseason2 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPctFH_seas2[1], "-", input$SCUPctFH_seas2[2]),
                                    BagLimit = paste(input$SCUPctFH_2_bag),
                                    Length = paste(input$SCUPctFH_2_len))
      SCUPctPRseason2 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPctPR_seas2[1], "-", input$SCUPctPR_seas2[2]),
                                    BagLimit = paste(input$SCUPctPR_2_bag),
                                    Length = paste(input$SCUPctPR_2_len))
      SCUPctSHseason2 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPctSH_seas2[1], "-", input$SCUPctSH_seas2[2]),
                                    BagLimit = paste(input$SCUPctSH_2_bag),
                                    Length = paste(input$SCUPctSH_2_len))
      SCUPctFHseason3 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPctFH_seas3[1], "-", input$SCUPctFH_seas3[2]),
                                    BagLimit = paste(input$SCUPctFH_3_bag),
                                    Length = paste(input$SCUPctFH_3_len))
      SCUPctFHseason4 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPctFH_seas4[1], "-", input$SCUPctFH_seas4[2]),
                                    BagLimit = paste(input$SCUPctFH_4_bag),
                                    Length = paste(input$SCUPctFH_4_len))
      
      SCUPct <- rbind(SCUPctFHseason1, SCUPctFHseason2, SCUPctFHseason3, SCUPctFHseason4,  SCUPctPRseason1,  SCUPctPRseason2,SCUPctSHseason1,  SCUPctSHseason2)
      
      dat <- dat %>% rbind(SFct, BSBct, SCUPct)
      
    }
    
    #### NY Regs ####
    if(any("NY" == input$state)){  
      if(input$SF_NY_input_type == "All Modes Combined"){
        
        SFnyseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFny_seas1[1], "-", input$SFny_seas1[2]),
                                  BagLimit = paste(input$SFny_1_bag),
                                  Length = paste(input$SFny_1_len))
        SFnyseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFny_seas2[1], "-", input$SFny_seas2[2]),
                                  BagLimit = paste(input$SFny_2_bag),
                                  Length = paste(input$SFny_2_len))
        SFnyFHseason3 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnyFH_seas3[1], "-", input$SFnyFH_seas3[2]),
                                    BagLimit = paste(input$SFnyFH_3_bag),
                                    Length = paste(input$SFnyFH_3_len))
        SFnyPRseason3 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnyPR_seas3[1], "-", input$SFnyPR_seas3[2]),
                                    BagLimit = paste(input$SFnyPR_3_bag),
                                    Length = paste(input$SFnyPR_3_len))
        SFnySHseason3 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnySH_seas3[1], "-", input$SFnySH_seas3[2]),
                                    BagLimit = paste(input$SFnySH_3_bag),
                                    Length = paste(input$SFnySH_3_len))
        SFny <- rbind(SFnyseason1, SFnyseason2, SFnyFHseason3, SFnyPRseason3, SFnySHseason3)
      } else {
        SFnyFHseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnyFH_seas1[1], "-", input$SFnyFH_seas1[2]),
                                    BagLimit = paste(input$SFnyFH_1_bag),
                                    Length = paste(input$SFnyFH_1_len))
        SFnyPRseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnyPR_seas1[1], "-", input$SFnyPR_seas1[2]),
                                    BagLimit = paste(input$SFnyPR_1_bag),
                                    Length = paste(input$SFnyPR_1_len))
        SFnySHseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnySH_seas1[1], "-", input$SFnySH_seas1[2]),
                                    BagLimit = paste(input$SFnySH_1_bag),
                                    Length = paste(input$SFnySH_1_len))
        SFnyFHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnyFH_seas2[1], "-", input$SFnyFH_seas2[2]),
                                    BagLimit = paste(input$SFnyFH_2_bag),
                                    Length = paste(input$SFnyFH_2_len))
        SFnyPRseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnyPR_seas2[1], "-", input$SFnyPR_seas2[2]),
                                    BagLimit = paste(input$SFnyPR_2_bag),
                                    Length = paste(input$SFnyPR_2_len))
        SFnySHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnySH_seas2[1], "-", input$SFnySH_seas2[2]),
                                    BagLimit = paste(input$SFnySH_2_bag),
                                    Length = paste(input$SFnySH_2_len))
        
        SFnyFHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnyFH_seas3[1], "-", input$SFnyFH_seas3[2]),
                                    BagLimit = paste(input$SFnyFH_3_bag),
                                    Length = paste(input$SFnyFH_3_len))
        SFnyPRseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnyPR_seas3[1], "-", input$SFnyPR_seas3[2]),
                                    BagLimit = paste(input$SFnyPR_3_bag),
                                    Length = paste(input$SFnyPR_3_len))
        SFnySHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnySH_seas3[1], "-", input$SFnySH_seas3[2]),
                                    BagLimit = paste(input$SFnySH_3_bag),
                                    Length = paste(input$SFnySH_3_len))
        
        SFny <- rbind(SFnyFHseason1,SFnyFHseason2, SFnyFHseason3,SFnyPRseason1,SFnyPRseason2, SFnyPRseason3,SFnySHseason1,SFnySHseason2,SFnySHseason3)
      }
      
      
      if(input$BSB_NY_input_type == "All Modes Combined"){
        BSBnyseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBny_seas1[1], "-", input$BSBny_seas1[2]),
                                   BagLimit = paste(input$BSBny_1_bag),
                                   Length = paste(input$BSBny_1_len))
        BSBnyseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBny_seas2[1], "-", input$BSBny_seas2[2]),
                                   BagLimit = paste(input$BSBny_2_bag),
                                   Length = paste(input$BSBny_2_len))
        BSBnyFHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnyFH_seas3[1], "-", input$BSBnyFH_seas3[2]),
                                     BagLimit = paste(input$BSBnyFH_3_bag),
                                     Length = paste(input$BSBnyFH_3_len))
        BSBnyPRseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnyPR_seas3[1], "-", input$BSBnyPR_seas3[2]),
                                     BagLimit = paste(input$BSBnyPR_3_bag),
                                     Length = paste(input$BSBnyPR_3_len))
        BSBnySHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnySH_seas3[1], "-", input$BSBnySH_seas3[2]),
                                     BagLimit = paste(input$BSBnySH_3_bag),
                                     Length = paste(input$BSBnySH_3_len))
        BSBny<- rbind(BSBnyseason1, BSBnyseason2,BSBnyFHseason3,BSBnyPRseason3,BSBnySHseason3)
      } else {
        BSBnyFHseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnyFH_seas1[1], "-", input$BSBnyFH_seas1[2]),
                                     BagLimit = paste(input$BSBnyFH_1_bag),
                                     Length = paste(input$BSBnyFH_1_len))
        BSBnyFHseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnyFH_seas2[1], "-", input$BSBnyFH_seas2[2]),
                                     BagLimit = paste(input$BSBnyFH_2_bag),
                                     Length = paste(input$BSBnyFH_2_len))
        BSBnyFHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnyFH_seas3[1], "-", input$BSBnyFH_seas3[2]),
                                     BagLimit = paste(input$BSBnyFH_3_bag),
                                     Length = paste(input$BSBnyFH_3_len))
        
        BSBnyPRseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnyPR_seas1[1], "-", input$BSBnyPR_seas1[2]),
                                     BagLimit = paste(input$BSBnyPR_1_bag),
                                     Length = paste(input$BSBnyPR_1_len))
        BSBnyPRseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnyPR_seas2[1], "-", input$BSBnyPR_seas2[2]),
                                     BagLimit = paste(input$BSBnyPR_2_bag),
                                     Length = paste(input$BSBnyPR_2_len))
        BSBnyPRseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnyPR_seas3[1], "-", input$BSBnyPR_seas3[2]),
                                     BagLimit = paste(input$BSBnyPR_3_bag),
                                     Length = paste(input$BSBnyPR_3_len))
        
        BSBnySHseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnySH_seas1[1], "-", input$BSBnySH_seas1[2]),
                                     BagLimit = paste(input$BSBnySH_1_bag),
                                     Length = paste(input$BSBnySH_1_len))
        BSBnySHseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnySH_seas2[1], "-", input$BSBnySH_seas2[2]),
                                     BagLimit = paste(input$BSBnySH_2_bag),
                                     Length = paste(input$BSBnySH_2_len))
        BSBnySHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnySH_seas3[1], "-", input$BSBnySH_seas3[2]),
                                     BagLimit = paste(input$BSBnySH_3_bag),
                                     Length = paste(input$BSBnySH_3_len))
        
        BSBny<- rbind(BSBnyFHseason1, BSBnyFHseason2, BSBnyFHseason3,BSBnyPRseason1, BSBnyPRseason2,BSBnyPRseason3,BSBnySHseason1, BSBnySHseason2, BSBnySHseason3)
      }
      
      
      
      SCUPnyFHseason1 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPnyFH_seas1[1], "-", input$SCUPnyFH_seas1[2]),
                                    BagLimit = paste(input$SCUPnyFH_1_bag),
                                    Length = paste(input$SCUPnyFH_1_len))
      SCUPnyPRseason1 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPnyPR_seas1[1], "-", input$SCUPnyPR_seas1[2]),
                                    BagLimit = paste(input$SCUPnyPR_1_bag),
                                    Length = paste(input$SCUPnyPR_1_len))
      SCUPnySHseason1 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPnySH_seas1[1], "-", input$SCUPnySH_seas1[2]),
                                    BagLimit = paste(input$SCUPnySH_1_bag),
                                    Length = paste(input$SCUPnySH_1_len))
      SCUPnyFHseason2 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPnyFH_seas2[1], "-", input$SCUPnyFH_seas2[2]),
                                    BagLimit = paste(input$SCUPnyFH_2_bag),
                                    Length = paste(input$SCUPnyFH_2_len))
      SCUPnyPRseason2 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Private"),
                                    Season = paste(input$SCUPnyPR_seas2[1], "-", input$SCUPnyPR_seas2[2]),
                                    BagLimit = paste(input$SCUPnyPR_2_bag),
                                    Length = paste(input$SCUPnyPR_2_len))
      SCUPnySHseason2 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Shore"),
                                    Season = paste(input$SCUPnySH_seas2[1], "-", input$SCUPnySH_seas2[2]),
                                    BagLimit = paste(input$SCUPnySH_2_bag),
                                    Length = paste(input$SCUPnySH_2_len))
      SCUPnyFHseason3 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPnyFH_seas3[1], "-", input$SCUPnyFH_seas3[2]),
                                    BagLimit = paste(input$SCUPnyFH_3_bag),
                                    Length = paste(input$SCUPnyFH_3_len))
      SCUPnyFHseason4 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
                                    Season = paste(input$SCUPnyFH_seas4[1], "-", input$SCUPnyFH_seas4[2]),
                                    BagLimit = paste(input$SCUPnyFH_4_bag),
                                    Length = paste(input$SCUPnyFH_4_len))
      
      SCUPny <- rbind(SCUPnyFHseason1, SCUPnyFHseason2, SCUPnyFHseason3, SCUPnyFHseason4,  SCUPnyPRseason1,  SCUPnyPRseason2,SCUPnySHseason1,  SCUPnySHseason2)
      
      dat <- dat %>% rbind(SFny, BSBny, SCUPny)
      
    }
     
    #### NJ Regs ####
    if(any("NJ" == input$state)){  
      if(input$SF_NJ_input_type == "All Modes Combined"){
        
        SFnjseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFnj_seas1[1], "-", input$SFnj_seas1[2]),
                                  BagLimit = paste(input$SFnj_1_bag),
                                  Length = paste(input$SFnj_1_len))
        SFnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnjFH_seas2[1], "-", input$SFnjFH_seas2[2]),
                                    BagLimit = paste(input$SFnjFH_2_bag),
                                    Length = paste(input$SFnjFH_2_len))
        SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]),
                                    BagLimit = paste(input$SFnjPR_2_bag),
                                    Length = paste(input$SFnjPR_2_len))
        SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
                                    BagLimit = paste(input$SFnjSH_2_bag),
                                    Length = paste(input$SFnjSH_2_len))
        SFnj <- rbind(SFnjseason1, SFnjFHseason2, SFnjPRseason2, SFnjSHseason2)
      } else {
        SFnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnjFH_seas1[1], "-", input$SFnjFH_seas1[2]),
                                    BagLimit = paste(input$SFnjFH_1_bag),
                                    Length = paste(input$SFnjFH_1_len))
        SFnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFnjFH_seas2[1], "-", input$SFnjFH_seas2[2]),
                                    BagLimit = paste(input$SFnjFH_2_bag),
                                    Length = paste(input$SFnjFH_2_len))
        SFnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnjPR_seas1[1], "-", input$SFnjPR_seas1[2]),
                                    BagLimit = paste(input$SFnjPR_1_bag),
                                    Length = paste(input$SFnjPR_1_len))
        SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]),
                                    BagLimit = paste(input$SFnjPR_2_bag),
                                    Length = paste(input$SFnjPR_2_len))
        SFnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas1[1], "-", input$SFnjSH_seas1[2]),
                                    BagLimit = paste(input$SFnjSH_1_bag),
                                    Length = paste(input$SFnjSH_1_len))
        SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
                                    BagLimit = paste(input$SFnjSH_2_bag),
                                    Length = paste(input$SFnjSH_2_len))
        
        SFnj <- rbind(SFnjFHseason1,SFnjFHseason2, SFnjPRseason1,SFnjPRseason2, SFnjSHseason1,SFnjSHseason2)
      }
      
      if(input$BSB_NJ_input_type == "All Modes Combined"){
        BSBnjseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBnj_seas1[1], "-", input$BSBnj_seas1[2]),
                                   BagLimit = paste(input$BSBnj_1_bag),
                                   Length = paste(input$BSBnj_1_len))
        BSBnjseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBnj_seas2[1], "-", input$BSBnj_seas2[2]),
                                   BagLimit = paste(input$BSBnj_2_bag),
                                   Length = paste(input$BSBnj_2_len))
        BSBnjseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBnj_seas3[1], "-", input$BSBnj_seas3[2]),
                                   BagLimit = paste(input$BSBnj_3_bag),
                                   Length = paste(input$BSBnj_3_len))
        BSBnjseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBnj_seas4[1], "-", input$BSBnj_seas4[2]),
                                   BagLimit = paste(input$BSBnj_4_bag),
                                   Length = paste(input$BSBnj_4_len))
        
        BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas5[1], "-", input$BSBnjFH_seas5[2]),
                                     BagLimit = paste(input$BSBnjFH_5_bag),
                                     Length = paste(input$BSBnjFH_5_len))
        BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas5[1], "-", input$BSBnjPR_seas5[2]),
                                     BagLimit = paste(input$BSBnjPR_5_bag),
                                     Length = paste(input$BSBnjPR_5_len))
        BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas5[1], "-", input$BSBnjSH_seas5[2]),
                                     BagLimit = paste(input$BSBnjSH_5_bag),
                                     Length = paste(input$BSBnjSH_5_len))
        
        BSBnj<- rbind(BSBnjseason1, BSBnjseason2, BSBnjseason3, BSBnjseason4, BSBnjFHseason5, BSBnjPRseason5, BSBnjSHseason5)
      }else{
        
        BSBnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas1[1], "-", input$BSBnjFH_seas1[2]),
                                     BagLimit = paste(input$BSBnjFH_1_bag),
                                     Length = paste(input$BSBnjFH_1_len))
        BSBnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas2[1], "-", input$BSBnjFH_seas2[2]),
                                     BagLimit = paste(input$BSBnjFH_2_bag),
                                     Length = paste(input$BSBnjFH_2_len))
        BSBnjFHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas3[1], "-", input$BSBnjFH_seas3[2]),
                                     BagLimit = paste(input$BSBnjFH_3_bag),
                                     Length = paste(input$BSBnjFH_3_len))
        BSBnjFHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas4[1], "-", input$BSBnjFH_seas4[2]),
                                     BagLimit = paste(input$BSBnjFH_4_bag),
                                     Length = paste(input$BSBnjFH_4_len))
        BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBnjFH_seas5[1], "-", input$BSBnjFH_seas5[2]),
                                     BagLimit = paste(input$BSBnjFH_5_bag),
                                     Length = paste(input$BSBnjFH_5_len))
        
        BSBnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas1[1], "-", input$BSBnjPR_seas1[2]),
                                     BagLimit = paste(input$BSBnjPR_1_bag),
                                     Length = paste(input$BSBnjPR_1_len))
        BSBnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas2[1], "-", input$BSBnjPR_seas2[2]),
                                     BagLimit = paste(input$BSBnjPR_2_bag),
                                     Length = paste(input$BSBnjPR_2_len))
        BSBnjPRseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas3[1], "-", input$BSBnjPR_seas3[2]),
                                     BagLimit = paste(input$BSBnjPR_3_bag),
                                     Length = paste(input$BSBnjPR_3_len))
        BSBnjPRseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas4[1], "-", input$BSBnjPR_seas4[2]),
                                     BagLimit = paste(input$BSBnjPR_4_bag),
                                     Length = paste(input$BSBnjPR_4_len))
        BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBnjPR_seas5[1], "-", input$BSBnjPR_seas5[2]),
                                     BagLimit = paste(input$BSBnjPR_5_bag),
                                     Length = paste(input$BSBnjPR_5_len))
        
        BSBnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas1[1], "-", input$BSBnjSH_seas1[2]),
                                     BagLimit = paste(input$BSBnjSH_1_bag),
                                     Length = paste(input$BSBnjSH_1_len))
        BSBnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas2[1], "-", input$BSBnjSH_seas2[2]),
                                     BagLimit = paste(input$BSBnjSH_2_bag),
                                     Length = paste(input$BSBnjSH_2_len))
        BSBnjSHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas3[1], "-", input$BSBnjSH_seas3[2]),
                                     BagLimit = paste(input$BSBnjSH_3_bag),
                                     Length = paste(input$BSBnjSH_3_len))
        BSBnjSHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas4[1], "-", input$BSBnjSH_seas4[2]),
                                     BagLimit = paste(input$BSBnjSH_4_bag),
                                     Length = paste(input$BSBnjSH_4_len))
        BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBnjSH_seas5[1], "-", input$BSBnjSH_seas5[2]),
                                     BagLimit = paste(input$BSBnjSH_5_bag),
                                     Length = paste(input$BSBnjSH_5_len))
        
        
        BSBnj<- rbind(BSBnjFHseason1, BSBnjFHseason2, BSBnjFHseason3, BSBnjFHseason4, BSBnjFHseason5,
                      BSBnjPRseason1, BSBnjPRseason2, BSBnjPRseason3, BSBnjPRseason4, BSBnjPRseason5,
                      BSBnjSHseason1, BSBnjSHseason2, BSBnjSHseason3, BSBnjSHseason4, BSBnjSHseason5)
      }
      
      
      
      if(input$SCUP_NJ_input_type == "All Modes Combined"){
        SCUPnjseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPnj_seas1[1], "-", input$SCUPnj_seas1[2]),
                                    BagLimit = paste(input$SCUPnj_1_bag),
                                    Length = paste(input$SCUPnj_1_len))
        
        SCUPnjFHseason2<- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
                                     Season = paste(input$SCUPnjFH_seas2[1], "-", input$SCUPnjFH_seas2[2]),
                                     BagLimit = paste(input$SCUPnjFH_2_bag),
                                     Length = paste(input$SCUPnjFH_2_len))
        SCUPnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPnjPR_seas2[1], "-", input$SCUPnjPR_seas2[2]),
                                      BagLimit = paste(input$SCUPnjPR_2_bag),
                                      Length = paste(input$SCUPnjPR_2_len))
        SCUPnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPnjSH_seas2[1], "-", input$SCUPnjSH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjSH_2_bag),
                                      Length = paste(input$SCUPnjSH_2_len))
        
        SCUPnj <- rbind(SCUPnjseason1, SCUPnjFHseason2, SCUPnjPRseason2, SCUPnjSHseason2)
      } else {
        SCUPnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPnjFH_seas1[1], "-", input$SCUPnjFH_seas1[2]),
                                      BagLimit = paste(input$SCUPnjFH_1_bag),
                                      Length = paste(input$SCUPnjFH_1_len))
        SCUPnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPnjPR_seas1[1], "-", input$SCUPnjPR_seas1[2]),
                                      BagLimit = paste(input$SCUPnjPR_1_bag),
                                      Length = paste(input$SCUPnjPR_1_len))
        SCUPnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPnjSH_seas1[1], "-", input$SCUPnjSH_seas1[2]),
                                      BagLimit = paste(input$SCUPnjSH_1_bag),
                                      Length = paste(input$SCUPnjSH_1_len))
        
        SCUPnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPnjFH_seas2[1], "-", input$SCUPnjFH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjFH_2_bag),
                                      Length = paste(input$SCUPnjFH_2_len))
        SCUPnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPnjPR_seas2[1], "-", input$SCUPnjPR_seas2[2]),
                                      BagLimit = paste(input$SCUPnjPR_2_bag),
                                      Length = paste(input$SCUPnjPR_2_len))
        SCUPnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPnjSH_seas2[1], "-", input$SCUPnjSH_seas2[2]),
                                      BagLimit = paste(input$SCUPnjSH_2_bag),
                                      Length = paste(input$SCUPnjSH_2_len))
        
        SCUPnj <- rbind(SCUPnjFHseason1, SCUPnjPRseason1, SCUPnjSHseason1, SCUPnjFHseason2, SCUPnjPRseason2, SCUPnjSHseason2)
      }
      dat <- dat %>% rbind(SFnj, BSBnj, SCUPnj)
    }
    
    #### DE Regs ####
    if(any("DE" == input$state)){  
      if(input$SF_DE_input_type == "All Modes Combined"){
        
        SFdeseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFde_seas1[1], "-", input$SFde_seas1[2]),
                                  BagLimit = paste(input$SFde_1_bag),
                                  Length = paste(input$SFde_1_len))
        SFdeseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFde_seas2[1], "-", input$SFde_seas2[2]),
                                  BagLimit = paste(input$SFde_2_bag),
                                  Length = paste(input$SFde_2_len))
        SFdeFHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFdeFH_seas3[1], "-", input$SFdeFH_seas3[2]),
                                    BagLimit = paste(input$SFdeFH_3_bag),
                                    Length = paste(input$SFdeFH_3_len))
        SFdePRseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFdePR_seas3[1], "-", input$SFdePR_seas3[2]),
                                    BagLimit = paste(input$SFdePR_3_bag),
                                    Length = paste(input$SFdePR_3_len))
        SFdeSHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFdeSH_seas3[1], "-", input$SFdeSH_seas3[2]),
                                    BagLimit = paste(input$SFdeSH_3_bag),
                                    Length = paste(input$SFdeSH_3_len))
        SFde <- rbind(SFdeseason1,SFdeseason2, SFdeFHseason3, SFdePRseason3, SFdeSHseason3)
      } else {
        SFdeFHseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFdeFH_seas1[1], "-", input$SFdeFH_seas1[2]),
                                    BagLimit = paste(input$SFdeFH_1_bag),
                                    Length = paste(input$SFdeFH_1_len))
        SFdePRseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFdePR_seas1[1], "-", input$SFdePR_seas1[2]),
                                    BagLimit = paste(input$SFdePR_1_bag),
                                    Length = paste(input$SFdePR_1_len))
        SFdeSHseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFdeSH_seas1[1], "-", input$SFdeSH_seas1[2]),
                                    BagLimit = paste(input$SFdeSH_1_bag),
                                    Length = paste(input$SFdeSH_1_len))
        SFdeFHseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFdeFH_seas2[1], "-", input$SFdeFH_seas2[2]),
                                    BagLimit = paste(input$SFdeFH_2_bag),
                                    Length = paste(input$SFdeFH_2_len))
        SFdePRseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFdePR_seas2[1], "-", input$SFdePR_seas2[2]),
                                    BagLimit = paste(input$SFdePR_2_bag),
                                    Length = paste(input$SFdePR_2_len))
        SFdeSHseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFdeSH_seas2[1], "-", input$SFdeSH_seas2[2]),
                                    BagLimit = paste(input$SFdeSH_2_bag),
                                    Length = paste(input$SFdeSH_2_len))
        SFdeFHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFdeFH_seas3[1], "-", input$SFdeFH_seas3[2]),
                                    BagLimit = paste(input$SFdeFH_3_bag),
                                    Length = paste(input$SFdeFH_3_len))
        SFdePRseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFdePR_seas3[1], "-", input$SFdePR_seas3[2]),
                                    BagLimit = paste(input$SFdePR_3_bag),
                                    Length = paste(input$SFdePR_3_len))
        SFdeSHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFdeSH_seas3[1], "-", input$SFdeSH_seas3[2]),
                                    BagLimit = paste(input$SFdeSH_3_bag),
                                    Length = paste(input$SFdeSH_3_len))
        
        SFde <- rbind(SFdeFHseason1,SFdeFHseason2,SFdeFHseason3, SFdePRseason1,SFdePRseason2, SFdePRseason3, SFdeSHseason1,SFdeSHseason2, SFdeSHseason3)
      }
      
      
      if(input$BSB_DE_input_type == "All Modes Combined"){
        BSBdeseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBde_seas1[1], "-", input$BSBde_seas1[2]),
                                   BagLimit = paste(input$BSBde_1_bag),
                                   Length = paste(input$BSBde_1_len))
        BSBdeseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBde_seas2[1], "-", input$BSBde_seas2[2]),
                                   BagLimit = paste(input$BSBde_2_bag),
                                   Length = paste(input$BSBde_2_len))
        BSBdeFHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBdeFH_seas3[1], "-", input$BSBdeFH_seas3[2]),
                                     BagLimit = paste(input$BSBdeFH_3_bag),
                                     Length = paste(input$BSBdeFH_3_len))
        BSBdePRseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBdePR_seas3[1], "-", input$BSBdePR_seas3[2]),
                                     BagLimit = paste(input$BSBdePR_3_bag),
                                     Length = paste(input$BSBdePR_3_len))
        BSBdeSHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBdeSH_seas3[1], "-", input$BSBdeSH_seas3[2]),
                                     BagLimit = paste(input$BSBdeSH_3_bag),
                                     Length = paste(input$BSBdeSH_3_len))
        BSBde<- rbind(BSBdeseason1, BSBdeseason2,BSBdeFHseason3,BSBdePRseason3,BSBdeSHseason3)
      } else {
        
        BSBdeFHseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBdeFH_seas1[1], "-", input$BSBdeFH_seas1[2]),
                                     BagLimit = paste(input$BSBdeFH_1_bag),
                                     Length = paste(input$BSBdeFH_1_len))
        BSBdeFHseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBdeFH_seas2[1], "-", input$BSBdeFH_seas2[2]),
                                     BagLimit = paste(input$BSBdeFH_2_bag),
                                     Length = paste(input$BSBdeFH_2_len))
        BSBdeFHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBdeFH_seas3[1], "-", input$BSBdeFH_seas3[2]),
                                     BagLimit = paste(input$BSBdeFH_3_bag),
                                     Length = paste(input$BSBdeFH_3_len))
        
        BSBdePRseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBdePR_seas1[1], "-", input$BSBdePR_seas1[2]),
                                     BagLimit = paste(input$BSBdePR_1_bag),
                                     Length = paste(input$BSBdePR_1_len))
        BSBdePRseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBdePR_seas2[1], "-", input$BSBdePR_seas2[2]),
                                     BagLimit = paste(input$BSBdePR_2_bag),
                                     Length = paste(input$BSBdePR_2_len))
        BSBdePRseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBdePR_seas3[1], "-", input$BSBdePR_seas3[2]),
                                     BagLimit = paste(input$BSBdePR_3_bag),
                                     Length = paste(input$BSBdePR_3_len))
        
        BSBdeSHseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBdeSH_seas1[1], "-", input$BSBdeSH_seas1[2]),
                                     BagLimit = paste(input$BSBdeSH_1_bag),
                                     Length = paste(input$BSBdeSH_1_len))
        BSBdeSHseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBdeSH_seas2[1], "-", input$BSBdeSH_seas2[2]),
                                     BagLimit = paste(input$BSBdeSH_2_bag),
                                     Length = paste(input$BSBdeSH_2_len))
        BSBdeSHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBdeSH_seas3[1], "-", input$BSBdeSH_seas3[2]),
                                     BagLimit = paste(input$BSBdeSH_3_bag),
                                     Length = paste(input$BSBdeSH_3_len))
        
        BSBde<- rbind(BSBdeFHseason1, BSBdeFHseason2, BSBdeFHseason3,
                      BSBdePRseason1, BSBdePRseason2, BSBdePRseason3,
                      BSBdeSHseason1, BSBdeSHseason2, BSBdeSHseason3)
        
      }
      
      
      
      if(input$SCUP_DE_input_type == "All Modes Combined"){
        SCUPdeseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPde_seas1[1], "-", input$SCUPde_seas1[2]),
                                    BagLimit = paste(input$SCUPde_1_bag),
                                    Length = paste(input$SCUPde_1_len))
        SCUPdeFHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPdeFH_seas2[1], "-", input$SCUPdeFH_seas2[2]),
                                      BagLimit = paste(input$SCUPdeFH_2_bag),
                                      Length = paste(input$SCUPdeFH_2_len))
        SCUPdePRseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPdePR_seas2[1], "-", input$SCUPdePR_seas2[2]),
                                      BagLimit = paste(input$SCUPdePR_2_bag),
                                      Length = paste(input$SCUPdePR_2_len))
        SCUPdeSHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPdeSH_seas2[1], "-", input$SCUPdeSH_seas2[2]),
                                      BagLimit = paste(input$SCUPdeSH_2_bag),
                                      Length = paste(input$SCUPdeSH_2_len))
        SCUPde <- rbind(SCUPdeseason1, SCUPdeFHseason2, SCUPdePRseason2, SCUPdeSHseason2)
      } else {
        SCUPdeFHseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPdeFH_seas1[1], "-", input$SCUPdeFH_seas1[2]),
                                      BagLimit = paste(input$SCUPdeFH_1_bag),
                                      Length = paste(input$SCUPdeFH_1_len))
        SCUPdePRseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPdePR_seas1[1], "-", input$SCUPdePR_seas1[2]),
                                      BagLimit = paste(input$SCUPdePR_1_bag),
                                      Length = paste(input$SCUPdePR_1_len))
        SCUPdeSHseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPdeSH_seas1[1], "-", input$SCUPdeSH_seas1[2]),
                                      BagLimit = paste(input$SCUPdeSH_1_bag),
                                      Length = paste(input$SCUPdeSH_1_len))
        SCUPdeFHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPdeFH_seas2[1], "-", input$SCUPdeFH_seas2[2]),
                                      BagLimit = paste(input$SCUPdeFH_2_bag),
                                      Length = paste(input$SCUPdeFH_2_len))
        SCUPdePRseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPdePR_seas2[1], "-", input$SCUPdePR_seas2[2]),
                                      BagLimit = paste(input$SCUPdePR_2_bag),
                                      Length = paste(input$SCUPdePR_2_len))
        SCUPdeSHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPdeSH_seas2[1], "-", input$SCUPdeSH_seas2[2]),
                                      BagLimit = paste(input$SCUPdeSH_2_bag),
                                      Length = paste(input$SCUPdeSH_2_len))
        
        SCUPde <- rbind(SCUPdeFHseason1, SCUPdeFHseason2, SCUPdePRseason1,  SCUPdePRseason2,SCUPdeSHseason1,  SCUPdeSHseason2)
        
      }
      dat <- dat %>% rbind(SFde, BSBde, SCUPde)
      
    }
    
    #### MD Regs ####
    if(any("MD" == input$state)){  
      if(input$SF_MD_input_type == "All Modes Combined"){
        
        SFmdseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFmd_seas1[1], "-", input$SFmd_seas1[2]),
                                  BagLimit = paste(input$SFmd_1_bag),
                                  Length = paste(input$SFmd_1_len))
        SFmdseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFmd_seas2[1], "-", input$SFmd_seas2[2]),
                                  BagLimit = paste(input$SFmd_2_bag),
                                  Length = paste(input$SFmd_2_len))
        
        SFmdFHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFmdFH_seas3[1], "-", input$SFmdFH_seas3[2]),
                                    BagLimit = paste(input$SFmdFH_3_bag),
                                    Length = paste(input$SFmdFH_3_len))
        SFmdPRseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFmdPR_seas3[1], "-", input$SFmdPR_seas3[2]),
                                    BagLimit = paste(input$SFmdPR_3_bag),
                                    Length = paste(input$SFmdPR_3_len))
        SFmdSHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFmdSH_seas3[1], "-", input$SFmdSH_seas3[2]),
                                    BagLimit = paste(input$SFmdSH_3_bag),
                                    Length = paste(input$SFmdSH_3_len))
        SFmd <- rbind(SFmdseason1,SFmdseason2, SFmdFHseason3, SFmdPRseason3, SFmdSHseason3)
      } else {
        SFmdFHseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFmdFH_seas1[1], "-", input$SFmdFH_seas1[2]),
                                    BagLimit = paste(input$SFmdFH_1_bag),
                                    Length = paste(input$SFmdFH_1_len))
        SFmdPRseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFmdPR_seas1[1], "-", input$SFmdPR_seas1[2]),
                                    BagLimit = paste(input$SFmdPR_1_bag),
                                    Length = paste(input$SFmdPR_1_len))
        SFmdSHseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFmdSH_seas1[1], "-", input$SFmdSH_seas1[2]),
                                    BagLimit = paste(input$SFmdSH_1_bag),
                                    Length = paste(input$SFmdSH_1_len))
        SFmdFHseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFmdFH_seas2[1], "-", input$SFmdFH_seas2[2]),
                                    BagLimit = paste(input$SFmdFH_2_bag),
                                    Length = paste(input$SFmdFH_2_len))
        SFmdPRseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFmdPR_seas2[1], "-", input$SFmdPR_seas2[2]),
                                    BagLimit = paste(input$SFmdPR_2_bag),
                                    Length = paste(input$SFmdPR_2_len))
        SFmdSHseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFmdSH_seas2[1], "-", input$SFmdSH_seas2[2]),
                                    BagLimit = paste(input$SFmdSH_2_bag),
                                    Length = paste(input$SFmdSH_2_len))
        SFmdFHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFmdFH_seas3[1], "-", input$SFmdFH_seas3[2]),
                                    BagLimit = paste(input$SFmdFH_3_bag),
                                    Length = paste(input$SFmdFH_3_len))
        SFmdPRseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFmdPR_seas3[1], "-", input$SFmdPR_seas3[2]),
                                    BagLimit = paste(input$SFmdPR_3_bag),
                                    Length = paste(input$SFmdPR_3_len))
        SFmdSHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFmdSH_seas3[1], "-", input$SFmdSH_seas3[2]),
                                    BagLimit = paste(input$SFmdSH_3_bag),
                                    Length = paste(input$SFmdSH_3_len))
        
        SFmd <- rbind(SFmdFHseason1,SFmdFHseason2,SFmdFHseason3, SFmdPRseason1,SFmdPRseason2, SFmdPRseason3, SFmdSHseason1,SFmdSHseason2,SFmdSHseason3)
      }
      
      if(input$BSB_MD_input_type == "All Modes Combined"){
        BSBmdseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBmd_seas1[1], "-", input$BSBmd_seas1[2]),
                                   BagLimit = paste(input$BSBmd_1_bag),
                                   Length = paste(input$BSBmd_1_len))
        BSBmdseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBmd_seas2[1], "-", input$BSBmd_seas2[2]),
                                   BagLimit = paste(input$BSBmd_2_bag),
                                   Length = paste(input$BSBmd_2_len))
        BSBmdFHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmdFH_seas3[1], "-", input$BSBmdFH_seas3[2]),
                                     BagLimit = paste(input$BSBmdFH_3_bag),
                                     Length = paste(input$BSBmdFH_3_len))
        BSBmdPRseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmdPR_seas3[1], "-", input$BSBmdPR_seas3[2]),
                                     BagLimit = paste(input$BSBmdPR_3_bag),
                                     Length = paste(input$BSBmdPR_3_len))
        BSBmdSHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmdSH_seas3[1], "-", input$BSBmdSH_seas3[2]),
                                     BagLimit = paste(input$BSBmdSH_3_bag),
                                     Length = paste(input$BSBmdSH_3_len))
        BSBmd<- rbind(BSBmdseason1, BSBmdseason2,BSBmdFHseason3,BSBmdPRseason3,BSBmdSHseason3)
      } else {
        BSBmdFHseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmdFH_seas1[1], "-", input$BSBmdFH_seas1[2]),
                                     BagLimit = paste(input$BSBmdFH_1_bag),
                                     Length = paste(input$BSBmdFH_1_len))
        BSBmdFHseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmdFH_seas2[1], "-", input$BSBmdFH_seas2[2]),
                                     BagLimit = paste(input$BSBmdFH_2_bag),
                                     Length = paste(input$BSBmdFH_2_len))
        BSBmdFHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBmdFH_seas3[1], "-", input$BSBmdFH_seas3[2]),
                                     BagLimit = paste(input$BSBmdFH_3_bag),
                                     Length = paste(input$BSBmdFH_3_len))
        
        BSBmdPRseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmdPR_seas1[1], "-", input$BSBmdPR_seas1[2]),
                                     BagLimit = paste(input$BSBmdPR_1_bag),
                                     Length = paste(input$BSBmdPR_1_len))
        BSBmdPRseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmdPR_seas2[1], "-", input$BSBmdPR_seas2[2]),
                                     BagLimit = paste(input$BSBmdPR_2_bag),
                                     Length = paste(input$BSBmdPR_2_len))
        BSBmdPRseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBmdPR_seas3[1], "-", input$BSBmdPR_seas3[2]),
                                     BagLimit = paste(input$BSBmdPR_3_bag),
                                     Length = paste(input$BSBmdPR_3_len))
        
        BSBmdSHseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmdSH_seas1[1], "-", input$BSBmdSH_seas1[2]),
                                     BagLimit = paste(input$BSBmdSH_1_bag),
                                     Length = paste(input$BSBmdSH_1_len))
        BSBmdSHseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmdSH_seas2[1], "-", input$BSBmdSH_seas2[2]),
                                     BagLimit = paste(input$BSBmdSH_2_bag),
                                     Length = paste(input$BSBmdSH_2_len))
        BSBmdSHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBmdSH_seas3[1], "-", input$BSBmdSH_seas3[2]),
                                     BagLimit = paste(input$BSBmdSH_3_bag),
                                     Length = paste(input$BSBmdSH_3_len))
        
        BSBmd<- rbind(BSBmdFHseason1, BSBmdFHseason2, BSBmdFHseason3,BSBmdPRseason1, BSBmdPRseason2,BSBmdPRseason3,BSBmdSHseason1, BSBmdSHseason2, BSBmdSHseason3)
      }
      
      
      
      if(input$SCUP_MD_input_type == "All Modes Combined"){
        SCUPmdseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPmd_seas1[1], "-", input$SCUPmd_seas1[2]),
                                    BagLimit = paste(input$SCUPmd_1_bag),
                                    Length = paste(input$SCUPmd_1_len))
        SCUPmdFHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPmdFH_seas2[1], "-", input$SCUPmdFH_seas2[2]),
                                      BagLimit = paste(input$SCUPmdFH_2_bag),
                                      Length = paste(input$SCUPmdFH_2_len))
        SCUPmdPRseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPmdPR_seas2[1], "-", input$SCUPmdPR_seas2[2]),
                                      BagLimit = paste(input$SCUPmdPR_2_bag),
                                      Length = paste(input$SCUPmdPR_2_len))
        SCUPmdSHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPmdSH_seas2[1], "-", input$SCUPmdSH_seas2[2]),
                                      BagLimit = paste(input$SCUPmdSH_2_bag),
                                      Length = paste(input$SCUPmdSH_2_len))
        SCUPmd <- rbind(SCUPmdseason1, SCUPmdFHseason2, SCUPmdPRseason2, SCUPmdSHseason2)
      } else {
        SCUPmdFHseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPmdFH_seas1[1], "-", input$SCUPmdFH_seas1[2]),
                                      BagLimit = paste(input$SCUPmdFH_1_bag),
                                      Length = paste(input$SCUPmdFH_1_len))
        SCUPmdPRseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPmdPR_seas1[1], "-", input$SCUPmdPR_seas1[2]),
                                      BagLimit = paste(input$SCUPmdPR_1_bag),
                                      Length = paste(input$SCUPmdPR_1_len))
        SCUPmdSHseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPmdSH_seas1[1], "-", input$SCUPmdSH_seas1[2]),
                                      BagLimit = paste(input$SCUPmdSH_1_bag),
                                      Length = paste(input$SCUPmdSH_1_len))
        SCUPmdFHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPmdFH_seas2[1], "-", input$SCUPmdFH_seas2[2]),
                                      BagLimit = paste(input$SCUPmdFH_2_bag),
                                      Length = paste(input$SCUPmdFH_2_len))
        SCUPmdPRseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPmdPR_seas2[1], "-", input$SCUPmdPR_seas2[2]),
                                      BagLimit = paste(input$SCUPmdPR_2_bag),
                                      Length = paste(input$SCUPmdPR_2_len))
        SCUPmdSHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPmdSH_seas2[1], "-", input$SCUPmdSH_seas2[2]),
                                      BagLimit = paste(input$SCUPmdSH_2_bag),
                                      Length = paste(input$SCUPmdSH_2_len))
        
        SCUPmd <- rbind(SCUPmdFHseason1, SCUPmdFHseason2, SCUPmdPRseason1,  SCUPmdPRseason2,SCUPmdSHseason1,  SCUPmdSHseason2)
      }
      dat <- dat %>% rbind(SFmd, BSBmd, SCUPmd)
      
    }
    
    #### VA Regs ####
    if(any("VA" == input$state)){  
      if(input$SF_VA_input_type == "All Modes Combined"){
        
        SFvaseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFva_seas1[1], "-", input$SFva_seas1[2]),
                                  BagLimit = paste(input$SFva_1_bag),
                                  Length = paste(input$SFva_1_len))
        SFvaseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFva_seas2[1], "-", input$SFva_seas2[2]),
                                  BagLimit = paste(input$SFva_2_bag),
                                  Length = paste(input$SFva_2_len))
        SFvaFHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFvaFH_seas3[1], "-", input$SFvaFH_seas3[2]),
                                    BagLimit = paste(input$SFvaFH_3_bag),
                                    Length = paste(input$SFvaFH_3_len))
        SFvaPRseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFvaPR_seas3[1], "-", input$SFvaPR_seas3[2]),
                                    BagLimit = paste(input$SFvaPR_3_bag),
                                    Length = paste(input$SFvaPR_3_len))
        SFvaSHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFvaSH_seas3[1], "-", input$SFvaSH_seas3[2]),
                                    BagLimit = paste(input$SFvaSH_3_bag),
                                    Length = paste(input$SFvaSH_3_len))
        SFva <- rbind(SFvaseason1,SFvaseason2, SFvaFHseason3, SFvaPRseason3, SFvaSHseason3)
      } else {
        SFvaFHseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFvaFH_seas1[1], "-", input$SFvaFH_seas1[2]),
                                    BagLimit = paste(input$SFvaFH_1_bag),
                                    Length = paste(input$SFvaFH_1_len))
        SFvaPRseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFvaPR_seas1[1], "-", input$SFvaPR_seas1[2]),
                                    BagLimit = paste(input$SFvaPR_1_bag),
                                    Length = paste(input$SFvaPR_1_len))
        SFvaSHseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFvaSH_seas1[1], "-", input$SFvaSH_seas1[2]),
                                    BagLimit = paste(input$SFvaSH_1_bag),
                                    Length = paste(input$SFvaSH_1_len))
        SFvaFHseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFvaFH_seas2[1], "-", input$SFvaFH_seas2[2]),
                                    BagLimit = paste(input$SFvaFH_2_bag),
                                    Length = paste(input$SFvaFH_2_len))
        SFvaPRseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFvaPR_seas2[1], "-", input$SFvaPR_seas2[2]),
                                    BagLimit = paste(input$SFvaPR_2_bag),
                                    Length = paste(input$SFvaPR_2_len))
        SFvaSHseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFvaSH_seas2[1], "-", input$SFvaSH_seas2[2]),
                                    BagLimit = paste(input$SFvaSH_2_bag),
                                    Length = paste(input$SFvaSH_2_len))
        SFvaFHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFvaFH_seas3[1], "-", input$SFvaFH_seas3[2]),
                                    BagLimit = paste(input$SFvaFH_3_bag),
                                    Length = paste(input$SFvaFH_3_len))
        SFvaPRseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFvaPR_seas3[1], "-", input$SFvaPR_seas3[2]),
                                    BagLimit = paste(input$SFvaPR_3_bag),
                                    Length = paste(input$SFvaPR_3_len))
        SFvaSHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFvaSH_seas3[1], "-", input$SFvaSH_seas3[2]),
                                    BagLimit = paste(input$SFvaSH_3_bag),
                                    Length = paste(input$SFvaSH_3_len))
        
        SFva <- rbind(SFvaFHseason1,SFvaFHseason2,SFvaFHseason3, SFvaPRseason1,SFvaPRseason2, SFvaPRseason3, SFvaSHseason1,SFvaSHseason2, SFvaSHseason3)
      }
      
      if(input$BSB_VA_input_type == "All Modes Combined"){
        BSBvaseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBva_seas1[1], "-", input$BSBva_seas1[2]),
                                   BagLimit = paste(input$BSBva_1_bag),
                                   Length = paste(input$BSBva_1_len))
        BSBvaseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBva_seas2[1], "-", input$BSBva_seas2[2]),
                                   BagLimit = paste(input$BSBva_2_bag),
                                   Length = paste(input$BSBva_2_len))
        BSBvaFHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBvaFH_seas3[1], "-", input$BSBvaFH_seas3[2]),
                                     BagLimit = paste(input$BSBvaFH_3_bag),
                                     Length = paste(input$BSBvaFH_3_len))
        BSBvaPRseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBvaPR_seas3[1], "-", input$BSBvaPR_seas3[2]),
                                     BagLimit = paste(input$BSBvaPR_3_bag),
                                     Length = paste(input$BSBvaPR_3_len))
        BSBvaSHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBvaSH_seas3[1], "-", input$BSBvaSH_seas3[2]),
                                     BagLimit = paste(input$BSBvaSH_3_bag),
                                     Length = paste(input$BSBvaSH_3_len))
        BSBva<- rbind(BSBvaseason1, BSBvaseason2,BSBvaFHseason3,BSBvaPRseason3,BSBvaSHseason3)
      } else {
        BSBvaFHseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBvaFH_seas1[1], "-", input$BSBvaFH_seas1[2]),
                                     BagLimit = paste(input$BSBvaFH_1_bag),
                                     Length = paste(input$BSBvaFH_1_len))
        BSBvaFHseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBvaFH_seas2[1], "-", input$BSBvaFH_seas2[2]),
                                     BagLimit = paste(input$BSBvaFH_2_bag),
                                     Length = paste(input$BSBvaFH_2_len))
        BSBvaFHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBvaFH_seas3[1], "-", input$BSBvaFH_seas3[2]),
                                     BagLimit = paste(input$BSBvaFH_3_bag),
                                     Length = paste(input$BSBvaFH_3_len))
        
        BSBvaPRseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBvaPR_seas1[1], "-", input$BSBvaPR_seas1[2]),
                                     BagLimit = paste(input$BSBvaPR_1_bag),
                                     Length = paste(input$BSBvaPR_1_len))
        BSBvaPRseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBvaPR_seas2[1], "-", input$BSBvaPR_seas2[2]),
                                     BagLimit = paste(input$BSBvaPR_2_bag),
                                     Length = paste(input$BSBvaPR_2_len))
        BSBvaPRseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBvaPR_seas3[1], "-", input$BSBvaPR_seas3[2]),
                                     BagLimit = paste(input$BSBvaPR_3_bag),
                                     Length = paste(input$BSBvaPR_3_len))
        
        BSBvaSHseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBvaSH_seas1[1], "-", input$BSBvaSH_seas1[2]),
                                     BagLimit = paste(input$BSBvaSH_1_bag),
                                     Length = paste(input$BSBvaSH_1_len))
        BSBvaSHseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBvaSH_seas2[1], "-", input$BSBvaSH_seas2[2]),
                                     BagLimit = paste(input$BSBvaSH_2_bag),
                                     Length = paste(input$BSBvaSH_2_len))
        BSBvaSHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBvaSH_seas3[1], "-", input$BSBvaSH_seas3[2]),
                                     BagLimit = paste(input$BSBvaSH_3_bag),
                                     Length = paste(input$BSBvaSH_3_len))
        
        BSBva<- rbind(BSBvaFHseason1, BSBvaFHseason2, BSBvaFHseason3,BSBvaPRseason1, BSBvaPRseason2,
                      BSBvaPRseason3,BSBvaSHseason1, BSBvaSHseason2, BSBvaSHseason3)
      }
      
      
      
      if(input$SCUP_VA_input_type == "All Modes Combined"){
        SCUPvaseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPva_seas1[1], "-", input$SCUPva_seas1[2]),
                                    BagLimit = paste(input$SCUPva_1_bag),
                                    Length = paste(input$SCUPva_1_len))
        SCUPvaFHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPvaFH_seas2[1], "-", input$SCUPvaFH_seas2[2]),
                                      BagLimit = paste(input$SCUPvaFH_2_bag),
                                      Length = paste(input$SCUPvaFH_2_len))
        SCUPvaPRseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPvaPR_seas2[1], "-", input$SCUPvaPR_seas2[2]),
                                      BagLimit = paste(input$SCUPvaPR_2_bag),
                                      Length = paste(input$SCUPvaPR_2_len))
        SCUPvaSHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPvaSH_seas2[1], "-", input$SCUPvaSH_seas2[2]),
                                      BagLimit = paste(input$SCUPvaSH_2_bag),
                                      Length = paste(input$SCUPvaSH_2_len))
        SCUPva <- rbind(SCUPvaseason1, SCUPvaFHseason2, SCUPvaPRseason2, SCUPvaSHseason2)
      } else {
        SCUPvaFHseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPvaFH_seas1[1], "-", input$SCUPvaFH_seas1[2]),
                                      BagLimit = paste(input$SCUPvaFH_1_bag),
                                      Length = paste(input$SCUPvaFH_1_len))
        SCUPvaPRseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPvaPR_seas1[1], "-", input$SCUPvaPR_seas1[2]),
                                      BagLimit = paste(input$SCUPvaPR_1_bag),
                                      Length = paste(input$SCUPvaPR_1_len))
        SCUPvaSHseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPvaSH_seas1[1], "-", input$SCUPvaSH_seas1[2]),
                                      BagLimit = paste(input$SCUPvaSH_1_bag),
                                      Length = paste(input$SCUPvaSH_1_len))
        SCUPvaFHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPvaFH_seas2[1], "-", input$SCUPvaFH_seas2[2]),
                                      BagLimit = paste(input$SCUPvaFH_2_bag),
                                      Length = paste(input$SCUPvaFH_2_len))
        SCUPvaPRseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPvaPR_seas2[1], "-", input$SCUPvaPR_seas2[2]),
                                      BagLimit = paste(input$SCUPvaPR_2_bag),
                                      Length = paste(input$SCUPvaPR_2_len))
        SCUPvaSHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPvaSH_seas2[1], "-", input$SCUPvaSH_seas2[2]),
                                      BagLimit = paste(input$SCUPvaSH_2_bag),
                                      Length = paste(input$SCUPvaSH_2_len))
        
        SCUPva <- rbind(SCUPvaFHseason1, SCUPvaFHseason2, SCUPvaPRseason1,  SCUPvaPRseason2,SCUPvaSHseason1,  SCUPvaSHseason2)
      }
      dat <- dat %>% rbind(SFva, BSBva, SCUPva)
      
    }
    
    #### NC Regs ####
    if(any("NC" == input$state)){  
      if(input$SF_NC_input_type == "All Modes Combined"){
        
        SFncseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("All"),
                                  Season = paste(input$SFnc_seas1[1], "-", input$SFnc_seas1[2]),
                                  BagLimit = paste(input$SFnc_1_bag),
                                  Length = paste(input$SFnc_1_len))
        SFncFHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFncFH_seas2[1], "-", input$SFncFH_seas2[2]),
                                    BagLimit = paste(input$SFncFH_2_bag),
                                    Length = paste(input$SFncFH_2_len))
        SFncPRseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFncPR_seas2[1], "-", input$SFncPR_seas2[2]),
                                    BagLimit = paste(input$SFncPR_2_bag),
                                    Length = paste(input$SFncPR_2_len))
        SFncSHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFncSH_seas2[1], "-", input$SFncSH_seas2[2]),
                                    BagLimit = paste(input$SFncSH_2_bag),
                                    Length = paste(input$SFncSH_2_len))
        SFnc <- rbind(SFncseason1, SFncFHseason2, SFncPRseason2, SFncSHseason2)
      } else {
        SFncFHseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFncFH_seas1[1], "-", input$SFncFH_seas1[2]),
                                    BagLimit = paste(input$SFncFH_1_bag),
                                    Length = paste(input$SFncFH_1_len))
        SFncPRseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFncPR_seas1[1], "-", input$SFncPR_seas1[2]),
                                    BagLimit = paste(input$SFncPR_1_bag),
                                    Length = paste(input$SFncPR_1_len))
        SFncSHseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFncSH_seas1[1], "-", input$SFncSH_seas1[2]),
                                    BagLimit = paste(input$SFncSH_1_bag),
                                    Length = paste(input$SFncSH_1_len))
        SFncFHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("For Hire"),
                                    Season = paste(input$SFncFH_seas2[1], "-", input$SFncFH_seas2[2]),
                                    BagLimit = paste(input$SFncFH_2_bag),
                                    Length = paste(input$SFncFH_2_len))
        SFncPRseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Private"),
                                    Season = paste(input$SFncPR_seas2[1], "-", input$SFncPR_seas2[2]),
                                    BagLimit = paste(input$SFncPR_2_bag),
                                    Length = paste(input$SFncPR_2_len))
        SFncSHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Shore"),
                                    Season = paste(input$SFncSH_seas2[1], "-", input$SFncSH_seas2[2]),
                                    BagLimit = paste(input$SFncSH_2_bag),
                                    Length = paste(input$SFncSH_2_len))
        
        SFnc <- rbind(SFncFHseason1,SFncFHseason2, SFncPRseason1,SFncPRseason2, SFncSHseason1,SFncSHseason2)
      }
      
      if(input$BSB_NC_input_type == "All Modes Combined"){
        BSBncseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBnc_seas1[1], "-", input$BSBnc_seas1[2]),
                                   BagLimit = paste(input$BSBnc_1_bag),
                                   Length = paste(input$BSBnc_1_len))
        BSBncseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("All"),
                                   Season = paste(input$BSBnc_seas2[1], "-", input$BSBnc_seas2[2]),
                                   BagLimit = paste(input$BSBnc_2_bag),
                                   Length = paste(input$BSBnc_2_len))
        BSBncFHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBncFH_seas3[1], "-", input$BSBncFH_seas3[2]),
                                     BagLimit = paste(input$BSBncFH_3_bag),
                                     Length = paste(input$BSBncFH_3_len))
        BSBncPRseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBncPR_seas3[1], "-", input$BSBncPR_seas3[2]),
                                     BagLimit = paste(input$BSBncPR_3_bag),
                                     Length = paste(input$BSBncPR_3_len))
        BSBncSHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBncSH_seas3[1], "-", input$BSBncSH_seas3[2]),
                                     BagLimit = paste(input$BSBncSH_3_bag),
                                     Length = paste(input$BSBncSH_3_len))
        BSBnc<- rbind(BSBncseason1, BSBncseason2,BSBncFHseason3,BSBncPRseason3,BSBncSHseason3)
      } else {
        BSBncFHseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBncFH_seas1[1], "-", input$BSBncFH_seas1[2]),
                                     BagLimit = paste(input$BSBncFH_1_bag),
                                     Length = paste(input$BSBncFH_1_len))
        BSBncFHseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBncFH_seas2[1], "-", input$BSBncFH_seas2[2]),
                                     BagLimit = paste(input$BSBncFH_2_bag),
                                     Length = paste(input$BSBncFH_2_len))
        BSBncFHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
                                     Season = paste(input$BSBncFH_seas3[1], "-", input$BSBncFH_seas3[2]),
                                     BagLimit = paste(input$BSBncFH_3_bag),
                                     Length = paste(input$BSBncFH_3_len))
        
        BSBncPRseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBncPR_seas1[1], "-", input$BSBncPR_seas1[2]),
                                     BagLimit = paste(input$BSBncPR_1_bag),
                                     Length = paste(input$BSBncPR_1_len))
        BSBncPRseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBncPR_seas2[1], "-", input$BSBncPR_seas2[2]),
                                     BagLimit = paste(input$BSBncPR_2_bag),
                                     Length = paste(input$BSBncPR_2_len))
        BSBncPRseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
                                     Season = paste(input$BSBncPR_seas3[1], "-", input$BSBncPR_seas3[2]),
                                     BagLimit = paste(input$BSBncPR_3_bag),
                                     Length = paste(input$BSBncPR_3_len))
        
        BSBncSHseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBncSH_seas1[1], "-", input$BSBncSH_seas1[2]),
                                     BagLimit = paste(input$BSBncSH_1_bag),
                                     Length = paste(input$BSBncSH_1_len))
        BSBncSHseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBncSH_seas2[1], "-", input$BSBncSH_seas2[2]),
                                     BagLimit = paste(input$BSBncSH_2_bag),
                                     Length = paste(input$BSBncSH_2_len))
        BSBncSHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
                                     Season = paste(input$BSBncSH_seas3[1], "-", input$BSBncSH_seas3[2]),
                                     BagLimit = paste(input$BSBncSH_3_bag),
                                     Length = paste(input$BSBncSH_3_len))
        
        BSBnc<- rbind(BSBncFHseason1, BSBncFHseason2, BSBncFHseason3,BSBncPRseason1, BSBncPRseason2,
                      BSBncPRseason3,BSBncSHseason1, BSBncSHseason2, BSBncSHseason3)
      }
      
      
      
      if(input$SCUP_NC_input_type == "All Modes Combined"){
        SCUPncseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("All"),
                                    Season = paste(input$SCUPnc_seas1[1], "-", input$SCUPnc_seas1[2]),
                                    BagLimit = paste(input$SCUPnc_1_bag),
                                    Length = paste(input$SCUPnc_1_len))
        SCUPncFHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPncFH_seas2[1], "-", input$SCUPncFH_seas2[2]),
                                      BagLimit = paste(input$SCUPncFH_2_bag),
                                      Length = paste(input$SCUPncFH_2_len))
        SCUPncPRseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPncPR_seas2[1], "-", input$SCUPncPR_seas2[2]),
                                      BagLimit = paste(input$SCUPncPR_2_bag),
                                      Length = paste(input$SCUPncPR_2_len))
        SCUPncSHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPncSH_seas2[1], "-", input$SCUPncSH_seas2[2]),
                                      BagLimit = paste(input$SCUPncSH_2_bag),
                                      Length = paste(input$SCUPncSH_2_len))
        SCUPnc <- rbind(SCUPncseason1, SCUPncFHseason2, SCUPncPRseason2, SCUPncSHseason2)
      } else {
        SCUPncFHseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPncFH_seas1[1], "-", input$SCUPncFH_seas1[2]),
                                      BagLimit = paste(input$SCUPvFH_1_bag),
                                      Length = paste(input$SCUPncFH_1_len))
        SCUPncPRseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPncPR_seas1[1], "-", input$SCUPncPR_seas1[2]),
                                      BagLimit = paste(input$SCUPncPR_1_bag),
                                      Length = paste(input$SCUPncPR_1_len))
        SCUPncSHseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPncSH_seas1[1], "-", input$SCUPncSH_seas1[2]),
                                      BagLimit = paste(input$SCUPncSH_1_bag),
                                      Length = paste(input$SCUPncSH_1_len))
        SCUPncFHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("For Hire"),
                                      Season = paste(input$SCUPncFH_seas2[1], "-", input$SCUPncFH_seas2[2]),
                                      BagLimit = paste(input$SCUPncFH_2_bag),
                                      Length = paste(input$SCUPncFH_2_len))
        SCUPncPRseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Private"),
                                      Season = paste(input$SCUPncPR_seas2[1], "-", input$SCUPncPR_seas2[2]),
                                      BagLimit = paste(input$SCUPncPR_2_bag),
                                      Length = paste(input$SCUPncPR_2_len))
        SCUPncSHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Shore"),
                                      Season = paste(input$SCUPncSH_seas2[1], "-", input$SCUPncSH_seas2[2]),
                                      BagLimit = paste(input$SCUPncSH_2_bag),
                                      Length = paste(input$SCUPncSH_2_len))
        
        SCUPnc <- rbind(SCUPncFHseason1, SCUPncFHseason2, SCUPncPRseason1,  SCUPncPRseason2,SCUPncSHseason1,  SCUPncSHseason2)
      }
      dat <- dat %>% rbind(SFnc, BSBnc, SCUPnc)
      
    }
    
    
    regs_output<- dat %>% 
      dplyr::filter(!BagLimit == "0",
                    !BagLimit == "0 , 0") %>%
      dplyr::mutate(Season = stringr::str_remove(Season, pattern = "2023-"),
                    Season = stringr::str_remove(Season, pattern = "2023-"))
    return(regs_output)
  })
  
  
  #### Output Tables #### 
  output$keep_tableout<- renderTable({
    keep()
  })
  
  output$welfare_tableout<- renderTable({
    welfare()
  })
  
  output$ntrips_tableout<- renderTable({
    ntrips()
  })
  
  output$regtableout <- renderTable({
    regulations()
  })
  
  output$releaseout <- renderTable({
    release()
  })
  
  output$fig <- renderPlot({
    dat<- keep_draws()
    
    draws<- dat %>% 
      dplyr::filter(draw != "Summary", 
                    Statistic %in% c("harvest pounds"))
    
    summary <-  dat %>% 
      dplyr::filter(draw == "Summary", 
                    Statistic %in% c("harvest pounds"))
    
    
    dat %>% 
      ggplot2::ggplot()+
      ggplot2::geom_violin(ggplot2::aes(x = `State`, y = as.numeric(`Alternative option value`, color = `State`)))+
      ggplot2::facet_wrap(Mode~Species, scales = "free_y")+
      ggplot2::geom_hline(yintercept = summary$`Alternative option value`)

  })
  
  #### Save Raw Data
  observeEvent(input$runmeplease, {
    dat<- predictions_1()
    
    dat_out<- dat
    Run_Name = Run_Name()
    readr::write_csv(dat_out, file = here::here(paste0("output/output_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
    
  })
  
  
  
  
  #### Download Button ####
  output$downloadData <- downloadHandler(
    filename = function(){"RecDSToutput.xlsx"},
    content = function(filename) {
      
      df_list <- list(Regulations=regulations(), Harvest=keep(), Releases = release(),
                      Change_Angler_Satisfaction = welfare(), Estimated_Trips = ntrips(), Harvest_Draws = keep_draws(), 
                      Releases_Draws = release_draws(), Change_Angler_Satisfaction_Draw = welfare_draws(), Estimated_Trips_Draws = ntrips_draws())
      openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
    })
  
  getPage<-function() {
    return(includeHTML(here::here("docs/documentation.html")))
  }
  output$documentation<-renderUI({getPage()})
  
}

shiny::shinyApp(ui = ui, server = server)