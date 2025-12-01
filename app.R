
# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Recreational Fisheries Decision Support Tool"),
  tabsetPanel(
    tabPanel("Summary Page",
             plotly::plotlyOutput(outputId = "summary_rhl_fig"),
             shiny::h2("Summary Table"), 
             DT::DTOutput(outputId = "summary_percdiff_table"),
             
             ### Figure and table output by state
             tabsetPanel(
               tabPanel("MA", 
                        shiny::h2("Massachusetts"),
                        plotly::plotlyOutput(outputId = "ma_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "ma_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "ma_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "ma_discards_fig") # Disczrds)
               ),
               tabPanel("RI", 
                        shiny::h2("Rhode Island"),
                        plotly::plotlyOutput(outputId = "ri_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "ri_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "ri_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "ri_discards_fig") # Disczrds)
               ), 
               tabPanel("CT", 
                        shiny::h2("Connecticut"),
                        plotly::plotlyOutput(outputId = "ct_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "ct_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "ct_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "ct_discards_fig") # Disczrds)
               ),
               tabPanel("NY", 
                        shiny::h2("New York"),
                        plotly::plotlyOutput(outputId = "ny_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "ny_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "ny_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "ny_discards_fig") # Disczrds)
               ),
               tabPanel("NJ", 
                        shiny::h2("New Jersey"),
                        plotly::plotlyOutput(outputId = "nj_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "nj_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "nj_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "nj_discards_fig") # Disczrds)
               ),
               tabPanel("DE", 
                        shiny::h2("Delaware"),
                        plotly::plotlyOutput(outputId = "de_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "de_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "de_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "de_discards_fig") # Disczrds)
               ),
               tabPanel("MD", 
                        shiny::h2("Marlyand"),
                        plotly::plotlyOutput(outputId = "md_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "md_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "md_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "md_discards_fig") # Disczrds)
               ),
               tabPanel("VA", 
                        shiny::h2("Virginia"),
                        plotly::plotlyOutput(outputId = "va_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "va_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "va_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "va_discards_fig") # Disczrds)
               ),
               tabPanel("NC", 
                        shiny::h2("North Carolina"),
                        plotly::plotlyOutput(outputId = "nc_rhl_fig"),# Harvest
                        plotly::plotlyOutput(outputId = "nc_CV_fig"),# Angler Satis
                        plotly::plotlyOutput(outputId = "nc_trips_fig"), # Ntrips
                        plotly::plotlyOutput(outputId = "nc_discards_fig") # Disczrds)
               ), 
               tabPanel("Regulations", 
                        shiny::h2("Regulations"),
                        selectInput( "file_choice",
                                     "Choose a file to download:",
                                     choices = NULL,  # Will be populated in server
                                     selected = NULL ),
                        downloadButton( "download_file",
                                        "Download Selected File",
                                        class = "btn-primary"),
                        DT::DTOutput(outputId = "summary_regs_table"))
               
             )),
    
    
    tabPanel( "Regulation Selection",
              strong(div("INSTRUCTIONS: (1) Give your policy a name, (2) Select one or more states,  (3) Select regulations, (4) Click run me and wait for the model to run, (5) Use the `Results` tab to examine the results.", style = "color:blue")), # Warning for users
              # Collect the Run Name
              textInput("Run_Name", "Please give your policy a unique name using your initials and a number (ex. AB1)."),
              
              shinyWidgets::awesomeCheckboxGroup( # Select which state(s) to run
                inputId = "state", 
                label = "State", 
                choices = c("MA", "RI", "CT", "NY", "NJ", "DE",  "MD", "VA", "NC"),
                inline = TRUE,
                status = "danger"),
              
              #Run Button
              actionButton("runmeplease", "Run Me"), 
              
              textOutput("message"),
              # Add UI code for each state
              uiOutput("addMA"),
              uiOutput("addRI"),
              uiOutput("addCT"), 
              uiOutput("addNY"),
              uiOutput("addNJ"), 
              uiOutput("addDE"),
              uiOutput("addMD"),
              uiOutput("addVA"), 
              uiOutput("addNC"))
  ))

####### Start Server ###################
server <- function(input, output, session) {
  
  library(magrittr) 
  
  ### Percent Change Approach
  sf_percent_change <- 10
  bsb_percent_change <- 10
  scup_percent_change <- 10
  
  sf_rhl <- function(){
    sf_rhl = 99
    return(sf_rhl)
  }
  
  bsb_rhl <- function(){
    bsb_rhl = 99
    return(bsb_rhl)
  }
  
  scup_rhl <- function(){
    scup_rhl = 99
    return(scup_rhl)
  }
  
  mytimeFormat <- "%b %d"
  
  date_slider_defaults <- list(
    min = as.Date("01-01", "%m-%d"),
    max = as.Date("12-31", "%m-%d"),
    timeFormat = "%b %d",
    ticks = FALSE
  )
  
  
  
  Run_Name <- function(){
    if(stringr::str_detect(input$Run_Name, "_")){
      Run_Name <-  gsub("_", "-", input$Run_Name)
    }else {
      Run_Name <- input$Run_Name
    }
    print(Run_Name)
    return(Run_Name)
  }
  
  
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
               rlang::exec(sliderInput, inputId= "SFmaFH_seas1", label ="For Hire Season 1", 
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SFmaFH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 17.5, step = .5))),
               rlang::exec(sliderInput, inputId= "SFmaPR_seas1", label ="Private Season 1", 
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SFmaPR_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 17.5, step = .5))),
               rlang::exec(sliderInput, inputId= "SFmaSH_seas1", label ="Shore  Season 1", 
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SFmaSH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 16.5, step = .5))),
               
               actionButton("SFMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmaSeason2",
                                    rlang::exec(sliderInput, inputId= "SFmaFH_seas2", label ="For Hire  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFmaFH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFmaPR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFmaPR_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFmaSH_seas2", label ="Shore  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFmaSH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MA"),
               
               selectInput("BSB_MA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBmaMode"),
               
               
               actionButton("BSBMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmaSeason2",
                                    rlang::exec(sliderInput, inputId= "BSBmaFH_seas2", label ="For Hire  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBmaFH_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBmaPR_seas2", label ="Private  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBmaPR_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBmaSH_seas2", label ="Shore  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBmaSH_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MA"),
               rlang::exec(sliderInput,inputId = "SCUPmaFH_seas1", label ="For Hire  Season 1", 
                           value =c(as.Date("05-01","%m-%d"),as.Date("06-30","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPmaFH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))),
               
               rlang::exec(sliderInput,inputId = "SCUPmaFH_seas2", label ="For Hire  Season 2", 
                           value =c(as.Date("07-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPmaFH_2_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))), 
               
               
               rlang::exec(sliderInput,inputId = "SCUPmaPR_seas1", label ="Private  Season 1", 
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPmaPR_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))),
               rlang::exec(sliderInput,inputId = "SCUPmaSH_seas1", label ="Shore  Season 1", 
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPmaSH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 9.5, step = .5))),
               
               actionButton("SCUPMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmaSeason2",
                                    rlang::exec(sliderInput,inputId = "SCUPmaFH_seas3", label ="For Hire  Season 3", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPmaFH_3_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPmaPR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPmaPR_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPmaSH_seas2", label ="Shore  Season 2", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPmaSH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5)))))))
    }})
  
  
  ############# MA Breakout by mode ######################################
  output$SFmaMode <- renderUI({
    if (is.null(input$SF_MA_input_type))
      return()
    
    switch(input$SF_MA_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFma_seas1", label =" Season 1",
                                                  value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFma_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 5)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFma_1_len", label ="Min Length",
                                                           min = 5, max = 25, value = 16.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFmaFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmaFH_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFmaPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmaPR_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFmaSH_seas1", label ="Shore  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmaSH_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5)))))
  })
  
  
  output$BSBmaMode <- renderUI({
    if (is.null(input$BSB_MA_input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component. i.e. when all modes combined is selected only one
    switch(input$BSB_MA_input_type,
           
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "BSBma_seas1", label =" Season 1", 
                                                  value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBma_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 4)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBma_1_len", label ="Min Length",
                                                           min = 3, max = 25, value = 16.5, step = .5)))),
           
           
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "BSBmaFH_seas1", label =" For Hire  Season 1", 
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmaFH_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5))),
                                     rlang::exec(sliderInput,inputId = "BSBmaPR_seas1", label ="Private/Rental  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmaPR_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5))),
                                     rlang::exec(sliderInput,inputId = "BSBmaSH_seas1", label ="Shore  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmaSH_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5)))))
    
  })
  
  
  ############## RHODE ISLAND ###########################################################
  output$addRI <- renderUI({
    if(any("RI" == input$state)){
      fluidRow( 
        style = "background-color: #B3CDE3;",
        column(4,
               titlePanel("Summer Flounder - RI"),
               
               selectInput("SF_RI_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFriMode"),
               
               actionButton("SFRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFriSeason2",
                                    rlang::exec(sliderInput,inputId = "SFriFH_seas2", label ="For Hire  Season 2",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFriFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SFriPR_seas2", label ="Private  Season 2",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFriPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SFriSH_seas2", label ="Shore  Season 2",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFriSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - RI"),
               
               rlang::exec(sliderInput,inputId = "BSBriFH_seas1", label ="For Hire  Season 1",
                           
                           
                           value=c(as.Date("06-18","%m-%d"),as.Date("08-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBriFH_1_len", label ="Min Length",
                                    min = 11, max =18, value = 16, step = .5))),
               rlang::exec(sliderInput,inputId = "BSBriFH_seas2", label ="For Hire  Season 2",
                           
                           
                           value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 6)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBriFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBriPR_seas1", label ="Private  Season 1",
                           
                           
                           value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBriPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBriPR_seas2", label ="Private  Season 2",
                           
                           
                           value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 3)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBriPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBriSH_seas1", label ="Shore  Season 1",
                           
                           
                           value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBriSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBriSH_seas2", label ="Shore  Season 2",
                           
                           
                           value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 3)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBriSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               actionButton("BSBRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBriSeason2",
                                    rlang::exec(sliderInput,inputId = "BSBriFH_seas3", label ="For Hire  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBriFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    rlang::exec(sliderInput,inputId = "BSBriPR_seas3", label ="Private  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBriPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    rlang::exec(sliderInput,inputId = "BSBriSH_seas3", label ="Shore  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBriSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - RI"),
               rlang::exec(sliderInput,inputId = "SCUPriFH_seas1", label ="For Hire  Season 1", 
                           
                           
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPriFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               rlang::exec(sliderInput,inputId = "SCUPriFH_seas2", label ="For Hire  Season 2", 
                           
                           
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPriFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               rlang::exec(sliderInput,inputId = "SCUPriFH_seas3", label ="For Hire  Season 3", 
                           
                           
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPriFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               rlang::exec(sliderInput,inputId = "SCUPriPR_seas1", label ="Private  Season 1", 
                           
                           
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPriPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               rlang::exec(sliderInput,inputId = "SCUPriSH_seas1", label ="Shore  Season 1", 
                           
                           
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPriSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPriSeason2",
                                    rlang::exec(sliderInput,inputId = "SCUPriFH_seas4", label ="For Hire  Season 4", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPriFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPriPR_seas2", label ="Private  Season 2", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPriPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPriSH_seas2", label ="Shore  Season 2", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPriSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# RI Breakout by mode ######################################
  output$SFriMode <- renderUI({
    if (is.null(input$SF_RI_input_type))
      return()
    
    switch(input$SF_RI_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFri_seas1", label =" Season 1",
                                                  
                                                  
                                                  value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFri_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFri_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFriFH_seas1", label ="For Hire  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFriFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFriPR_seas1", label ="Private  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFriPR_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 19, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFriSH_seas1", label ="Shore  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFriSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5)))))
  })
  
  ############## CONNECTICUT ###########################################################
  output$addCT <- renderUI({
    if(any("CT" == input$state)){
      fluidRow( 
        style = "background-color: #CCEBC5;",
        column(4,
               titlePanel("Summer Flounder - CT"),
               
               selectInput("SF_CT_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFctMode"),
               
               actionButton("SFCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFctSeason3",
                                    rlang::exec(sliderInput,inputId = "SFctFH_seas3", label ="For Hire  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFctFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SFctPR_seas3", label ="Private  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFctPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SFctSH_seas3", label ="Shore  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFctSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - CT"),
               
               rlang::exec(sliderInput,inputId = "BSBctFH_seas1", label ="For Hire  Season 1",
                           
                           
                           value=c(as.Date("05-18","%m-%d"),as.Date("08-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBctFH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               rlang::exec(sliderInput,inputId = "BSBctFH_seas2", label ="For Hire  Season 2",
                           
                           
                           value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 7)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBctFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBctPR_seas1", label ="Private  Season 1",
                           
                           
                           value=c(as.Date("05-18","%m-%d"),as.Date("06-23","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBctPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBctPR_seas2", label ="Private  Season 2",
                           
                           
                           value=c(as.Date("07-08","%m-%d"),as.Date("11-28","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBctPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBctSH_seas1", label ="Shore  Season 1",
                           
                           
                           value=c(as.Date("05-18","%m-%d"),as.Date("06-23","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBctSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               rlang::exec(sliderInput,inputId = "BSBctSH_seas2", label ="Shore  Season 2",
                           
                           
                           value=c(as.Date("07-08","%m-%d"),as.Date("11-28","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        rlang::exec(sliderInput, inputId= "BSBctSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               actionButton("BSBCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBctSeason2",
                                    rlang::exec(sliderInput,inputId = "BSBctFH_seas3", label ="For Hire  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBctFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    rlang::exec(sliderInput,inputId = "BSBctPR_seas3", label ="Private  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBctPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    rlang::exec(sliderInput,inputId = "BSBctSH_seas3", label ="Shore  Season 3",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBctSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - CT"),
               rlang::exec(sliderInput,inputId = "SCUPctFH_seas1", label ="For Hire  Season 1",
                           
                           
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPctFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               rlang::exec(sliderInput,inputId = "SCUPctFH_seas2", label ="For Hire  Season 2", 
                           
                           
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPctFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               rlang::exec(sliderInput,inputId = "SCUPctFH_seas3", label ="For Hire  Season 3", 
                           
                           
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPctFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               rlang::exec(sliderInput,inputId = "SCUPctPR_seas1", label ="Private  Season 1", 
                           
                           
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPctPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               rlang::exec(sliderInput,inputId = "SCUPctSH_seas1", label ="Shore  Season 1", 
                           
                           
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPctSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPctSeason2",
                                    rlang::exec(sliderInput,inputId = "SCUPctFH_seas4", label ="For Hire  Season 4", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPctFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPctPR_seas2", label ="Private  Season 2", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPctPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPctSH_seas2", label ="Shore  Season 2", 
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPctSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# CT Breakout by mode ######################################
  output$SFctMode <- renderUI({
    if (is.null(input$SF_CT_input_type))
      return()
    
    switch(input$SF_CT_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFct_seas1", label =" Season 1",
                                                  
                                                  
                                                  value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFct_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      rlang::exec(sliderInput,inputId = "SFct_seas2", label =" Season 2",
                                                  
                                                  
                                                  value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFct_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFctFH_seas1", label ="For Hire  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFctFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFctPR_seas1", label ="Private  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFctPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFctSH_seas1", label ="Shore  Season 1",
                                                 
                                                 
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFctSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     rlang::exec(sliderInput,inputId = "SFctFH_seas2", label ="For Hire  Season 2",
                                                 
                                                 
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFctFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFctPR_seas2", label ="Private  Season 2",
                                                 
                                                 
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFctPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     rlang::exec(sliderInput,inputId = "SFctSH_seas2", label ="Shore  Season 2",
                                                 
                                                 
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 2)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFctSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
    
  })
  
  
  
  ############# NEW YORK #######################
  output$addNY <- renderUI({
    if(any("NY" == input$state)){
      fluidRow( 
        style = "background-color: #DECBE4;",
        column(4,
               titlePanel("Summer Flounder - NY"),
               
               selectInput("SF_NY_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFnyMode"),
               
               actionButton("SFNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnySeason3",
                                    rlang::exec(sliderInput,inputId = "SFnyFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFnyFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SFnyPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFnyPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SFnySH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFnySH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NY"),
               
               selectInput("BSB_NY_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBnyMode"),
               
               
               actionButton("BSBNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBnySeason3",
                                    rlang::exec(sliderInput,inputId = "BSBnyFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBnyFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    rlang::exec(sliderInput,inputId = "BSBnyPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBnyPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    rlang::exec(sliderInput,inputId = "BSBnySH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBnySH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NY"),
               rlang::exec(sliderInput,inputId = "SCUPnyFH_seas1", label ="For Hire  Season 1", 
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPnyFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               rlang::exec(sliderInput,inputId = "SCUPnyFH_seas2", label ="For Hire  Season 2", 
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPnyFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               rlang::exec(sliderInput,inputId = "SCUPnyFH_seas3", label ="For Hire  Season 3", 
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPnyFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               rlang::exec(sliderInput,inputId = "SCUPnyPR_seas1", label ="Private  Season 1", 
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPnyPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               rlang::exec(sliderInput,inputId = "SCUPnySH_seas1", label ="Shore  Season 1", 
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           !!!date_slider_defaults),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnySH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        rlang::exec(sliderInput, inputId= "SCUPnySH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnySeason2",
                                    rlang::exec(sliderInput,inputId = "SCUPnyFH_seas4", label ="For Hire  Season 4", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPnyFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPnyPR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPnyPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput,inputId = "SCUPnySH_seas2", label ="Shore  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnySH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPnySH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  ############# NY Breakout by mode ######################################
  output$SFnyMode <- renderUI({
    if (is.null(input$SF_NY_input_type))
      return()
    
    switch(input$SF_NY_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFny_seas1", label =" Season 1",
                                                  value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFny_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      rlang::exec(sliderInput,inputId = "SFny_seas2", label =" Season 2",
                                                  value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFny_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFnyFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFnyFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFnyPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFnyPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFnySH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFnySH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     rlang::exec(sliderInput, inputId= "SFnyFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFnyFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFnyPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFnyPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFnySH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFnySH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
  })
  
  
  output$BSBnyMode <- renderUI({
    if (is.null(input$BSB_NY_input_type))
      return()
    
    switch(input$BSB_NY_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "BSBny_seas1", label =" Season 1",
                                                  value=c(as.Date("06-23","%m-%d"),as.Date("8-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBny_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5))), 
                                      
                                      rlang::exec(sliderInput, inputId= "BSBny_seas2", label =" Season 2",
                                                  value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBny_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput, inputId = "BSBnyFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBnyFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBnyPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBnyPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBnySH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBnySH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))), 
                                     rlang::exec(sliderInput, inputId= "BSBnyFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBnyFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBnyPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBnyPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBnySH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBnySH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5)))))
  })
  
  ############## NEW JERSEY ############################################################
  output$addNJ <- renderUI({
    if(any("NJ" == input$state)){
      fluidRow( 
        style = "background-color: #FED9A6;",
        column(4,
               titlePanel("Summer Flounder - NJ"),
               
               selectInput("SF_NJ_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFnjMode"),
               
               
               actionButton("SFNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnjSeason2",
                                    rlang::exec(sliderInput, inputId= "SFnjFH_seas2", label ="For Hire  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),#)),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 7, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFnjFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))),
                                    rlang::exec(sliderInput, inputId= "SFnjPR_seas2", label ="Private/Rental  Season 2",  
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFnjPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5))),
                                    rlang::exec(sliderInput, inputId= "SFnjSH_seas2", label ="Shore  Season 2",  
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFnjSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NJ"),
               
               selectInput("BSB_NJ_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBnjMode"),
               
               actionButton("BSBNJaddSeason", "Add Season"), 
               #Season 5
               shinyjs::hidden( div(ID = "BSBnjSeason5",
                                    rlang::exec(sliderInput, inputId= "BSBnjFH_seas5", label =" For Hire  Season 5", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjFH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBnjFH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBnjPR_seas5", label ="Private/Rental  Season 5",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjPR_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBnjPR_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBnjSH_seas5", label ="Shore  Season 5",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjSH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBnjSH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5)))))),
        
        
        
        
        column(4, 
               titlePanel("Scup - NJ"),
               
               selectInput("SCUP_NJ_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPnjMode"),
               
               actionButton("SCUPNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnjSeason2",
                                    rlang::exec(sliderInput, inputId= "SCUPnjFH_seas2", label ="For Hire  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPnjFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPnjPR_seas2", label ="Private  Season 2",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPnjPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPnjSH_seas2", label ="Shore  Season 2",
                                                
                                                
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPnjSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }
    
  })
  
  ############# NJ Breakout by mode ######################################
  output$SFnjMode <- renderUI({
    if (is.null(input$SF_NJ_input_type))
      return()
    
    switch(input$SF_NJ_input_type, 
           "All Modes Combined" = div( rlang::exec(sliderInput,inputId = "SFnj_seas1", label =" Season 1", 
                                                   value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                   !!!date_slider_defaults),
                                       fluidRow(
                                         column(4, 
                                                numericInput(inputId = "SFnj_1_bag", label ="Bag Limit", 
                                                             min = 0, max = 100, value = 3)),
                                         column(6,
                                                rlang::exec(sliderInput, inputId= "SFnj_1_len", label ="Min Length",
                                                            min = 14, max = 21, value = 18, step = .5)))), 
           "Separated By Mode" = div( rlang::exec(sliderInput,inputId = "SFnjFH_seas1", label ="For Hire  Season 1", 
                                                  value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4, 
                                               numericInput(inputId = "SFnjFH_1_bag", label ="Bag Limit", 
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFnjFH_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 18, step = .5)), 
                                        rlang::exec(sliderInput, inputId= "SFnjPR_seas1", label ="Private/Rental  Season 1",  
                                                    value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                    !!!date_slider_defaults),
                                        fluidRow(
                                          column(4, 
                                                 numericInput(inputId = "SFnjPR_1_bag", label ="Bag Limit",
                                                              min = 0, max = 100, value = 3)),
                                          column(6,
                                                 rlang::exec(sliderInput, inputId= "SFnjPR_1_len", label ="Min Length",
                                                             min = 14, max = 21, value = 18, step = .5))),
                                        rlang::exec(sliderInput, inputId= "SFnjSH_seas1", label ="Shore  Season 1",  
                                                    value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                    !!!date_slider_defaults),
                                        fluidRow(
                                          column(4, 
                                                 numericInput(inputId = "SFnjSH_1_bag", label ="Bag Limit",
                                                              min = 0, max = 100, value = 3)), 
                                          column(6,
                                                 rlang::exec(sliderInput, inputId= "SFnjSH_1_len", label ="Min Length",
                                                             min = 14, max = 21, value = 18, step = .5))))))
  })
  
  
  output$BSBnjMode <- renderUI({
    if (is.null(input$BSB_NJ_input_type))
      return()
    
    switch(input$BSB_NJ_input_type,
           
           "All Modes Combined" = div( rlang::exec(sliderInput,inputId = "BSBnj_seas1", label =" Season 1", 
                                                   value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                   !!!date_slider_defaults),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_1_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 10)), 
                                         column(6,
                                                rlang::exec(sliderInput, inputId= "BSBnj_1_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5))),
                                       
                                       #Season 2
                                       rlang::exec(sliderInput, inputId= "BSBnj_seas2", label =" Season 2", 
                                                   value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                   !!!date_slider_defaults),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 1)), 
                                         column(6,
                                                rlang::exec(sliderInput, inputId= "BSBnj_2_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5))),
                                       
                                       #Season 3
                                       rlang::exec(sliderInput, inputId= "BSBnj_seas3", label =" Season 3", 
                                                   value=c(as.Date("10-01","%m-%d"),as.Date("10-31","%m-%d")), 
                                                   !!!date_slider_defaults),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 10)), 
                                         column(6,
                                                rlang::exec(sliderInput, inputId= "BSBnj_3_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5))),
                                       
                                       #Season 4
                                       rlang::exec(sliderInput, inputId= "BSBnj_seas4", label =" Season 4", 
                                                   value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                   !!!date_slider_defaults),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_4_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 15)), 
                                         column(6,
                                                rlang::exec(sliderInput, inputId= "BSBnj_4_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5)))),
           
           "Separated By Mode" = div( rlang::exec(sliderInput,inputId = "BSBnjFH_seas1", label =" For Hire  Season 1", 
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjFH_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjPR_seas1", label ="Private/Rental  Season 1",
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjPR_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjSH_seas1", label ="Shore  Season 1",
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjSH_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      #Season 2
                                      rlang::exec(sliderInput, inputId= "BSBnjFH_seas2", label =" For Hire  Season 2", 
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjFH_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjPR_seas2", label ="Private/Rental  Season 2",
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjPR_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjSH_seas2", label ="Shore  Season 2",
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjSH_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      #Season 3
                                      rlang::exec(sliderInput, inputId= "BSBnjFH_seas3", label =" For Hire  Season 3", 
                                                  value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjFH_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjPR_seas3", label ="Private/Rental  Season 3",
                                                  value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjPR_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjSH_seas3", label ="Shore  Season 3",
                                                  value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjSH_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      #Season 4
                                      rlang::exec(sliderInput, inputId= "BSBnjFH_seas4", label =" For Hire  Season 4", 
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjFH_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjPR_seas4", label ="Private/Rental  Season 4",
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjPR_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      rlang::exec(sliderInput, inputId= "BSBnjSH_seas4", label ="Shore  Season 4",
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnjSH_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5)))))
  })
  
  output$SCUPnjMode <- renderUI({
    if (is.null(input$SCUP_NJ_input_type))
      return()
    
    switch(input$SCUP_NJ_input_type,
           
           "All Modes Combined" = div( rlang::exec(sliderInput, inputId = "SCUPnj_seas1", label =" Season 1",
                                                   value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                   !!!date_slider_defaults),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "SCUPnj_1_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 30)),
                                         column(6,
                                                rlang::exec(sliderInput, inputId= "SCUPnj_1_len", label ="Min Length",
                                                            min = 8, max = 12, value = 10, step = .5)))),
           "Separated By Mode" = div(rlang::exec(sliderInput, inputId = "SCUPnjFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPnjFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     rlang::exec(sliderInput, inputId= "SCUPnjPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPnjPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     rlang::exec(sliderInput, inputId= "SCUPnjSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPnjSH_1_len", label ="Min Length",
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
  
  
  
  
  
  
  
  ############## DELAWARE ###########################################################
  output$addDE <- renderUI({
    if(any("DE" == input$state)){
      fluidRow( 
        style = "background-color: #FFFFCC;",
        column(4,
               titlePanel("Summer Flounder - DE"),
               
               selectInput("SF_DE_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFdeMode"),
               
               actionButton("SFDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFdeSeason3",
                                    rlang::exec(sliderInput, inputId= "SFdeFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFdeFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFdePR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFdePR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFdeSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFdeSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - DE"),
               
               selectInput("BSB_DE_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBdeMode"),
               
               
               actionButton("BSBDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBdeSeason3",
                                    rlang::exec(sliderInput, inputId= "BSBdeFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBdeFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBdePR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBdePR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBdeSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBdeSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - DE"),
               
               selectInput("SCUP_DE_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPdeMode"),
               
               actionButton("SCUPDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPdeSeason2",
                                    rlang::exec(sliderInput, inputId= "SCUPdeFH_seas2", label ="For Hire  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPdeFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPdePR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdePR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPdePR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPdeSH_seas2", label ="Shore  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPdeSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## DE breakout by mode ############################
  
  output$SFdeMode <- renderUI({
    if (is.null(input$SF_DE_input_type))
      return()
    
    switch(input$SF_DE_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFde_seas1", label ="Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFde_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      rlang::exec(sliderInput, inputId= "SFde_seas2", label =" Season 2",
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFde_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFdeFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFdeFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFdePR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFdePR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFdeSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFdeSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     
                                     rlang::exec(sliderInput, inputId= "SFdeFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFdeFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFdePR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFdePR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFdeSH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFdeSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBdeMode <- renderUI({
    if (is.null(input$BSB_DE_input_type))
      return()
    
    switch(input$BSB_DE_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "BSBde_seas1", label =" Season 1",
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBde_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      rlang::exec(sliderInput, inputId= "BSBde_seas2", label =" Season 2",
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBde_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput, inputId = "BSBdeFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBdeFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBdePR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBdePR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBdeSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBdeSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     rlang::exec(sliderInput, inputId= "BSBdeFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBdeFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBdePR_seas2", label ="Private  Season 2",
                                                 
                                                 
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBdePR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBdeSH_seas2", label ="Shore  Season 2",
                                                 
                                                 
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBdeSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPdeMode <- renderUI({
    if (is.null(input$SCUP_DE_input_type))
      return()
    
    switch(input$SCUP_DE_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput, inputId = "SCUPde_seas1", label =" Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SCUPde_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SCUPdeFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPdeFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPdePR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPdePR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPdeSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPdeSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ############## MARYLAND ###########################################################
  output$addMD <- renderUI({
    if(any("MD" == input$state)){
      fluidRow( 
        style = "background-color: #E5D8BD;",
        column(4,
               titlePanel("Summer Flounder - MD"),
               
               selectInput("SF_MD_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFmdMode"),
               
               actionButton("SFMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmdSeason3",
                                    rlang::exec(sliderInput, inputId= "SFmdFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFmdFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFmdPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFmdPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFmdSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFmdSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MD"),
               
               selectInput("BSB_MD_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBmdMode"),
               
               
               actionButton("BSBMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmdSeason3",
                                    rlang::exec(sliderInput, inputId= "BSBmdFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBmdFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBmdPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBmdPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBmdSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBmdSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MD"),
               
               selectInput("SCUP_MD_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPmdMode"),
               
               actionButton("SCUPMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmdSeason2",
                                    rlang::exec(sliderInput, inputId= "SCUPmdFH_seas2", label ="For Hire  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPmdFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPmdPR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPmdPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPmdSH_seas2", label ="Shore  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPmdSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## MD breakout by mode ############################
  output$SFmdMode <- renderUI({
    if (is.null(input$SF_MD_input_type))
      return()
    
    switch(input$SF_MD_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFmd_seas1", label ="Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFmd_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      rlang::exec(sliderInput, inputId= "SFmd_seas2", label =" Season 2",
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFmd_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFmdFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmdFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFmdPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmdPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFmdSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmdSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     
                                     rlang::exec(sliderInput, inputId= "SFmdFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmdFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFmdPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmdPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFmdSH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFmdSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  output$BSBmdMode <- renderUI({
    if (is.null(input$BSB_MD_input_type))
      return()
    
    switch(input$BSB_MD_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "BSBmd_seas1", label =" Season 1",
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBmd_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      rlang::exec(sliderInput, inputId= "BSBmd_seas2", label =" Season 2",
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBmd_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "BSBmdFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmdFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBmdPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmdPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBmdSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmdSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     rlang::exec(sliderInput, inputId= "BSBmdFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmdFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBmdPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmdPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBmdSH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBmdSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPmdMode <- renderUI({
    if (is.null(input$SCUP_MD_input_type))
      return()
    
    switch(input$SCUP_MD_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SCUPmd_seas1", label =" Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SCUPmd_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SCUPmdFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPmdFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPmdPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPmdPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPmdSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPmdSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ############## VIRGINIA ###########################################################
  output$addVA <- renderUI({
    if(any("VA" == input$state)){
      fluidRow( 
        style = "background-color: #FDDAEC;",
        column(4,
               titlePanel("Summer Flounder - VA"),
               
               selectInput("SF_VA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFvaMode"),
               
               actionButton("SFVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFvaSeason3",
                                    rlang::exec(sliderInput, inputId= "SFvaFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFvaFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFvaPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFvaPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFvaSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFvaSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - VA"),
               
               selectInput("BSB_VA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBvaMode"),
               
               
               actionButton("BSBVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBvaSeason3",
                                    rlang::exec(sliderInput, inputId= "BSBvaFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBvaFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBvaPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBvaPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBvaSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBvaSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - VA"),
               
               selectInput("SCUP_VA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPvaMode"),
               
               actionButton("SCUPVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPvaSeason2",
                                    rlang::exec(sliderInput, inputId= "SCUPvaFH_seas2", label ="For Hire  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPvaFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPvaPR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPvaPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPvaSH_seas2", label ="Shore  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPvaSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## VA breakout by mode ############################
  
  output$SFvaMode <- renderUI({
    if (is.null(input$SF_VA_input_type))
      return()
    
    switch(input$SF_VA_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFva_seas1", label =" Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFva_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      rlang::exec(sliderInput, inputId= "SFva_seas2", label =" Season 2",
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFva_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFvaFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFvaFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFvaPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFvaPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFvaSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFvaSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))),
                                     rlang::exec(sliderInput, inputId= "SFvaFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFvaFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFvaPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFvaPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFvaSH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFvaSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBvaMode <- renderUI({
    if (is.null(input$BSB_VA_input_type))
      return()
    
    switch(input$BSB_VA_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "BSBva_seas1", label =" Season 1",
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("07-15","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBva_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      rlang::exec(sliderInput, inputId= "BSBva_seas2", label =" Season 2",
                                                  value=c(as.Date("07-27","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBva_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "BSBvaFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBvaFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBvaPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBvaPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBvaSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-15","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBvaSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     rlang::exec(sliderInput, inputId= "BSBvaFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("07-27","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBvaFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBvaPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("07-27","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBvaPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBvaSH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("07-27","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBvaSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPvaMode <- renderUI({
    if (is.null(input$SCUP_VA_input_type))
      return()
    
    switch(input$SCUP_VA_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SCUPva_seas1", label =" Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SCUPva_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SCUPvaFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPvaFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPvaPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPvaPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPvaSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPvaSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  ############## NORTH CAROLINA ###########################################################
  output$addNC <- renderUI({
    if(any("NC" == input$state)){
      fluidRow( 
        style = "background-color: #F2F2F2;",
        column(4,
               titlePanel("Summer Flounder - NC"),
               
               selectInput("SF_NC_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SFncMode"),
               
               actionButton("SFNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFncSeason2",
                                    rlang::exec(sliderInput, inputId= "SFncFH_seas2", label ="For Hire  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFncFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFncPR_seas2", label ="Private  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFncPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SFncSH_seas2", label ="Shore  Season 2",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SFncSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NC"),
               
               selectInput("BSB_NC_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBncMode"),
               
               
               actionButton("BSBNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBncSeason3",
                                    rlang::exec(sliderInput, inputId= "BSBncFH_seas3", label ="For Hire  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBncFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBncPR_seas3", label ="Private  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBncPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    rlang::exec(sliderInput, inputId= "BSBncSH_seas3", label ="Shore  Season 3",
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "BSBncSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NC"),
               
               selectInput("SCUP_NC_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPncMode"),
               
               actionButton("SCUPNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPncSeason2",
                                    rlang::exec(sliderInput, inputId= "SCUPncFH_seas2", label ="For Hire  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPncFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPncPR_seas2", label ="Private  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPncPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    rlang::exec(sliderInput, inputId= "SCUPncSH_seas2", label ="Shore  Season 2", 
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                !!!date_slider_defaults),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             rlang::exec(sliderInput, inputId= "SCUPncSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## NC breakout by mode ############################
  
  output$SFncMode <- renderUI({
    if (is.null(input$SF_NC_input_type))
      return()
    
    switch(input$SF_NC_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SFnc_seas1", label =" Season 1",
                                                  value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 1)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SFnc_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 15, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SFncFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFncFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFncPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFncPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SFncSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SFncSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5)))))
  })
  
  
  output$BSBncMode <- renderUI({
    if (is.null(input$BSB_NC_input_type))
      return()
    
    switch(input$BSB_NC_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "BSBnc_seas1", label =" Season 1",
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnc_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      rlang::exec(sliderInput, inputId= "BSBnc_seas2", label =" Season 2",
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "BSBnc_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "BSBncFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBncFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBncPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBncPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBncSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBncSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     rlang::exec(sliderInput, inputId= "BSBncFH_seas2", label ="For Hire  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBncFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBncPR_seas2", label ="Private  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBncPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "BSBncSH_seas2", label ="Shore  Season 2",
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "BSBncSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPncMode <- renderUI({
    if (is.null(input$SCUP_NC_input_type))
      return()
    
    switch(input$SCUP_NC_input_type, 
           "All Modes Combined" = div(rlang::exec(sliderInput,inputId = "SCUPnc_seas1", label =" Season 1",
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  !!!date_slider_defaults),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               rlang::exec(sliderInput, inputId= "SCUPnc_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(rlang::exec(sliderInput,inputId = "SCUPncFH_seas1", label ="For Hire  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPncFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPncPR_seas1", label ="Private  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPncPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     rlang::exec(sliderInput, inputId= "SCUPncSH_seas1", label ="Shore  Season 1",
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 !!!date_slider_defaults),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              rlang::exec(sliderInput, inputId= "SCUPncSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  
  
  ################ Summary page outputs #################
  
  
  
  outputs <- function(){
    flist <- list.files(path = here::here("output/"), pattern = "\\.csv$", full.names = TRUE)
    
    read_cols<-c("metric","species","value", "mode","state","draw","model")
    read_cols_types<-c("c","c","d","c","c","i","c")
    all_data <- flist %>%
      set_names(flist) %>%  # Optional: keep file names for reference
      purrr::map_dfr(readr::read_csv, .id = "filename", col_select=all_of(read_cols), col_types=read_cols_types) %>% 
      dplyr::mutate(filename = stringr::str_extract(filename, "(?<=output_).+?(?=_202)"))
    return(all_data)
  }
  
  perc_changes <- function(){
    perc_changes <- outputs() %>% #all_data %>% 
      dplyr::filter(stringr::str_detect(filename, "SQ")) %>% 
      dplyr::group_by(state,filename, metric, mode, species) %>%
      dplyr::summarise(value = round(median(value),2)) %>% 
      dplyr::mutate(pca_reqs = dplyr::case_when(species == "sf" ~ .1, TRUE ~ .1), 
                    pca_reqs = dplyr::case_when(species == "bsb" ~ .1, TRUE ~ pca_reqs), 
                    pca_reqs = dplyr::case_when(species == "scup" ~ .1, TRUE ~ pca_reqs))
  }
  
  
  regs<- function(){
    flist <- list.files(path = here::here("saved_regs/"), pattern = "\\.csv$", full.names = TRUE)
    
    regs_data <- flist %>%
      purrr::map_dfr(readr::read_csv)
  }
  
  # get all_data
  #all_data<-outputs()
  
  # Summary
  output$summary_rhl_fig<- plotly::renderPlotly({
    
    ref_pct <- outputs() %>% #all_data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes" & model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    harv <- outputs() %>% #all_data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = join_by(species,  state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value+1) * 100) %>%
      dplyr::group_by(state,filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff), 2)) %>%
      tidyr::pivot_wider(names_from = species, values_from = median_pct_diff) %>% 
      dplyr::rename(Run_Name = filename.x)
    
    
    harv2 <- harv %>%
      ggplot2::ggplot(ggplot2::aes(x = bsb, y = sf, label = Run_Name, color = scup)) +
      ggplot2::geom_point( size = 3) +
      ggplot2::geom_text(color = "black", vjust = -0.5, size = 3) +
      #ggplot2::geom_hline(data = pca_sf, ggplot2::aes(yintercept = pca_reqs), color = "black")+
      #ggplot2::geom_vline(data = pca_bsb, ggplot2::aes(xintercept = pca_reqs), color = "black", linetype = "dashed")+
      ggplot2::facet_wrap(~ state) +
      ggplot2::scale_x_continuous(labels = function(x) format(round(x, 2), nsmall = 0)) +
      ggplot2::scale_y_continuous(labels = function(x) format(round(x, 2), nsmall = 0)) + 
      ggplot2::labs(title = "Percentage change in SF (vertical) and BSB (horizontal) Recreational Harvest By State",
                    x = "Change in BSB Harvest(%)",
                    y = "Change in SF Harvest (%)",
                    color="Change in Scup\nharvest (%)") +
      #ggplot2::scale_color_gradient2( low = "blue", mid = "gray", high = "red",  midpoint = 0, limits = c(-10, 10)) + 
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::theme_bw()
    
    fig<- plotly::ggplotly(harv2) %>%
      plotly::style(textposition = "top center")
    fig
  })
  
  output$summary_percdiff_table <- DT::renderDT({
    
    ref_pct <- outputs() %>% #all_data %>%
      dplyr::filter(metric == "keep_weight" &  mode == "all modes" & model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    harv <- outputs() %>% #all_data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = join_by(species,  state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value+1)  * 100) %>%
      dplyr::group_by(state,filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff),2)) %>%
      tidyr::pivot_wider(names_from = species, values_from = median_pct_diff)
    
    tab<- harv %>% 
      dplyr::mutate(bsb_ok  = abs(bsb)  <= bsb_percent_change,
                    scup_ok = abs(scup) <= scup_percent_change,
                    sf_ok   = abs(sf)   <= sf_percent_change) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ok_count = paste0(sum(c_across(c(bsb_ok, scup_ok, sf_ok))), "/3")) %>%
      dplyr::ungroup()%>%
      dplyr::select( -metric,  -bsb_ok ,-scup_ok, -sf_ok) %>%
      mutate(
        bsb  = paste0(sprintf("%.2f", bsb),"%"),
        scup = paste0(sprintf("%.2f", scup),"%"),
        sf   = paste0(sprintf("%.2f", sf),"%")
      ) %>% 
      dplyr::rename(State = state, `Run Name` = filename.x,
                    `BSB Change`= bsb, `Scup Change`= scup, `SF Change` = sf, `Below RHL` = ok_count)
    
    tab
  })
  
  output$summary_regs_table <- DT::renderDT({
    Regs_out <-regs() %>%
      tidyr::separate(input, into = c("species", "season", "measure"), sep = "_") %>%
      dplyr::mutate(season = stringr::str_remove(season, "^seas")) %>%
      tidyr::extract(species, into = c("species", "state2", "mode"), regex =  "([^a-z]+)([a-z]+)(.*)") %>%
      dplyr::select(-state2) %>%
      dplyr::group_by(run_name, state, species, mode, season) %>%
      tidyr::pivot_wider(names_from = measure, values_from = value) %>%
      dplyr::filter(!bag == 0) %>%
      dplyr::mutate(season2 = paste0(op, " - ", cl)) %>%
      dplyr::group_by(run_name, state, species, mode) %>%
      dplyr::summarise(
        bag = paste(bag, collapse = ","),
        len = paste(len, collapse = ","),
        season = paste(season2, collapse = ","),
        .groups = "drop" ) %>%
      dplyr::mutate(mode = if_else(mode == "", "All modes", mode)) %>% 
      dplyr::mutate(season = gsub("2025-", "", season))
    
  })
  
  
  ### Functions for state displays
  rhl_fig <- function(data, state_name){
    
    # Reference values (SQ model only)
    ref_pct <- data %>%
      dplyr::filter(
        metric == "keep_weight",
        mode == "all modes",
        model == "SQ",
        state == state_name
      ) %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    # Percent difference vs reference
    harv <- data %>%
      dplyr::filter(
        metric == "keep_weight",
        mode == "all modes",
        state == state_name
      ) %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value+1)  * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff),2), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = species, values_from = median_pct_diff) %>% 
      dplyr::rename(Run_Name = filename.x)
    
    # Static ggplot
    harv2 <- harv %>%
      ggplot2::ggplot(ggplot2::aes(x = bsb, y = sf, label = Run_Name, color = scup)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::labs(
        title = paste("Percentage change in SF (vertical) and BSB (horizontal) Recreational Harvest in", state_name),
        x = "Change in BSB Harvest(%)",
        y = "Change in SF Harvest (%)",
        color="Change in Scup\nharvest (%)") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::theme_bw()
    
    # Convert to plotly
    fig <- plotly::ggplotly(harv2) %>%
      plotly::style(textposition = "top center")
    
    return(fig)
  }
  
  cv_fig <- function(data, state_name){
    ref_pct <- data %>% 
      dplyr::filter(state == state_name & metric == "keep_weight"& mode == "all modes" & model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    harv <- data %>% #all_data %>%
      dplyr::filter( state == state_name & metric == "keep_weight" & mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = join_by(species,  state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value+1)  * 100) %>%
      dplyr::group_by(state,filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff),2)) %>%
      dplyr::rename(filename = filename.x)
    #tidyr::pivot_wider(names_from = category, values_from = median_pct_diff)
    
    welfare <-  data %>% 
      dplyr::filter(metric %in% c("CV"),
                    state == state_name,
                    mode == "all modes") %>%
      # dplyr::group_by( filename, category, draw) %>%
      # dplyr::summarise(Value = sum(as.numeric(value))) %>%
      dplyr::group_by(filename) %>%
      dplyr::mutate(value = value/1000000) %>% 
      dplyr::summarise(CV = round(median(value),2),
                       ci_lower = quantile(value, 0.05),
                       ci_upper = quantile(value, 0.95)) %>%
      left_join(harv)
    
    p1<- welfare %>% ggplot2::ggplot(ggplot2::aes(x = median_pct_diff, y = CV, label = filename))+
      ggplot2::geom_point() +
      ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle("Angler Satisfaction")+
      ggplot2::ylab("Angler Satisfaction ($M)")+
      ggplot2::xlab("Change in Harvest from SQ (%)")+
      ggplot2::theme(legend.position = "none")+
      ggplot2::facet_wrap(.~species)+
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::theme_bw()
    
    fig<- plotly::ggplotly(p1) %>%
      plotly::style(textposition = "top center")
    
    return(fig)
  }
  
  trips_fig <- function(data, state_name){
    # Reference values (SQ model only)
    ref_pct <- data %>%
      dplyr::filter(
        metric == "keep_weight",
        state == state_name,
        mode == "all modes",
        model == "SQ"
      ) %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    # Harvest percent difference
    harv <- data %>%
      dplyr::filter(
        metric == "keep_weight",
        state == state_name,
        mode == "all modes"
      ) %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value+1)  * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff),2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    
    # Trips data
    trips <- data %>%
      dplyr::filter(
        metric %in% c("predicted trips"),
        state == state_name,
        mode == "all modes"
      ) %>%
      dplyr::group_by(filename) %>%
      dplyr::summarise(trips = median(value), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename) %>% 
      dplyr::left_join(harv, by = "Run_Name") %>% 
      dplyr::mutate(trips = round(trips/1000000,2))
    
    # Static plot
    p1 <- trips %>%
      ggplot2::ggplot(ggplot2::aes(x = median_pct_diff, y = trips, label = Run_Name)) +
      ggplot2::geom_point() +
      ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle(paste("Number of Trips in", state_name)) +
      ggplot2::ylab("Predicted trips (N) millions") +
      ggplot2::xlab("Change in Harvest from SQ (%)")+
      ggplot2::theme(legend.position = "none") +
      ggplot2::facet_wrap(. ~ species) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::theme_bw()
    
    # Convert to plotly
    fig <- plotly::ggplotly(p1) %>%
      plotly::style(textposition = "top center")
    
    return(fig)
  }
  
  discards_fig<- function(data, state_name) {
    
    # Reference values (SQ model only, keep only)
    ref_pct <- data %>%
      dplyr::filter(
        metric == "keep_weight",
        state == state_name,
        mode == "all modes",
        model == "SQ"
      ) %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    # Harvest percent difference
    harv <- data %>%
      dplyr::filter(
        metric == "keep_weight",
        state == state_name,
        mode == "all modes"
      ) %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value+1)  * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_keep_pct_diff = round(median(pct_diff),2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    
    # Discards
    disc <- data %>%
      dplyr::filter(
        metric == "release_weight",
        state == state_name,
        mode == "all modes"
      ) %>%
      dplyr::group_by(state, filename, species) %>%
      dplyr::summarise(median_rel_weight = median(value), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename) %>% 
      dplyr::left_join(harv, by = c("state", "Run_Name", "species")) %>% 
      dplyr::mutate(median_rel_weight = round(median_rel_weight/1000000,2))
    
    # Static plot
    p1 <- disc %>%
      ggplot2::ggplot(ggplot2::aes(x = median_keep_pct_diff, y = median_rel_weight, label = Run_Name)) +
      ggplot2::geom_point() +
      ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle(paste("Discards in", state_name)) +
      ggplot2::ylab("Discards (million lbs)") +
      ggplot2::xlab("Change in Harvest from SQ (%)")+
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::facet_wrap(. ~ species) +
      ggplot2::theme_bw()
    
    # Convert to plotly
    fig <- plotly::ggplotly(p1) %>%
      plotly::style(textposition = "top center")
    
    return(fig)
  }
  
  ### MA
  output$ma_rhl_fig<- plotly::renderPlotly({
    rhl_ma <- rhl_fig(outputs(), "MA")
    rhl_ma
  })
  
  output$ma_CV_fig<- plotly::renderPlotly({
    cv_ma <- cv_fig(outputs(), "MA")
    cv_ma
  })
  
  output$ma_trips_fig<- plotly::renderPlotly({
    trips_ma <- trips_fig(outputs(), "MA")
    trips_ma
  })
  
  output$ma_discards_fig <- plotly::renderPlotly({
    discards_ma <- discards_fig(outputs(), "MA")
    discards_ma
  })
  
  ### RI
  output$ri_rhl_fig<- plotly::renderPlotly({
    rhl_ri <- rhl_fig(outputs(), "RI")
    rhl_ri
  })
  
  output$ri_CV_fig<- plotly::renderPlotly({
    cv_ri <- cv_fig(outputs(), "RI")
    cv_ri
  })
  
  output$ri_trips_fig<- plotly::renderPlotly({
    trips_ri <- trips_fig(outputs(), "RI")
    trips_ri
  })
  
  output$ri_discards_fig <- plotly::renderPlotly({
    discards_ri <- discards_fig(outputs(), "RI")
    discards_ri
  })
  
  ### CT
  output$ct_rhl_fig<- plotly::renderPlotly({
    rhl_ct <- rhl_fig(outputs(), "CT")
    rhl_ct
  })
  
  output$ct_CV_fig<- plotly::renderPlotly({
    cv_ct <- cv_fig(outputs(), "CT")
    cv_ct
  })
  
  output$ct_trips_fig<- plotly::renderPlotly({
    trips_ct <- trips_fig(outputs(), "CT")
    trips_ct
  })
  
  output$ct_discards_fig <- plotly::renderPlotly({
    discards_ct <- discards_fig(outputs(), "CT")
    discards_ct
  })
  
  ### NY
  output$ny_rhl_fig<- plotly::renderPlotly({
    rhl_ny <- rhl_fig(outputs(), "NY")
    rhl_ny
  })
  
  output$ny_CV_fig<- plotly::renderPlotly({
    cv_ny <- cv_fig(outputs(), "NY")
    cv_ny
  })
  
  output$ny_trips_fig<- plotly::renderPlotly({
    trips_ny <- trips_fig(outputs(), "NY")
    trips_ny
  })
  
  output$ny_discards_fig <- plotly::renderPlotly({
    discards_ny <- discards_fig(outputs(), "NY")
    discards_ny
  })
  
  
  ### NJ
  output$nj_rhl_fig<- plotly::renderPlotly({
    rhl_nj <- rhl_fig(outputs(), "NJ")
    rhl_nj
  })
  
  output$nj_CV_fig<- plotly::renderPlotly({
    cv_nj <- cv_fig(outputs(), "NJ")
    cv_nj
  })
  
  output$nj_trips_fig<- plotly::renderPlotly({
    trips_nj <- trips_fig(outputs(), "NJ")
    trips_nj
  })
  
  output$nj_discards_fig <- plotly::renderPlotly({
    discards_nj <- discards_fig(outputs(), "NJ")
    discards_nj
  })
  
  ### DE
  output$de_rhl_fig<- plotly::renderPlotly({
    rhl_de <- rhl_fig(outputs(), "DE")
    rhl_de
  })
  
  output$de_CV_fig<- plotly::renderPlotly({
    cv_de <- cv_fig(outputs(), "DE")
    cv_de
  })
  
  output$de_trips_fig<- plotly::renderPlotly({
    trips_de <- trips_fig(outputs(), "DE")
    trips_de
  })
  
  output$de_discards_fig <- plotly::renderPlotly({
    discards_de <- discards_fig(outputs(), "DE")
    discards_de
  })
  
  ### MD
  output$md_rhl_fig<- plotly::renderPlotly({
    rhl_md <- rhl_fig(outputs(), "MD")
    rhl_md
  })
  
  output$md_CV_fig<- plotly::renderPlotly({
    cv_md <- cv_fig(outputs(), "MD")
    cv_md
  })
  
  output$md_trips_fig<- plotly::renderPlotly({
    trips_md <- trips_fig(outputs(), "MD")
    trips_md
  })
  
  output$md_discards_fig <- plotly::renderPlotly({
    discards_md <- discards_fig(outputs(), "MD")
    discards_md
  })
  
  ### VA
  output$va_rhl_fig<- plotly::renderPlotly({
    rhl_va <- rhl_fig(outputs(), "VA")
    rhl_va
  })
  
  output$va_CV_fig<- plotly::renderPlotly({
    cv_va <- cv_fig(outputs(), "VA")
    cv_va
  })
  
  output$va_trips_fig<- plotly::renderPlotly({
    trips_va <- trips_fig(outputs(), "VA")
    trips_va
  })
  
  output$va_discards_fig <- plotly::renderPlotly({
    discards_va <- discards_fig(outputs(), "VA")
    discards_va
  })
  ### NC
  output$nc_rhl_fig<- plotly::renderPlotly({
    rhl_nc <- rhl_fig(outputs(), "NC")
    rhl_nc
  })
  
  output$nc_CV_fig<- plotly::renderPlotly({
    cv_nc <- cv_fig(outputs(), "NC")
    cv_nc
  })
  
  output$nc_trips_fig<- plotly::renderPlotly({
    trips_nc <- trips_fig(outputs(), "NC")
    trips_nc
  })
  
  output$nc_discards_fig <- plotly::renderPlotly({
    discards_nc <- discards_fig(outputs(), "NC")
    discards_nc
  })
  
  ####  Storing Inputs for decoupled model ####
  
  regulations <- observeEvent(input$runmeplease,{
    library(httr)
    library(jsonlite)
    library(openssl)
    library(uuid)
    
    enqueue_simple_sas <- function(run_name, queue_url_sas = Sys.getenv("AZURE_STORAGE_QUEUE_URL")) {
      stopifnot(nzchar(run_name), nzchar(queue_url_sas))
      payload <- list(
        runName = run_name,
        submissionId = UUIDgenerate(),
        submittedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
      msg_b64 <- base64_encode(charToRaw(toJSON(payload, auto_unbox = TRUE)))
      xml_body <- sprintf("<QueueMessage><MessageText>%s</MessageText></QueueMessage>", msg_b64)
      
      res <- POST(
        url = queue_url_sas,
        body = xml_body,
        content_type_xml(),
        add_headers(`x-ms-version` = "2020-10-02")
      )
      stop_for_status(res)
      invisible(TRUE)
    }
    
    
    regulations <- NULL
    print("where am i")
    #if(any( )) will run all selected check boxes on UI-regulations selection tab
    if(any("MA" == input$state)){
      print("start MA")
      
      if(input$BSB_MA_input_type == "All Modes Combined"){
        bsbMAregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("MA"), 
                                input =  c("BSBma_seas1_op", "BSBma_seas1_cl", "BSBma_1_bag", "BSBma_1_len", 
                                           "BSBmaFH_seas2_op", "BSBmaFH_seas2_cl", "BSBmaFH_2_bag", "BSBmaFH_2_len", 
                                           "BSBmaPR_seas2_op", "BSBmaPR_seas2_cl", "BSBmaPR_2_bag", "BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op", "BSBmaSH_seas2_cl", "BSBmaSH_2_bag", "BSBmaSH_2_len"),
                                value =  c(as.character(input$BSBma_seas1[1]), as.character(input$BSBma_seas1[2]), as.character(input$BSBma_1_bag), as.character(input$BSBma_1_len), 
                                           as.character(input$BSBmaFH_seas2[1]), as.character(input$BSBmaFH_seas2[2]), as.character(input$BSBmaFH_2_bag), as.character(input$BSBmaFH_2_len), 
                                           as.character(input$BSBmaPR_seas2[1]), as.character(input$BSBmaPR_seas2[2]), as.character(input$BSBmaPR_2_bag), as.character(input$BSBmaPR_2_len),
                                           as.character(input$BSBmaSH_seas2[1]), as.character(input$BSBmaSH_seas2[2]), as.character(input$BSBmaSH_2_bag), as.character(input$BSBmaSH_2_len)))
      }else{
        bsbMAregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("MA"), 
                                input =  c("BSBmaFH_seas1_op", "BSBmaFH_seas1_cl", "BSBmaFH_1_bag", "BSBmaFH_1_len", 
                                           "BSBmaPR_seas1_op", "BSBmaPR_seas1_cl", "BSBmaPR_1_bag", "BSBmaPR_1_len",
                                           "BSBmaSH_seas1_op", "BSBmaSH_seas1_cl", "BSBmaSH_1_bag", "BSBmaSH_1_len",
                                           "BSBmaFH_seas2_op", "BSBmaFH_seas2_cl", "BSBmaFH_2_bag", "BSBmaFH_2_len", 
                                           "BSBmaPR_seas2_op", "BSBmaPR_seas2_cl", "BSBmaPR_2_bag", "BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op", "BSBmaSH_seas2_cl", "BSBmaSH_2_bag", "BSBmaSH_2_len"),
                                value =  c(as.character(input$BSBmaFH_seas1[1]), as.character(input$BSBmaFH_seas1[2]), as.character(input$BSBmaFH_1_bag), as.character(input$BSBmaFH_1_len),
                                           as.character(input$BSBmaPR_seas1[1]), as.character(input$BSBmaPR_seas1[2]), as.character(input$BSBmaPR_1_bag), as.character(input$BSBmaPR_1_len), 
                                           as.character(input$BSBmaSH_seas1[1]), as.character(input$BSBmaSH_seas1[2]), as.character(input$BSBmaSH_1_bag), as.character(input$BSBmaSH_1_len), 
                                           as.character(input$BSBmaFH_seas2[1]), as.character(input$BSBmaFH_seas2[2]), as.character(input$BSBmaFH_2_bag), as.character(input$BSBmaFH_2_len), 
                                           as.character(input$BSBmaPR_seas2[1]), as.character(input$BSBmaPR_seas2[2]), as.character(input$BSBmaPR_2_bag), as.character(input$BSBmaPR_2_len),
                                           as.character(input$BSBmaSH_seas2[1]), as.character(input$BSBmaSH_seas2[2]), as.character(input$BSBmaSH_2_bag), as.character(input$BSBmaSH_2_len)))
      }
      
      MA_regs <- data.frame(run_name = c(Run_Name()), 
                            state = c("MA"), 
                            input =  c("SFmaFH_seas1_op", "SFmaFH_seas1_cl", "SFmaFH_1_bag", "SFmaFH_1_len", 
                                       "SFmaPR_seas1_op", "SFmaPR_seas1_cl", "SFmaPR_1_bag", "SFmaPR_1_len",
                                       "SFmaSH_seas1_op", "SFmaSH_seas1_cl", "SFmaSH_1_bag", "SFmaSH_1_len",
                                       "SFmaFH_seas2_op", "SFmaFH_seas2_cl", "SFmaFH_2_bag", "SFmaFH_2_len", 
                                       "SFmaPR_seas2_op", "SFmaPR_seas2_cl", "SFmaPR_2_bag", "SFmaPR_2_len",
                                       "SFmaSH_seas2_op", "SFmaSH_seas2_cl", "SFmaSH_2_bag", "SFmaSH_2_len",
                                       
                                       "SCUPmaFH_seas1_op", "SCUPmaFH_seas1_cl", "SCUPmaFH_1_bag", "SCUPmaFH_1_len", 
                                       "SCUPmaPR_seas1_op", "SCUPmaPR_seas1_cl", "SCUPmaPR_1_bag", "SCUPmaPR_1_len",
                                       "SCUPmaSH_seas1_op", "SCUPmaSH_seas1_cl", "SCUPmaSH_1_bag", "SCUPmaSH_1_len",
                                       "SCUPmaFH_seas2_op", "SCUPmaFH_seas2_cl", "SCUPmaFH_2_bag", "SCUPmaFH_2_len", 
                                       "SCUPmaPR_seas2_op", "SCUPmaPR_seas2_cl", "SCUPmaPR_2_bag", "SCUPmaPR_2_len",
                                       "SCUPmaSH_seas2_op", "SCUPmaSH_seas2_cl", "SCUPmaSH_2_bag", "SCUPmaSH_2_len", 
                                       "SCUPmaFH_seas3_op", "SCUPmaFH_seas3_cl", "SCUPmaFH_3_bag", "SCUPmaFH_3_len"),
                            value =  c(as.character(input$SFmaFH_seas1[1]), as.character(input$SFmaFH_seas1[2]), as.character(input$SFmaFH_1_bag), as.character(input$SFmaFH_1_len), 
                                       as.character(input$SFmaPR_seas1[1]), as.character(input$SFmaPR_seas1[2]), as.character(input$SFmaPR_1_bag), as.character(input$SFmaPR_1_len),
                                       as.character(input$SFmaSH_seas1[1]), as.character(input$SFmaSH_seas1[2]), as.character(input$SFmaSH_1_bag), as.character(input$SFmaSH_1_len),
                                       as.character(input$SFmaFH_seas2[1]), as.character(input$SFmaFH_seas2[2]), as.character(input$SFmaFH_2_bag), as.character(input$SFmaFH_2_len), 
                                       as.character(input$SFmaPR_seas2[1]), as.character(input$SFmaPR_seas2[2]), as.character(input$SFmaPR_2_bag), as.character(input$SFmaPR_2_len),
                                       as.character(input$SFmaSH_seas2[1]), as.character(input$SFmaSH_seas2[2]), as.character(input$SFmaSH_2_bag), as.character(input$SFmaSH_2_len),
                                       
                                       as.character(input$SCUPmaFH_seas1[1]), as.character(input$SCUPmaFH_seas1[2]), as.character(input$SCUPmaFH_1_bag), as.character(input$SCUPmaFH_1_len), 
                                       as.character(input$SCUPmaPR_seas1[1]), as.character(input$SCUPmaPR_seas1[2]), as.character(input$SCUPmaPR_1_bag), as.character(input$SCUPmaPR_1_len),
                                       as.character(input$SCUPmaSH_seas1[1]), as.character(input$SCUPmaSH_seas1[2]), as.character(input$SCUPmaSH_1_bag), as.character(input$SCUPmaSH_1_len),
                                       as.character(input$SCUPmaFH_seas2[1]), as.character(input$SCUPmaFH_seas2[2]), as.character(input$SCUPmaFH_2_bag), as.character(input$SCUPmaFH_2_len), 
                                       as.character(input$SCUPmaPR_seas2[1]), as.character(input$SCUPmaPR_seas2[2]), as.character(input$SCUPmaPR_2_bag), as.character(input$SCUPmaPR_2_len),
                                       as.character(input$SCUPmaSH_seas2[1]), as.character(input$SCUPmaSH_seas2[2]), as.character(input$SCUPmaSH_2_bag), as.character(input$SCUPmaSH_2_len),
                                       as.character(input$SCUPmaFH_seas3[1]), as.character(input$SCUPmaFH_seas3[2]), as.character(input$SCUPmaFH_3_bag), as.character(input$SCUPmaFH_3_len)))
      
      print("out SF and Scup")
      
      
      regulations <- regulations %>% rbind(MA_regs, bsbMAregs)
      print("made regulations MA")
    }
    
    if(any("RI" == input$state)){
      if(input$SF_RI_input_type == "All Modes Combined"){
        sfRIregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("RI"), 
                               input =  c("SFri_seas1_op", "SFri_seas1_cl", "SFri_1_bag", "SFri_1_len", 
                                          "SFriFH_seas2_op", "SFriFH_seas2_cl", "SFriFH_2_bag", "SFriFH_2_len", 
                                          "SFriPR_seas2_op", "SFriPR_seas2_cl", "SFriPR_2_bag", "SFriPR_2_len",
                                          "SFriSH_seas2_op", "SFriSH_seas2_cl", "SFriSH_2_bag", "SFriSH_2_len"),
                               value =  c(as.character(input$SFri_seas1[1]), as.character(input$SFri_seas1[2]), as.character(input$SFri_1_bag), as.character(input$SFri_1_len), 
                                          as.character(input$SFriFH_seas2[1]), as.character(input$SFriFH_seas2[2]), as.character(input$SFriFH_2_bag), as.character(input$SFriFH_2_len), 
                                          as.character(input$SFriPR_seas2[1]), as.character(input$SFriPR_seas2[2]), as.character(input$SFriPR_2_bag), as.character(input$SFriPR_2_len),
                                          as.character(input$SFriSH_seas2[1]), as.character(input$SFriSH_seas2[2]), as.character(input$SFriSH_2_bag), as.character(input$SFriSH_2_len)))
      }else{
        sfRIregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("RI"), 
                                input =  c("SFriFH_seas1_op", "SFriFH_seas1_cl", "SFriFH_1_bag", "SFriFH_1_len", 
                                           "SFriPR_seas1_op", "SFriPR_seas1_cl", "SFriPR_1_bag", "SFriPR_1_len",
                                           "SFriSH_seas1_op", "SFriSH_seas1_cl", "SFriSH_1_bag", "SFriSH_1_len",
                                           "SFriFH_seas2_op", "SFriFH_seas2_cl", "SFriFH_2_bag", "SFriFH_2_len", 
                                           "SFriPR_seas2_op", "SFriPR_seas2_cl", "SFriPR_2_bag", "SFriPR_2_len",
                                           "SFriSH_seas2_op", "SFriSH_seas2_cl", "SFriSH_2_bag", "SFriSH_2_len"),
                                value = c( as.character(input$SFriFH_seas1[1]), as.character(input$SFriFH_seas1[2]), as.character(input$SFriFH_1_bag), as.character(input$SFriFH_1_len), 
                                           as.character(input$SFriPR_seas1[1]), as.character(input$SFriPR_seas1[2]), as.character(input$SFriPR_1_bag), as.character(input$SFriPR_1_len),
                                           as.character(input$SFriSH_seas1[1]), as.character(input$SFriSH_seas1[2]), as.character(input$SFriSH_1_bag), as.character(input$SFriSH_1_len),
                                           as.character(input$SFriFH_seas2[1]), as.character(input$SFriFH_seas2[2]), as.character(input$SFriFH_2_bag), as.character(input$SFriFH_2_len), 
                                           as.character(input$SFriPR_seas2[1]), as.character(input$SFriPR_seas2[2]), as.character(input$SFriPR_2_bag), as.character(input$SFriPR_2_len),
                                           as.character(input$SFriSH_seas2[1]), as.character(input$SFriSH_seas2[2]), as.character(input$SFriSH_2_bag), as.character(input$SFriSH_2_len)))
      }
      
      
      RI_regs <- data.frame(run_name = c(Run_Name()), 
                            state = c("RI"), 
                            input =  c( "BSBriFH_seas1_op", "BSBriFH_seas1_cl", "BSBriFH_1_bag", "BSBriFH_1_len",
                                        "BSBriPR_seas1_op", "BSBriPR_seas1_cl", "BSBriPR_1_bag", "BSBriPR_1_len",
                                        "BSBriSH_seas1_op", "BSBriSH_seas1_cl", "BSBriSH_1_bag", "BSBriSH_1_len",
                                        "BSBriFH_seas2_op", "BSBriFH_seas2_cl", "BSBriFH_2_bag", "BSBriFH_2_len", 
                                        "BSBriPR_seas2_op", "BSBriPR_seas2_cl", "BSBriPR_2_bag", "BSBriPR_2_len",
                                        "BSBriSH_seas2_op", "BSBriSH_seas2_cl", "BSBriSH_2_bag", "BSBriSH_2_len",
                                        "BSBriFH_seas3_op", "BSBriFH_seas3_cl", "BSBriFH_3_bag", "BSBriFH_3_len", 
                                        "BSBriPR_seas3_op", "BSBriPR_seas3_cl", "BSBriPR_3_bag", "BSBriPR_3_len",
                                        "BSBriSH_seas3_op", "BSBriSH_seas3_cl", "BSBriSH_3_bag", "BSBriSH_3_len",
                                        
                                        "SCUPriFH_seas1_op", "SCUPriFH_seas1_cl", "SCUPriFH_1_bag", "SCUPriFH_1_len", 
                                        "SCUPriPR_seas1_op", "SCUPriPR_seas1_cl", "SCUPriPR_1_bag", "SCUPriPR_1_len",
                                        "SCUPriSH_seas1_op", "SCUPriSH_seas1_cl", "SCUPriSH_1_bag", "SCUPriSH_1_len",
                                        "SCUPriFH_seas2_op", "SCUPriFH_seas2_cl", "SCUPriFH_2_bag", "SCUPriFH_2_len", 
                                        "SCUPriPR_seas2_op", "SCUPriPR_seas2_cl", "SCUPriPR_2_bag", "SCUPriPR_2_len",
                                        "SCUPriSH_seas2_op", "SCUPriSH_seas2_cl", "SCUPriSH_2_bag", "SCUPriSH_2_len", 
                                        "SCUPriFH_seas3_op", "SCUPriFH_seas3_cl", "SCUPriFH_3_bag", "SCUPriFH_3_len", 
                                        "SCUPriFH_seas4_op", "SCUPriFH_seas4_cl", "SCUPriFH_4_bag", "SCUPriFH_4_len"),
                            value =  c(as.character(input$BSBriFH_seas1[1]), as.character(input$BSBriFH_seas1[2]), as.character(input$BSBriFH_1_bag), as.character(input$BSBriFH_1_len),
                                       as.character(input$BSBriPR_seas1[1]), as.character(input$BSBriPR_seas1[2]), as.character(input$BSBriPR_1_bag), as.character(input$BSBriPR_1_len), 
                                       as.character(input$BSBriSH_seas1[1]), as.character(input$BSBriSH_seas1[2]), as.character(input$BSBriSH_1_bag), as.character(input$BSBriSH_1_len), 
                                       as.character(input$BSBriFH_seas2[1]), as.character(input$BSBriFH_seas2[2]), as.character(input$BSBriFH_2_bag), as.character(input$BSBriFH_2_len), 
                                       as.character(input$BSBriPR_seas2[1]), as.character(input$BSBriPR_seas2[2]), as.character(input$BSBriPR_2_bag), as.character(input$BSBriPR_2_len),
                                       as.character(input$BSBriSH_seas2[1]), as.character(input$BSBriSH_seas2[2]), as.character(input$BSBriSH_2_bag), as.character(input$BSBriSH_2_len), 
                                       as.character(input$BSBriFH_seas3[1]), as.character(input$BSBriFH_seas3[2]), as.character(input$BSBriFH_3_bag), as.character(input$BSBriFH_3_len), 
                                       as.character(input$BSBriPR_seas3[1]), as.character(input$BSBriPR_seas3[2]), as.character(input$BSBriPR_3_bag), as.character(input$BSBriPR_3_len),
                                       as.character(input$BSBriSH_seas3[1]), as.character(input$BSBriSH_seas3[2]), as.character(input$BSBriSH_3_bag), as.character(input$BSBriSH_3_len),
                                       
                                       as.character(input$SCUPriFH_seas1[1]), as.character(input$SCUPriFH_seas1[2]), as.character(input$SCUPriFH_1_bag), as.character(input$SCUPriFH_1_len), 
                                       as.character(input$SCUPriPR_seas1[1]), as.character(input$SCUPriPR_seas1[2]), as.character(input$SCUPriPR_1_bag), as.character(input$SCUPriPR_1_len),
                                       as.character(input$SCUPriSH_seas1[1]), as.character(input$SCUPriSH_seas1[2]), as.character(input$SCUPriSH_1_bag), as.character(input$SCUPriSH_1_len),
                                       as.character(input$SCUPriFH_seas2[1]), as.character(input$SCUPriFH_seas2[2]), as.character(input$SCUPriFH_2_bag), as.character(input$SCUPriFH_2_len), 
                                       as.character(input$SCUPriPR_seas2[1]), as.character(input$SCUPriPR_seas2[2]), as.character(input$SCUPriPR_2_bag), as.character(input$SCUPriPR_2_len),
                                       as.character(input$SCUPriSH_seas2[1]), as.character(input$SCUPriSH_seas2[2]), as.character(input$SCUPriSH_2_bag), as.character(input$SCUPriSH_2_len),
                                       as.character(input$SCUPriFH_seas3[1]), as.character(input$SCUPriFH_seas3[2]), as.character(input$SCUPriFH_3_bag), as.character(input$SCUPriFH_3_len), 
                                       as.character(input$SCUPriFH_seas4[1]), as.character(input$SCUPriFH_seas4[2]), as.character(input$SCUPriFH_4_bag), as.character(input$SCUPriFH_4_len)))
      regulations <- regulations %>% rbind(RI_regs, sfRIregs)
    }
    
    if(any("CT" == input$state)){
      if(input$SF_CT_input_type == "All Modes Combined"){
        sfCTregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("CT"), 
                               input =  c("SFct_seas1_op", "SFct_seas1_cl", "SFct_1_bag", "SFct_1_len", 
                                          "SFct_seas2_op", "SFct_seas2_cl", "SFct_2_bag", "SFct_2_len", 
                                          "SFctFH_seas3_op", "SFctFH_seas3_cl", "SFctFH_3_bag", "SFctFH_3_len", 
                                          "SFctPR_seas3_op", "SFctPR_seas3_cl", "SFctPR_3_bag", "SFctPR_3_len",
                                          "SFctSH_seas3_op", "SFctSH_seas3_cl", "SFctSH_3_bag", "SFctSH_3_len"),
                               value =  c(as.character(input$SFct_seas1[1]), as.character(input$SFct_seas1[2]), as.character(input$SFct_1_bag), as.character(input$SFct_1_len), 
                                          as.character(input$SFct_seas2[1]), as.character(input$SFct_seas2[2]), as.character(input$SFct_2_bag), as.character(input$SFct_2_len), 
                                          as.character(input$SFctFH_seas3[1]), as.character(input$SFctFH_seas3[2]), as.character(input$SFctFH_3_bag), as.character(input$SFctFH_3_len), 
                                          as.character(input$SFctPR_seas3[1]), as.character(input$SFctPR_seas3[2]), as.character(input$SFctPR_3_bag), as.character(input$SFctPR_3_len),
                                          as.character(input$SFctSH_seas3[1]), as.character(input$SFctSH_seas3[2]), as.character(input$SFctSH_3_bag), as.character(input$SFctSH_3_len)))
      }else{
        sfCTregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("CT"), 
                                input =  c("SFctFH_seas1_op", "SFctFH_seas1_cl", "SFctFH_1_bag", "SFctFH_1_len", 
                                           "SFctPR_seas1_op", "SFctPR_seas1_cl", "SFctPR_1_bag", "SFctPR_1_len",
                                           "SFctSH_seas1_op", "SFctSH_seas1_cl", "SFctSH_1_bag", "SFctSH_1_len",
                                           "SFctFH_seas2_op", "SFctFH_seas2_cl", "SFctFH_2_bag", "SFctFH_2_len", 
                                           "SFctPR_seas2_op", "SFctPR_seas2_cl", "SFctPR_2_bag", "SFctPR_2_len",
                                           "SFctSH_seas2_op", "SFctSH_seas2_cl", "SFctSH_2_bag", "SFctSH_2_len", 
                                           "SFctFH_seas3_op", "SFctFH_seas3_cl", "SFctFH_3_bag", "SFctFH_3_len", 
                                           "SFctPR_seas3_op", "SFctPR_seas3_cl", "SFctPR_3_bag", "SFctPR_3_len",
                                           "SFctSH_seas3_op", "SFctSH_seas3_cl", "SFctSH_3_bag", "SFctSH_3_len"),
                                value = c( as.character(input$SFctFH_seas1[1]), as.character(input$SFctFH_seas1[2]), as.character(input$SFctFH_1_bag), as.character(input$SFctFH_1_len), 
                                           as.character(input$SFctPR_seas1[1]), as.character(input$SFctPR_seas1[2]), as.character(input$SFctPR_1_bag), as.character(input$SFctPR_1_len),
                                           as.character(input$SFctSH_seas1[1]), as.character(input$SFctSH_seas1[2]), as.character(input$SFctSH_1_bag), as.character(input$SFctSH_1_len),
                                           as.character(input$SFctFH_seas2[1]), as.character(input$SFctFH_seas2[2]), as.character(input$SFctFH_2_bag), as.character(input$SFctFH_2_len), 
                                           as.character(input$SFctPR_seas2[1]), as.character(input$SFctPR_seas2[2]), as.character(input$SFctPR_2_bag), as.character(input$SFctPR_2_len),
                                           as.character(input$SFctSH_seas2[1]), as.character(input$SFctSH_seas2[2]), as.character(input$SFctSH_2_bag), as.character(input$SFctSH_2_len), 
                                           as.character(input$SFctFH_seas3[1]), as.character(input$SFctFH_seas3[2]), as.character(input$SFctFH_3_bag), as.character(input$SFctFH_3_len), 
                                           as.character(input$SFctPR_seas3[1]), as.character(input$SFctPR_seas3[2]), as.character(input$SFctPR_3_bag), as.character(input$SFctPR_3_len),
                                           as.character(input$SFctSH_seas3[1]), as.character(input$SFctSH_seas3[2]), as.character(input$SFctSH_3_bag), as.character(input$SFctSH_3_len)))
      }
      
      
      CT_regs <- data.frame(run_name = c(Run_Name()), 
                            state = c("CT"), 
                            input =  c( "BSBctFH_seas1_op", "BSBctFH_seas1_cl", "BSBctFH_1_bag", "BSBctFH_1_len",
                                        "BSBctPR_seas1_op", "BSBctPR_seas1_cl", "BSBctPR_1_bag", "BSBctPR_1_len",
                                        "BSBctSH_seas1_op", "BSBctSH_seas1_cl", "BSBctSH_1_bag", "BSBctSH_1_len",
                                        "BSBctFH_seas2_op", "BSBctFH_seas2_cl", "BSBctFH_2_bag", "BSBctFH_2_len", 
                                        "BSBctPR_seas2_op", "BSBctPR_seas2_cl", "BSBctPR_2_bag", "BSBctPR_2_len",
                                        "BSBctSH_seas2_op", "BSBctSH_seas2_cl", "BSBctSH_2_bag", "BSBctSH_2_len",
                                        "BSBctFH_seas3_op", "BSBctFH_seas3_cl", "BSBctFH_3_bag", "BSBctFH_3_len", 
                                        "BSBctPR_seas3_op", "BSBctPR_seas3_cl", "BSBctPR_3_bag", "BSBctPR_3_len",
                                        "BSBctSH_seas3_op", "BSBctSH_seas3_cl", "BSBctSH_3_bag", "BSBctSH_3_len",
                                        
                                        "SCUPctFH_seas1_op", "SCUPctFH_seas1_cl", "SCUPctFH_1_bag", "SCUPctFH_1_len", 
                                        "SCUPctPR_seas1_op", "SCUPctPR_seas1_cl", "SCUPctPR_1_bag", "SCUPctPR_1_len",
                                        "SCUPctSH_seas1_op", "SCUPctSH_seas1_cl", "SCUPctSH_1_bag", "SCUPctSH_1_len",
                                        "SCUPctFH_seas2_op", "SCUPctFH_seas2_cl", "SCUPctFH_2_bag", "SCUPctFH_2_len", 
                                        "SCUPctPR_seas2_op", "SCUPctPR_seas2_cl", "SCUPctPR_2_bag", "SCUPctPR_2_len",
                                        "SCUPctSH_seas2_op", "SCUPctSH_seas2_cl", "SCUPctSH_2_bag", "SCUPctSH_2_len", 
                                        "SCUPctFH_seas3_op", "SCUPctFH_seas3_cl", "SCUPctFH_3_bag", "SCUPctFH_3_len", 
                                        "SCUPctFH_seas4_op", "SCUPctFH_seas4_cl", "SCUPctFH_4_bag", "SCUPctFH_4_len"),
                            value =  c(as.character(input$BSBctFH_seas1[1]), as.character(input$BSBctFH_seas1[2]), as.character(input$BSBctFH_1_bag), as.character(input$BSBctFH_1_len),
                                       as.character(input$BSBctPR_seas1[1]), as.character(input$BSBctPR_seas1[2]), as.character(input$BSBctPR_1_bag), as.character(input$BSBctPR_1_len), 
                                       as.character(input$BSBctSH_seas1[1]), as.character(input$BSBctSH_seas1[2]), as.character(input$BSBctSH_1_bag), as.character(input$BSBctSH_1_len), 
                                       as.character(input$BSBctFH_seas2[1]), as.character(input$BSBctFH_seas2[2]), as.character(input$BSBctFH_2_bag), as.character(input$BSBctFH_2_len), 
                                       as.character(input$BSBctPR_seas2[1]), as.character(input$BSBctPR_seas2[2]), as.character(input$BSBctPR_2_bag), as.character(input$BSBctPR_2_len),
                                       as.character(input$BSBctSH_seas2[1]), as.character(input$BSBctSH_seas2[2]), as.character(input$BSBctSH_2_bag), as.character(input$BSBctSH_2_len), 
                                       as.character(input$BSBctFH_seas3[1]), as.character(input$BSBctFH_seas3[2]), as.character(input$BSBctFH_3_bag), as.character(input$BSBctFH_3_len), 
                                       as.character(input$BSBctPR_seas3[1]), as.character(input$BSBctPR_seas3[2]), as.character(input$BSBctPR_3_bag), as.character(input$BSBctPR_3_len),
                                       as.character(input$BSBctSH_seas3[1]), as.character(input$BSBctSH_seas3[2]), as.character(input$BSBctSH_3_bag), as.character(input$BSBctSH_3_len),
                                       
                                       as.character(input$SCUPctFH_seas1[1]), as.character(input$SCUPctFH_seas1[2]), as.character(input$SCUPctFH_1_bag), as.character(input$SCUPctFH_1_len), 
                                       as.character(input$SCUPctPR_seas1[1]), as.character(input$SCUPctPR_seas1[2]), as.character(input$SCUPctPR_1_bag), as.character(input$SCUPctPR_1_len),
                                       as.character(input$SCUPctSH_seas1[1]), as.character(input$SCUPctSH_seas1[2]), as.character(input$SCUPctSH_1_bag), as.character(input$SCUPctSH_1_len),
                                       as.character(input$SCUPctFH_seas2[1]), as.character(input$SCUPctFH_seas2[2]), as.character(input$SCUPctFH_2_bag), as.character(input$SCUPctFH_2_len), 
                                       as.character(input$SCUPctPR_seas2[1]), as.character(input$SCUPctPR_seas2[2]), as.character(input$SCUPctPR_2_bag), as.character(input$SCUPctPR_2_len),
                                       as.character(input$SCUPctSH_seas2[1]), as.character(input$SCUPctSH_seas2[2]), as.character(input$SCUPctSH_2_bag), as.character(input$SCUPctSH_2_len),
                                       as.character(input$SCUPctFH_seas3[1]), as.character(input$SCUPctFH_seas3[2]), as.character(input$SCUPctFH_3_bag), as.character(input$SCUPctFH_3_len), 
                                       as.character(input$SCUPctFH_seas4[1]), as.character(input$SCUPctFH_seas4[2]), as.character(input$SCUPctFH_4_bag), as.character(input$SCUPctFH_4_len)))
      regulations <- regulations %>% rbind(CT_regs, sfCTregs)
      
    }
    
    if(any("NY" == input$state)){
      if(input$SF_NY_input_type == "All Modes Combined"){
        sfNYregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("NY"),
                               input =  c("SFny_seas1_op", "SFny_seas1_cl", "SFny_1_bag", "SFny_1_len", 
                                          "SFny_seas2_op", "SFny_seas2_cl", "SFny_2_bag", "SFny_2_len", 
                                          "SFnyFH_seas3_op", "SFnyFH_seas3_cl", "SFnyFH_3_bag", "SFnyFH_3_len", 
                                          "SFnyPR_seas3_op", "SFnyPR_seas3_cl", "SFnyPR_3_bag", "SFnyPR_3_len",
                                          "SFnySH_seas3_op", "SFnySH_seas3_cl", "SFnySH_3_bag", "SFnySH_3_len"),
                               value =  c(as.character(input$SFny_seas1[1]), as.character(input$SFny_seas1[2]), as.character(input$SFny_1_bag), as.character(input$SFny_1_len), 
                                          as.character(input$SFny_seas2[1]), as.character(input$SFny_seas2[2]), as.character(input$SFny_2_bag), as.character(input$SFny_2_len), 
                                          as.character(input$SFnyFH_seas3[1]), as.character(input$SFnyFH_seas3[2]), as.character(input$SFnyFH_3_bag), as.character(input$SFnyFH_3_len), 
                                          as.character(input$SFnyPR_seas3[1]), as.character(input$SFnyPR_seas3[2]), as.character(input$SFnyPR_3_bag), as.character(input$SFnyPR_3_len),
                                          as.character(input$SFnySH_seas3[1]), as.character(input$SFnySH_seas3[2]), as.character(input$SFnySH_3_bag), as.character(input$SFnySH_3_len)))
      }else{
        sfNYregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("NY"),
                                input =  c("SFnyFH_seas1_op", "SFnyFH_seas1_cl", "SFnyFH_1_bag", "SFnyFH_1_len", 
                                           "SFnyPR_seas1_op", "SFnyPR_seas1_cl", "SFnyPR_1_bag", "SFnyPR_1_len",
                                           "SFnySH_seas1_op", "SFnySH_seas1_cl", "SFnySH_1_bag", "SFnySH_1_len",
                                           "SFnyFH_seas2_op", "SFnyFH_seas2_cl", "SFnyFH_2_bag", "SFnyFH_2_len", 
                                           "SFnyPR_seas2_op", "SFnyPR_seas2_cl", "SFnyPR_2_bag", "SFnyPR_2_len",
                                           "SFnySH_seas2_op", "SFnySH_seas2_cl", "SFnySH_2_bag", "SFnySH_2_len", 
                                           "SFnyFH_seas3_op", "SFnyFH_seas3_cl", "SFnyFH_3_bag", "SFnyFH_3_len", 
                                           "SFnyPR_seas3_op", "SFnyPR_seas3_cl", "SFnyPR_3_bag", "SFnyPR_3_len",
                                           "SFnySH_seas3_op", "SFnySH_seas3_cl", "SFnySH_3_bag", "SFnySH_3_len"),
                                value = c( as.character(input$SFnyFH_seas1[1]), as.character(input$SFnyFH_seas1[2]), as.character(input$SFnyFH_1_bag), as.character(input$SFnyFH_1_len), 
                                           as.character(input$SFnyPR_seas1[1]), as.character(input$SFnyPR_seas1[2]), as.character(input$SFnyPR_1_bag), as.character(input$SFnyPR_1_len),
                                           as.character(input$SFnySH_seas1[1]), as.character(input$SFnySH_seas1[2]), as.character(input$SFnySH_1_bag), as.character(input$SFnySH_1_len),
                                           as.character(input$SFnyFH_seas2[1]), as.character(input$SFnyFH_seas2[2]), as.character(input$SFnyFH_2_bag), as.character(input$SFnyFH_2_len), 
                                           as.character(input$SFnyPR_seas2[1]), as.character(input$SFnyPR_seas2[2]), as.character(input$SFnyPR_2_bag), as.character(input$SFnyPR_2_len),
                                           as.character(input$SFnySH_seas2[1]), as.character(input$SFnySH_seas2[2]), as.character(input$SFnySH_2_bag), as.character(input$SFnySH_2_len), 
                                           as.character(input$SFnyFH_seas3[1]), as.character(input$SFnyFH_seas3[2]), as.character(input$SFnyFH_3_bag), as.character(input$SFnyFH_3_len), 
                                           as.character(input$SFnyPR_seas3[1]), as.character(input$SFnyPR_seas3[2]), as.character(input$SFnyPR_3_bag), as.character(input$SFnyPR_3_len),
                                           as.character(input$SFnySH_seas3[1]), as.character(input$SFnySH_seas3[2]), as.character(input$SFnySH_3_bag), as.character(input$SFnySH_3_len)))
      }
      
      
      if(input$BSB_NY_input_type == "All Modes Combined"){
        bsbNYregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("NY"),
                                input =  c("BSBny_seas1_op", "BSBny_seas1_cl", "BSBny_1_bag", "BSBny_1_len", 
                                           "BSBny_seas2_op", "BSBny_seas2_cl", "BSBny_2_bag", "BSBny_2_len", 
                                           "BSBnyFH_seas3_op", "BSBnyFH_seas3_cl", "BSBnyFH_3_bag", "BSBnyFH_3_len", 
                                           "BSBnyPR_seas3_op", "BSBnyPR_seas3_cl", "BSBnyPR_3_bag", "BSBnyPR_3_len",
                                           "BSBnySH_seas3_op", "BSBnySH_seas3_cl", "BSBnySH_3_bag", "BSBnySH_3_len"),
                                value =  c(as.character(input$BSBny_seas1[1]), as.character(input$BSBny_seas1[2]), as.character(input$BSBny_1_bag), as.character(input$BSBny_1_len), 
                                           as.character(input$BSBny_seas2[1]), as.character(input$BSBny_seas2[2]), as.character(input$BSBny_2_bag), as.character(input$BSBny_2_len), 
                                           as.character(input$BSBnyFH_seas3[1]), as.character(input$BSBnyFH_seas3[2]), as.character(input$BSBnyFH_3_bag), as.character(input$BSBnyFH_3_len), 
                                           as.character(input$BSBnyPR_seas3[1]), as.character(input$BSBnyPR_seas3[2]), as.character(input$BSBnyPR_3_bag), as.character(input$BSBnyPR_3_len),
                                           as.character(input$BSBnySH_seas3[1]), as.character(input$BSBnySH_seas3[2]), as.character(input$BSBnySH_3_bag), as.character(input$BSBnySH_3_len)))
      }else{
        bsbNYregs <-  data.frame(run_name = c(Run_Name()), 
                                 state = c("NY"),
                                 input =  c( "BSBnyFH_seas1_op", "BSBnyFH_seas1_cl", "BSBnyFH_1_bag", "BSBnyFH_1_len",
                                             "BSBnyPR_seas1_op", "BSBnyPR_seas1_cl", "BSBnyPR_1_bag", "BSBnyPR_1_len",
                                             "BSBnySH_seas1_op", "BSBnySH_seas1_cl", "BSBnySH_1_bag", "BSBnySH_1_len",
                                             "BSBnyFH_seas2_op", "BSBnyFH_seas2_cl", "BSBnyFH_2_bag", "BSBnyFH_2_len", 
                                             "BSBnyPR_seas2_op", "BSBnyPR_seas2_cl", "BSBnyPR_2_bag", "BSBnyPR_2_len",
                                             "BSBnySH_seas2_op", "BSBnySH_seas2_cl", "BSBnySH_2_bag", "BSBnySH_2_len",
                                             "BSBnyFH_seas3_op", "BSBnyFH_seas3_cl", "BSBnyFH_3_bag", "BSBnyFH_3_len", 
                                             "BSBnyPR_seas3_op", "BSBnyPR_seas3_cl", "BSBnyPR_3_bag", "BSBnyPR_3_len",
                                             "BSBnySH_seas3_op", "BSBnySH_seas3_cl", "BSBnySH_3_bag", "BSBnySH_3_len"),
                                 value = c( as.character(input$BSBnyFH_seas1[1]), as.character(input$BSBnyFH_seas1[2]), as.character(input$BSBnyFH_1_bag), as.character(input$BSBnyFH_1_len),
                                            as.character(input$BSBnyPR_seas1[1]), as.character(input$BSBnyPR_seas1[2]), as.character(input$BSBnyPR_1_bag), as.character(input$BSBnyPR_1_len), 
                                            as.character(input$BSBnySH_seas1[1]), as.character(input$BSBnySH_seas1[2]), as.character(input$BSBnySH_1_bag), as.character(input$BSBnySH_1_len), 
                                            as.character(input$BSBnyFH_seas2[1]), as.character(input$BSBnyFH_seas2[2]), as.character(input$BSBnyFH_2_bag), as.character(input$BSBnyFH_2_len), 
                                            as.character(input$BSBnyPR_seas2[1]), as.character(input$BSBnyPR_seas2[2]), as.character(input$BSBnyPR_2_bag), as.character(input$BSBnyPR_2_len),
                                            as.character(input$BSBnySH_seas2[1]), as.character(input$BSBnySH_seas2[2]), as.character(input$BSBnySH_2_bag), as.character(input$BSBnySH_2_len), 
                                            as.character(input$BSBnyFH_seas3[1]), as.character(input$BSBnyFH_seas3[2]), as.character(input$BSBnyFH_3_bag), as.character(input$BSBnyFH_3_len), 
                                            as.character(input$BSBnyPR_seas3[1]), as.character(input$BSBnyPR_seas3[2]), as.character(input$BSBnyPR_3_bag), as.character(input$BSBnyPR_3_len),
                                            as.character(input$BSBnySH_seas3[1]), as.character(input$BSBnySH_seas3[2]), as.character(input$BSBnySH_3_bag), as.character(input$BSBnySH_3_len)))
      }
      
      
      
      NY_regs <- data.frame(run_name = c(Run_Name()), 
                            state = c("NY"),
                            input =  c( "SCUPnyFH_seas1_op", "SCUPnyFH_seas1_cl", "SCUPnyFH_1_bag", "SCUPnyFH_1_len", 
                                        "SCUPnyPR_seas1_op", "SCUPnyPR_seas1_cl", "SCUPnyPR_1_bag", "SCUPnyPR_1_len",
                                        "SCUPnySH_seas1_op", "SCUPnySH_seas1_cl", "SCUPnySH_1_bag", "SCUPnySH_1_len",
                                        "SCUPnyFH_seas2_op", "SCUPnyFH_seas2_cl", "SCUPnyFH_2_bag", "SCUPnyFH_2_len", 
                                        "SCUPnyPR_seas2_op", "SCUPnyPR_seas2_cl", "SCUPnyPR_2_bag", "SCUPnyPR_2_len",
                                        "SCUPnySH_seas2_op", "SCUPnySH_seas2_cl", "SCUPnySH_2_bag", "SCUPnySH_2_len", 
                                        "SCUPnyFH_seas3_op", "SCUPnyFH_seas3_cl", "SCUPnyFH_3_bag", "SCUPnyFH_3_len", 
                                        "SCUPnyFH_seas4_op", "SCUPnyFH_seas4_cl", "SCUPnyFH_4_bag", "SCUPnyFH_4_len"),
                            value =  c(as.character(input$SCUPnyFH_seas1[1]), as.character(input$SCUPnyFH_seas1[2]), as.character(input$SCUPnyFH_1_bag), as.character(input$SCUPnyFH_1_len), 
                                       as.character(input$SCUPnyPR_seas1[1]), as.character(input$SCUPnyPR_seas1[2]), as.character(input$SCUPnyPR_1_bag), as.character(input$SCUPnyPR_1_len),
                                       as.character(input$SCUPnySH_seas1[1]), as.character(input$SCUPnySH_seas1[2]), as.character(input$SCUPnySH_1_bag), as.character(input$SCUPnySH_1_len),
                                       as.character(input$SCUPnyFH_seas2[1]), as.character(input$SCUPnyFH_seas2[2]), as.character(input$SCUPnyFH_2_bag), as.character(input$SCUPnyFH_2_len), 
                                       as.character(input$SCUPnyPR_seas2[1]), as.character(input$SCUPnyPR_seas2[2]), as.character(input$SCUPnyPR_2_bag), as.character(input$SCUPnyPR_2_len),
                                       as.character(input$SCUPnySH_seas2[1]), as.character(input$SCUPnySH_seas2[2]), as.character(input$SCUPnySH_2_bag), as.character(input$SCUPnySH_2_len),
                                       as.character(input$SCUPnyFH_seas3[1]), as.character(input$SCUPnyFH_seas3[2]), as.character(input$SCUPnyFH_3_bag), as.character(input$SCUPnyFH_3_len), 
                                       as.character(input$SCUPnyFH_seas4[1]), as.character(input$SCUPnyFH_seas4[2]), as.character(input$SCUPnyFH_4_bag), as.character(input$SCUPnyFH_4_len)))
      regulations <- regulations %>% rbind(NY_regs, sfNYregs, bsbNYregs)
      
    }
    
    if(any("NJ" == input$state)){
      if(input$SF_NJ_input_type == "All Modes Combined"){
        sfNJregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("NJ"),
                               input =  c("SFnj_seas1_op", "SFnj_seas1_cl", "SFnj_1_bag", "SFnj_1_len", 
                                          "SFnjFH_seas2_op", "SFnjFH_seas2_cl", "SFnjFH_2_bag", "SFnjFH_2_len", 
                                          "SFnjPR_seas2_op", "SFnjPR_seas2_cl", "SFnjPR_2_bag", "SFnjPR_2_len",
                                          "SFnjSH_seas2_op", "SFnjSH_seas2_cl", "SFnjSH_2_bag", "SFnjSH_2_len"),
                               value =  c(as.character(input$SFnj_seas1[1]), as.character(input$SFnj_seas1[2]), as.character(input$SFnj_1_bag), as.character(input$SFnj_1_len), 
                                          as.character(input$SFnjFH_seas2[1]), as.character(input$SFnjFH_seas2[2]), as.character(input$SFnjFH_2_bag), as.character(input$SFnjFH_2_len), 
                                          as.character(input$SFnjPR_seas2[1]), as.character(input$SFnjPR_seas2[2]), as.character(input$SFnjPR_2_bag), as.character(input$SFnjPR_2_len),
                                          as.character(input$SFnjSH_seas2[1]), as.character(input$SFnjSH_seas2[2]), as.character(input$SFnjSH_2_bag), as.character(input$SFnjSH_2_len)))
      }else{
        sfNJregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("NJ"),
                                input =  c("SFnjFH_seas1_op", "SFnjFH_seas1_cl", "SFnjFH_1_bag", "SFnjFH_1_len", 
                                           "SFnjPR_seas1_op", "SFnjPR_seas1_cl", "SFnjPR_1_bag", "SFnjPR_1_len",
                                           "SFnjSH_seas1_op", "SFnjSH_seas1_cl", "SFnjSH_1_bag", "SFnjSH_1_len",
                                           "SFnjFH_seas2_op", "SFnjFH_seas2_cl", "SFnjFH_2_bag", "SFnjFH_2_len", 
                                           "SFnjPR_seas2_op", "SFnjPR_seas2_cl", "SFnjPR_2_bag", "SFnjPR_2_len",
                                           "SFnjSH_seas2_op", "SFnjSH_seas2_cl", "SFnjSH_2_bag", "SFnjSH_2_len"),
                                value = c( as.character(input$SFnjFH_seas1[1]), as.character(input$SFnjFH_seas1[2]), as.character(input$SFnjFH_1_bag), as.character(input$SFnjFH_1_len), 
                                           as.character(input$SFnjPR_seas1[1]), as.character(input$SFnjPR_seas1[2]), as.character(input$SFnjPR_1_bag), as.character(input$SFnjPR_1_len),
                                           as.character(input$SFnjSH_seas1[1]), as.character(input$SFnjSH_seas1[2]), as.character(input$SFnjSH_1_bag), as.character(input$SFnjSH_1_len),
                                           as.character(input$SFnjFH_seas2[1]), as.character(input$SFnjFH_seas2[2]), as.character(input$SFnjFH_2_bag), as.character(input$SFnjFH_2_len), 
                                           as.character(input$SFnjPR_seas2[1]), as.character(input$SFnjPR_seas2[2]), as.character(input$SFnjPR_2_bag), as.character(input$SFnjPR_2_len),
                                           as.character(input$SFnjSH_seas2[1]), as.character(input$SFnjSH_seas2[2]), as.character(input$SFnjSH_2_bag), as.character(input$SFnjSH_2_len)))
      }
      
      
      if(input$BSB_NJ_input_type == "All Modes Combined"){
        bsbNJregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("NJ"),
                                input =  c("BSBnj_seas1_op", "BSBnj_seas1_cl", "BSBnj_1_bag", "BSBnj_1_len", 
                                           "BSBnj_seas2_op", "BSBnj_seas2_cl", "BSBnj_2_bag", "BSBnj_2_len", 
                                           "BSBnj_seas3_op", "BSBnj_seas3_cl", "BSBnj_3_bag", "BSBnj_3_len", 
                                           "BSBnj_seas4_op", "BSBnj_seas4_cl", "BSBnj_4_bag", "BSBnj_4_len", 
                                           "BSBnjFH_seas5_op", "BSBnjFH_seas5_cl", "BSBnjFH_5_bag", "BSBnjFH_5_len", 
                                           "BSBnjPR_seas5_op", "BSBnjPR_seas5_cl", "BSBnjPR_5_bag", "BSBnjPR_5_len",
                                           "BSBnjSH_seas5_op", "BSBnjSH_seas5_cl", "BSBnjSH_5_bag", "BSBnjSH_5_len"),
                                value =  c(as.character(input$BSBnj_seas1[1]), as.character(input$BSBnj_seas1[2]), as.character(input$BSBnj_1_bag), as.character(input$BSBnj_1_len), 
                                           as.character(input$BSBnj_seas2[1]), as.character(input$BSBnj_seas2[2]), as.character(input$BSBnj_2_bag), as.character(input$BSBnj_2_len), 
                                           as.character(input$BSBnj_seas3[1]), as.character(input$BSBnj_seas3[2]), as.character(input$BSBnj_3_bag), as.character(input$BSBnj_3_len), 
                                           as.character(input$BSBnj_seas4[1]), as.character(input$BSBnj_seas4[2]), as.character(input$BSBnj_4_bag), as.character(input$BSBnj_4_len),
                                           as.character(input$BSBnjFH_seas5[1]), as.character(input$BSBnjFH_seas5[2]), as.character(input$BSBnjFH_5_bag), as.character(input$BSBnjFH_5_len), 
                                           as.character(input$BSBnjPR_seas5[1]), as.character(input$BSBnjPR_seas5[2]), as.character(input$BSBnjPR_5_bag), as.character(input$BSBnjPR_5_len),
                                           as.character(input$BSBnjSH_seas5[1]), as.character(input$BSBnjSH_seas5[2]), as.character(input$BSBnjSH_5_bag), as.character(input$BSBnjSH_5_len)))
      }else{
        bsbNJregs <-  data.frame(run_name = c(Run_Name()), 
                                 state = c("NJ"),
                                 input =  c( "BSBnjFH_seas1_op", "BSBnjFH_seas1_cl", "BSBnjFH_1_bag", "BSBnjFH_1_len",
                                             "BSBnjPR_seas1_op", "BSBnjPR_seas1_cl", "BSBnjPR_1_bag", "BSBnjPR_1_len",
                                             "BSBnjSH_seas1_op", "BSBnjSH_seas1_cl", "BSBnjSH_1_bag", "BSBnjSH_1_len",
                                             "BSBnjFH_seas2_op", "BSBnjFH_seas2_cl", "BSBnjFH_2_bag", "BSBnjFH_2_len", 
                                             "BSBnjPR_seas2_op", "BSBnjPR_seas2_cl", "BSBnjPR_2_bag", "BSBnjPR_2_len",
                                             "BSBnjSH_seas2_op", "BSBnjSH_seas2_cl", "BSBnjSH_2_bag", "BSBnjSH_2_len",
                                             "BSBnjFH_seas3_op", "BSBnjFH_seas3_cl", "BSBnjFH_3_bag", "BSBnjFH_3_len", 
                                             "BSBnjPR_seas3_op", "BSBnjPR_seas3_cl", "BSBnjPR_3_bag", "BSBnjPR_3_len",
                                             "BSBnjSH_seas3_op", "BSBnjSH_seas3_cl", "BSBnjSH_3_bag", "BSBnjSH_3_len",
                                             "BSBnjFH_seas4_op", "BSBnjFH_seas4_cl", "BSBnjFH_4_bag", "BSBnjFH_4_len",
                                             "BSBnjPR_seas4_op", "BSBnjPR_seas4_cl", "BSBnjPR_4_bag", "BSBnjPR_4_len",
                                             "BSBnjSH_seas4_op", "BSBnjSH_seas4_cl", "BSBnjSH_4_bag", "BSBnjSH_4_len",
                                             "BSBnjFH_seas5_op", "BSBnjFH_seas5_cl", "BSBnjFH_5_bag", "BSBnjFH_5_len", 
                                             "BSBnjPR_seas5_op", "BSBnjPR_seas5_cl", "BSBnjPR_5_bag", "BSBnjPR_5_len",
                                             "BSBnjSH_seas5_op", "BSBnjSH_seas5_cl", "BSBnjSH_5_bag", "BSBnjSH_5_len"),
                                 value = c( as.character(input$BSBnjFH_seas1[1]), as.character(input$BSBnjFH_seas1[2]), as.character(input$BSBnjFH_1_bag), as.character(input$BSBnjFH_1_len),
                                            as.character(input$BSBnjPR_seas1[1]), as.character(input$BSBnjPR_seas1[2]), as.character(input$BSBnjPR_1_bag), as.character(input$BSBnjPR_1_len), 
                                            as.character(input$BSBnjSH_seas1[1]), as.character(input$BSBnjSH_seas1[2]), as.character(input$BSBnjSH_1_bag), as.character(input$BSBnjSH_1_len), 
                                            as.character(input$BSBnjFH_seas2[1]), as.character(input$BSBnjFH_seas2[2]), as.character(input$BSBnjFH_2_bag), as.character(input$BSBnjFH_2_len), 
                                            as.character(input$BSBnjPR_seas2[1]), as.character(input$BSBnjPR_seas2[2]), as.character(input$BSBnjPR_2_bag), as.character(input$BSBnjPR_2_len),
                                            as.character(input$BSBnjSH_seas2[1]), as.character(input$BSBnjSH_seas2[2]), as.character(input$BSBnjSH_2_bag), as.character(input$BSBnjSH_2_len), 
                                            as.character(input$BSBnjFH_seas3[1]), as.character(input$BSBnjFH_seas3[2]), as.character(input$BSBnjFH_3_bag), as.character(input$BSBnjFH_3_len), 
                                            as.character(input$BSBnjPR_seas3[1]), as.character(input$BSBnjPR_seas3[2]), as.character(input$BSBnjPR_3_bag), as.character(input$BSBnjPR_3_len),
                                            as.character(input$BSBnjSH_seas3[1]), as.character(input$BSBnjSH_seas3[2]), as.character(input$BSBnjSH_3_bag), as.character(input$BSBnjSH_3_len), 
                                            as.character(input$BSBnjFH_seas4[1]), as.character(input$BSBnjFH_seas4[2]), as.character(input$BSBnjFH_4_bag), as.character(input$BSBnjFH_4_len),
                                            as.character(input$BSBnjPR_seas4[1]), as.character(input$BSBnjPR_seas4[2]), as.character(input$BSBnjPR_4_bag), as.character(input$BSBnjPR_4_len), 
                                            as.character(input$BSBnjSH_seas4[1]), as.character(input$BSBnjSH_seas4[2]), as.character(input$BSBnjSH_4_bag), as.character(input$BSBnjSH_4_len), 
                                            as.character(input$BSBnjFH_seas5[1]), as.character(input$BSBnjFH_seas5[2]), as.character(input$BSBnjFH_5_bag), as.character(input$BSBnjFH_5_len), 
                                            as.character(input$BSBnjPR_seas5[1]), as.character(input$BSBnjPR_seas5[2]), as.character(input$BSBnjPR_5_bag), as.character(input$BSBnjPR_5_len),
                                            as.character(input$BSBnjSH_seas5[1]), as.character(input$BSBnjSH_seas5[2]), as.character(input$BSBnjSH_5_bag), as.character(input$BSBnjSH_5_len)))
      }
      
      
      if(input$SCUP_NJ_input_type == "All Modes Combined"){
        scupNJregs <- data.frame(run_name = c(Run_Name()), 
                                 state = c("NJ"),
                                 input =  c("SCUPnj_seas1_op", "SCUPnj_seas1_cl", "SCUPnj_1_bag", "SCUPnj_1_len", 
                                            "SCUPnj_seas2_op", "SCUPnj_seas2_cl", "SCUPnj_2_bag", "SCUPnj_2_len",
                                            "SCUPnjFH_seas3_op", "SCUPnjFH_seas3_cl", "SCUPnjFH_3_bag", "SCUPnjFH_3_len", 
                                            "SCUPnjPR_seas3_op", "SCUPnjPR_seas3_cl", "SCUPnjPR_3_bag", "SCUPnjPR_3_len",
                                            "SCUPnjSH_seas3_op", "SCUPnjSH_seas3_cl", "SCUPnjSH_3_bag", "SCUPnjSH_3_len"),
                                 value =  c(as.character(input$SCUPnj_seas1[1]), as.character(input$SCUPnj_seas1[2]), as.character(input$SCUPnj_1_bag), as.character(input$SCUPnj_1_len),
                                            as.character(input$SCUPnj_seas2[1]), as.character(input$SCUPnj_seas2[2]), as.character(input$SCUPnj_2_bag), as.character(input$SCUPnj_2_len), 
                                            as.character(input$SCUPnjFH_seas3[1]), as.character(input$SCUPnjFH_seas3[2]), as.character(input$SCUPnjFH_3_bag), as.character(input$SCUPnjFH_3_len), 
                                            as.character(input$SCUPnjPR_seas3[1]), as.character(input$SCUPnjPR_seas3[2]), as.character(input$SCUPnjPR_3_bag), as.character(input$SCUPnjPR_3_len),
                                            as.character(input$SCUPnjSH_seas3[1]), as.character(input$SCUPnjSH_seas3[2]), as.character(input$SCUPnjSH_3_bag), as.character(input$SCUPnjSH_3_len)))
      }else{
        scupbNJregs <-  data.frame(run_name = c(Run_Name()),
                                   state = c("NJ"),
                                   input =  c( "SCUPnjFH_seas1_op", "SCUPnjFH_seas1_cl", "SCUPnjFH_1_bag", "SCUPnjFH_1_len", 
                                               "SCUPnjPR_seas1_op", "SCUPnjPR_seas1_cl", "SCUPnjPR_1_bag", "SCUPnjPR_1_len",
                                               "SCUPnjSH_seas1_op", "SCUPnjSH_seas1_cl", "SCUPnjSH_1_bag", "SCUPnjSH_1_len",
                                               "SCUPnjFH_seas2_op", "SCUPnjFH_seas2_cl", "SCUPnjFH_2_bag", "SCUPnjFH_2_len", 
                                               "SCUPnjPR_seas2_op", "SCUPnjPR_seas2_cl", "SCUPnjPR_2_bag", "SCUPnjPR_2_len",
                                               "SCUPnjSH_seas2_op", "SCUPnjSH_seas2_cl", "SCUPnjSH_2_bag", "SCUPnjSH_2_len", 
                                               "SCUPnjFH_seas3_op", "SCUPnjFH_seas3_cl", "SCUPnjFH_3_bag", "SCUPnjFH_3_len", 
                                               "SCUPnjPR_seas3_op", "SCUPnjPR_seas3_cl", "SCUPnjPR_3_bag", "SCUPnjPR_3_len",
                                               "SCUPnjSH_seas3_op", "SCUPnjSH_seas3_cl", "SCUPnjSH_3_bag", "SCUPnjSH_3_len"),
                                   value =  c(as.character(input$SCUPnjFH_seas1[1]), as.character(input$SCUPnjFH_seas1[2]), as.character(input$SCUPnjFH_1_bag), as.character(input$SCUPnjFH_1_len), 
                                              as.character(input$SCUPnjPR_seas1[1]), as.character(input$SCUPnjPR_seas1[2]), as.character(input$SCUPnjPR_1_bag), as.character(input$SCUPnjPR_1_len),
                                              as.character(input$SCUPnjSH_seas1[1]), as.character(input$SCUPnjSH_seas1[2]), as.character(input$SCUPnjSH_1_bag), as.character(input$SCUPnjSH_1_len),
                                              as.character(input$SCUPnjFH_seas2[1]), as.character(input$SCUPnjFH_seas2[2]), as.character(input$SCUPnjFH_2_bag), as.character(input$SCUPnjFH_2_len), 
                                              as.character(input$SCUPnjPR_seas2[1]), as.character(input$SCUPnjPR_seas2[2]), as.character(input$SCUPnjPR_2_bag), as.character(input$SCUPnjPR_2_len),
                                              as.character(input$SCUPnjSH_seas2[1]), as.character(input$SCUPnjSH_seas2[2]), as.character(input$SCUPnjSH_2_bag), as.character(input$SCUPnjSH_2_len),
                                              as.character(input$SCUPnjFH_seas3[1]), as.character(input$SCUPnjFH_seas3[2]), as.character(input$SCUPnjFH_3_bag), as.character(input$SCUPnjFH_3_len), 
                                              as.character(input$SCUPnjPR_seas3[1]), as.character(input$SCUPnjPR_seas3[2]), as.character(input$SCUPnjPR_3_bag), as.character(input$SCUPnjPR_3_len),
                                              as.character(input$SCUPnjSH_seas3[1]), as.character(input$SCUPnjSH_seas3[2]), as.character(input$SCUPnjSH_3_bag), as.character(input$SCUPnjSH_3_len)))
      }
      regulations <- regulations %>% rbind(sfNJregs, bsbNJregs, scupNJregs)
      
    }
    
    if(any("DE" == input$state)){
      if(input$SF_DE_input_type == "All Modes Combined"){
        sfDEregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("DE"),
                               input =  c("SFde_seas1_op", "SFde_seas1_cl", "SFde_1_bag", "SFde_1_len", 
                                          "SFde_seas2_op", "SFde_seas2_cl", "SFde_2_bag", "SFde_2_len", 
                                          "SFdeFH_seas3_op", "SFdeFH_seas3_cl", "SFdeFH_3_bag", "SFdeFH_3_len", 
                                          "SFdePR_seas3_op", "SFdePR_seas3_cl", "SFdePR_3_bag", "SFdePR_3_len",
                                          "SFdeSH_seas3_op", "SFdeSH_seas3_cl", "SFdeSH_3_bag", "SFdeSH_3_len"),
                               value =  c(as.character(input$SFde_seas1[1]), as.character(input$SFde_seas1[2]), as.character(input$SFde_1_bag), as.character(input$SFde_1_len), 
                                          as.character(input$SFde_seas2[1]), as.character(input$SFde_seas2[2]), as.character(input$SFde_2_bag), as.character(input$SFde_2_len),
                                          as.character(input$SFdeFH_seas3[1]), as.character(input$SFdeFH_seas3[2]), as.character(input$SFdeFH_3_bag), as.character(input$SFdeFH_3_len), 
                                          as.character(input$SFdePR_seas3[1]), as.character(input$SFdePR_seas3[2]), as.character(input$SFdePR_3_bag), as.character(input$SFdePR_3_len),
                                          as.character(input$SFdeSH_seas3[1]), as.character(input$SFdeSH_seas3[2]), as.character(input$SFdeSH_3_bag), as.character(input$SFdeSH_3_len)))
      }else{
        sfDEregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("DE"),
                                input =  c("SFdeFH_seas1_op", "SFdeFH_seas1_cl", "SFdeFH_1_bag", "SFdeFH_1_len", 
                                           "SFdePR_seas1_op", "SFdePR_seas1_cl", "SFdePR_1_bag", "SFdePR_1_len",
                                           "SFdeSH_seas1_op", "SFdeSH_seas1_cl", "SFdeSH_1_bag", "SFdeSH_1_len",
                                           "SFdeFH_seas2_op", "SFdeFH_seas2_cl", "SFdeFH_2_bag", "SFdeFH_2_len", 
                                           "SFdePR_seas2_op", "SFdePR_seas2_cl", "SFdePR_2_bag", "SFdePR_2_len",
                                           "SFdeSH_seas2_op", "SFdeSH_seas2_cl", "SFdeSH_2_bag", "SFdeSH_2_len", 
                                           "SFdeFH_seas3_op", "SFdeFH_seas3_cl", "SFdeFH_3_bag", "SFdeFH_3_len", 
                                           "SFdePR_seas3_op", "SFdePR_seas3_cl", "SFdePR_3_bag", "SFdePR_3_len",
                                           "SFdeSH_seas3_op", "SFdeSH_seas3_cl", "SFdeSH_3_bag", "SFdeSH_3_len"),
                                value = c( as.character(input$SFdeFH_seas1[1]), as.character(input$SFdeFH_seas1[2]), as.character(input$SFdeFH_1_bag), as.character(input$SFdeFH_1_len), 
                                           as.character(input$SFdePR_seas1[1]), as.character(input$SFdePR_seas1[2]), as.character(input$SFdePR_1_bag), as.character(input$SFdePR_1_len),
                                           as.character(input$SFdeSH_seas1[1]), as.character(input$SFdeSH_seas1[2]), as.character(input$SFdeSH_1_bag), as.character(input$SFdeSH_1_len),
                                           as.character(input$SFdeFH_seas2[1]), as.character(input$SFdeFH_seas2[2]), as.character(input$SFdeFH_2_bag), as.character(input$SFdeFH_2_len), 
                                           as.character(input$SFdePR_seas2[1]), as.character(input$SFdePR_seas2[2]), as.character(input$SFdePR_2_bag), as.character(input$SFdePR_2_len),
                                           as.character(input$SFdeSH_seas2[1]), as.character(input$SFdeSH_seas2[2]), as.character(input$SFdeSH_2_bag), as.character(input$SFdeSH_2_len),
                                           as.character(input$SFdeFH_seas3[1]), as.character(input$SFdeFH_seas3[2]), as.character(input$SFdeFH_3_bag), as.character(input$SFdeFH_3_len), 
                                           as.character(input$SFdePR_seas3[1]), as.character(input$SFdePR_seas3[2]), as.character(input$SFdePR_3_bag), as.character(input$SFdePR_3_len),
                                           as.character(input$SFdeSH_seas3[1]), as.character(input$SFdeSH_seas3[2]), as.character(input$SFdeSH_3_bag), as.character(input$SFdeSH_3_len)))
      }
      
      
      if(input$BSB_DE_input_type == "All Modes Combined"){
        bsbDEregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("DE"),
                                input =  c("BSBde_seas1_op", "BSBde_seas1_cl", "BSBde_1_bag", "BSBde_1_len", 
                                           "BSBde_seas2_op", "BSBde_seas2_cl", "BSBde_2_bag", "BSBde_2_len", 
                                           "BSBdeFH_seas3_op", "BSBdeFH_seas3_cl", "BSBdeFH_3_bag", "BSBdeFH_3_len", 
                                           "BSBdePR_seas3_op", "BSBdePR_seas3_cl", "BSBdePR_3_bag", "BSBdePR_3_len",
                                           "BSBdeSH_seas3_op", "BSBdeSH_seas3_cl", "BSBdeSH_3_bag", "BSBdeSH_3_len"),
                                value =  c(as.character(input$BSBde_seas1[1]), as.character(input$BSBde_seas1[2]), as.character(input$BSBde_1_bag), as.character(input$BSBde_1_len), 
                                           as.character(input$BSBde_seas2[1]), as.character(input$BSBde_seas2[2]), as.character(input$BSBde_2_bag), as.character(input$BSBde_2_len), 
                                           as.character(input$BSBdeFH_seas3[1]), as.character(input$BSBdeFH_seas3[2]), as.character(input$BSBdeFH_3_bag), as.character(input$BSBdeFH_3_len), 
                                           as.character(input$BSBdePR_seas3[1]), as.character(input$BSBdePR_seas3[2]), as.character(input$BSBdePR_3_bag), as.character(input$BSBdePR_3_len),
                                           as.character(input$BSBdeSH_seas3[1]), as.character(input$BSBdeSH_seas3[2]), as.character(input$BSBdeSH_3_bag), as.character(input$BSBdeSH_3_len)))
      }else{
        bsbDEregs <-  data.frame(run_name = c(Run_Name()), 
                                 state = c("DE"),
                                 input =  c( "BSBdeFH_seas1_op", "BSBdeFH_seas1_cl", "BSBdeFH_1_bag", "BSBdeFH_1_len",
                                             "BSBdePR_seas1_op", "BSBdePR_seas1_cl", "BSBdePR_1_bag", "BSBdePR_1_len",
                                             "BSBdeSH_seas1_op", "BSBdeSH_seas1_cl", "BSBdeSH_1_bag", "BSBdeSH_1_len",
                                             "BSBdeFH_seas2_op", "BSBdeFH_seas2_cl", "BSBdeFH_2_bag", "BSBdeFH_2_len", 
                                             "BSBdePR_seas2_op", "BSBdePR_seas2_cl", "BSBdePR_2_bag", "BSBdePR_2_len",
                                             "BSBdeSH_seas2_op", "BSBdeSH_seas2_cl", "BSBdeSH_2_bag", "BSBdeSH_2_len",
                                             "BSBdeFH_seas3_op", "BSBdeFH_seas3_cl", "BSBdeFH_3_bag", "BSBdeFH_3_len", 
                                             "BSBdePR_seas3_op", "BSBdePR_seas3_cl", "BSBdePR_3_bag", "BSBdePR_3_len",
                                             "BSBdeSH_seas3_op", "BSBdeSH_seas3_cl", "BSBdeSH_3_bag", "BSBdeSH_3_len"),
                                 value = c( as.character(input$BSBdeFH_seas1[1]), as.character(input$BSBdeFH_seas1[2]), as.character(input$BSBdeFH_1_bag), as.character(input$BSBdeFH_1_len),
                                            as.character(input$BSBdePR_seas1[1]), as.character(input$BSBdePR_seas1[2]), as.character(input$BSBdePR_1_bag), as.character(input$BSBdePR_1_len), 
                                            as.character(input$BSBdeSH_seas1[1]), as.character(input$BSBdeSH_seas1[2]), as.character(input$BSBdeSH_1_bag), as.character(input$BSBdeSH_1_len), 
                                            as.character(input$BSBdeFH_seas2[1]), as.character(input$BSBdeFH_seas2[2]), as.character(input$BSBdeFH_2_bag), as.character(input$BSBdeFH_2_len), 
                                            as.character(input$BSBdePR_seas2[1]), as.character(input$BSBdePR_seas2[2]), as.character(input$BSBdePR_2_bag), as.character(input$BSBdePR_2_len),
                                            as.character(input$BSBdeSH_seas2[1]), as.character(input$BSBdeSH_seas2[2]), as.character(input$BSBdeSH_2_bag), as.character(input$BSBdeSH_2_len), 
                                            as.character(input$BSBdeFH_seas3[1]), as.character(input$BSBdeFH_seas3[2]), as.character(input$BSBdeFH_3_bag), as.character(input$BSBdeFH_3_len), 
                                            as.character(input$BSBdePR_seas3[1]), as.character(input$BSBdePR_seas3[2]), as.character(input$BSBdePR_3_bag), as.character(input$BSBdePR_3_len),
                                            as.character(input$BSBdeSH_seas3[1]), as.character(input$BSBdeSH_seas3[2]), as.character(input$BSBdeSH_3_bag), as.character(input$BSBdeSH_3_len)))
      }
      
      
      if(input$SCUP_DE_input_type == "All Modes Combined"){
        scupDEregs <- data.frame(run_name = c(Run_Name()),
                                 state = c("DE"),
                                 input =  c("SCUPde_seas1_op", "SCUPde_seas1_cl", "SCUPde_1_bag", "SCUPde_1_len", 
                                            "SCUPdeFH_seas2_op", "SCUPdeFH_seas2_cl", "SCUPdeFH_2_bag", "SCUPdeFH_2_len", 
                                            "SCUPdePR_seas2_op", "SCUPdePR_seas2_cl", "SCUPdePR_2_bag", "SCUPdePR_2_len",
                                            "SCUPdeSH_seas2_op", "SCUPdeSH_seas2_cl", "SCUPdeSH_2_bag", "SCUPdeSH_2_len"),
                                 value =  c(as.character(input$SCUPde_seas1[1]), as.character(input$SCUPde_seas1[2]), as.character(input$SCUPde_1_bag), as.character(input$SCUPde_1_len), 
                                            as.character(input$SCUPdeFH_seas2[1]), as.character(input$SCUPdeFH_seas2[2]), as.character(input$SCUPdeFH_2_bag), as.character(input$SCUPdeFH_2_len), 
                                            as.character(input$SCUPdePR_seas2[1]), as.character(input$SCUPdePR_seas2[2]), as.character(input$SCUPdePR_2_bag), as.character(input$SCUPdePR_2_len),
                                            as.character(input$SCUPdeSH_seas2[1]), as.character(input$SCUPdeSH_seas2[2]), as.character(input$SCUPdeSH_2_bag), as.character(input$SCUPdeSH_2_len)))
      }else{
        scupDEregs <-  data.frame(run_name = c(Run_Name()), 
                                  state = c("DE"),
                                  input =  c( "SCUPdeFH_seas1_op", "SCUPdeFH_seas1_cl", "SCUPdeFH_1_bag", "SCUPdeFH_1_len", 
                                              "SCUPdePR_seas1_op", "SCUPdePR_seas1_cl", "SCUPdePR_1_bag", "SCUPdePR_1_len",
                                              "SCUPdeSH_seas1_op", "SCUPdeSH_seas1_cl", "SCUPdeSH_1_bag", "SCUPdeSH_1_len",
                                              "SCUPdeFH_seas2_op", "SCUPdeFH_seas2_cl", "SCUPdeFH_2_bag", "SCUPdeFH_2_len", 
                                              "SCUPdePR_seas2_op", "SCUPdePR_seas2_cl", "SCUPdePR_2_bag", "SCUPdePR_2_len",
                                              "SCUPdeSH_seas2_op", "SCUPdeSH_seas2_cl", "SCUPdeSH_2_bag", "SCUPdeSH_2_len"),
                                  value =  c(as.character(input$SCUPdeFH_seas1[1]), as.character(input$SCUPdeFH_seas1[2]), as.character(input$SCUPdeFH_1_bag), as.character(input$SCUPdeFH_1_len), 
                                             as.character(input$SCUPdePR_seas1[1]), as.character(input$SCUPdePR_seas1[2]), as.character(input$SCUPdePR_1_bag), as.character(input$SCUPdePR_1_len),
                                             as.character(input$SCUPdeSH_seas1[1]), as.character(input$SCUPdeSH_seas1[2]), as.character(input$SCUPdeSH_1_bag), as.character(input$SCUPdeSH_1_len),
                                             as.character(input$SCUPdeFH_seas2[1]), as.character(input$SCUPdeFH_seas2[2]), as.character(input$SCUPdeFH_2_bag), as.character(input$SCUPdeFH_2_len), 
                                             as.character(input$SCUPdePR_seas2[1]), as.character(input$SCUPdePR_seas2[2]), as.character(input$SCUPdePR_2_bag), as.character(input$SCUPdePR_2_len),
                                             as.character(input$SCUPdeSH_seas2[1]), as.character(input$SCUPdeSH_seas2[2]), as.character(input$SCUPdeSH_2_bag), as.character(input$SCUPdeSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfDEregs, bsbDEregs, scupDEregs)
      
    }
    
    if(any("MD" == input$state)){
      if(input$SF_MD_input_type == "All Modes Combined"){
        sfMDregs <- data.frame(run_name = c(Run_Name()),
                               state = c("MD"),
                               input =  c("SFmd_seas1_op", "SFmd_seas1_cl", "SFmd_1_bag", "SFmd_1_len", 
                                          "SFmd_seas2_op", "SFmd_seas2_cl", "SFmd_2_bag", "SFmd_2_len", 
                                          "SFmdFH_seas3_op", "SFmdFH_seas3_cl", "SFmdFH_3_bag", "SFmdFH_3_len", 
                                          "SFmdPR_seas3_op", "SFmdPR_seas3_cl", "SFmdPR_3_bag", "SFmdPR_3_len",
                                          "SFmdSH_seas3_op", "SFmdSH_seas3_cl", "SFmdSH_3_bag", "SFmdSH_3_len"),
                               value =  c(as.character(input$SFmd_seas1[1]), as.character(input$SFmd_seas1[2]), as.character(input$SFmd_1_bag), as.character(input$SFmd_1_len), 
                                          as.character(input$SFmd_seas2[1]), as.character(input$SFmd_seas2[2]), as.character(input$SFmd_2_bag), as.character(input$SFmd_2_len),
                                          as.character(input$SFmdFH_seas3[1]), as.character(input$SFmdFH_seas3[2]), as.character(input$SFmdFH_3_bag), as.character(input$SFmdFH_3_len), 
                                          as.character(input$SFmdPR_seas3[1]), as.character(input$SFmdPR_seas3[2]), as.character(input$SFmdPR_3_bag), as.character(input$SFmdPR_3_len),
                                          as.character(input$SFmdSH_seas3[1]), as.character(input$SFmdSH_seas3[2]), as.character(input$SFmdSH_3_bag), as.character(input$SFmdSH_3_len)))
      }else{
        sfMDregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("MD"),
                                input =  c("SFmdFH_seas1_op", "SFmdFH_seas1_cl", "SFmdFH_1_bag", "SFmdFH_1_len", 
                                           "SFmdPR_seas1_op", "SFmdPR_seas1_cl", "SFmdPR_1_bag", "SFmdPR_1_len",
                                           "SFmdSH_seas1_op", "SFmdSH_seas1_cl", "SFmdSH_1_bag", "SFmdSH_1_len",
                                           "SFmdFH_seas2_op", "SFmdFH_seas2_cl", "SFmdFH_2_bag", "SFmdFH_2_len", 
                                           "SFmdPR_seas2_op", "SFmdPR_seas2_cl", "SFmdPR_2_bag", "SFmdPR_2_len",
                                           "SFmdSH_seas2_op", "SFmdSH_seas2_cl", "SFmdSH_2_bag", "SFmdSH_2_len", 
                                           "SFmdFH_seas3_op", "SFmdFH_seas3_cl", "SFmdFH_3_bag", "SFmdFH_3_len", 
                                           "SFmdPR_seas3_op", "SFmdPR_seas3_cl", "SFmdPR_3_bag", "SFmdPR_3_len",
                                           "SFmdSH_seas3_op", "SFmdSH_seas3_cl", "SFmdSH_3_bag", "SFmdSH_3_len"),
                                value = c( as.character(input$SFmdFH_seas1[1]), as.character(input$SFmdFH_seas1[2]), as.character(input$SFmdFH_1_bag), as.character(input$SFmdFH_1_len), 
                                           as.character(input$SFmdPR_seas1[1]), as.character(input$SFmdPR_seas1[2]), as.character(input$SFmdPR_1_bag), as.character(input$SFmdPR_1_len),
                                           as.character(input$SFmdSH_seas1[1]), as.character(input$SFmdSH_seas1[2]), as.character(input$SFmdSH_1_bag), as.character(input$SFmdSH_1_len),
                                           as.character(input$SFmdFH_seas2[1]), as.character(input$SFmdFH_seas2[2]), as.character(input$SFmdFH_2_bag), as.character(input$SFmdFH_2_len), 
                                           as.character(input$SFmdPR_seas2[1]), as.character(input$SFmdPR_seas2[2]), as.character(input$SFmdPR_2_bag), as.character(input$SFmdPR_2_len),
                                           as.character(input$SFmdSH_seas2[1]), as.character(input$SFmdSH_seas2[2]), as.character(input$SFmdSH_2_bag), as.character(input$SFmdSH_2_len),
                                           as.character(input$SFmdFH_seas3[1]), as.character(input$SFmdFH_seas3[2]), as.character(input$SFmdFH_3_bag), as.character(input$SFmdFH_3_len), 
                                           as.character(input$SFmdPR_seas3[1]), as.character(input$SFmdPR_seas3[2]), as.character(input$SFmdPR_3_bag), as.character(input$SFmdPR_3_len),
                                           as.character(input$SFmdSH_seas3[1]), as.character(input$SFmdSH_seas3[2]), as.character(input$SFmdSH_3_bag), as.character(input$SFmdSH_3_len)))
      }
      
      
      if(input$BSB_MD_input_type == "All Modes Combined"){
        bsbMDregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("MD"),
                                input =  c("BSBmd_seas1_op", "BSBmd_seas1_cl", "BSBmd_1_bag", "BSBmd_1_len", 
                                           "BSBmd_seas2_op", "BSBmd_seas2_cl", "BSBmd_2_bag", "BSBmd_2_len", 
                                           "BSBmdFH_seas3_op", "BSBmdFH_seas3_cl", "BSBmdFH_3_bag", "BSBmdFH_3_len", 
                                           "BSBmdPR_seas3_op", "BSBmdPR_seas3_cl", "BSBmdPR_3_bag", "BSBmdPR_3_len",
                                           "BSBmdSH_seas3_op", "BSBmdSH_seas3_cl", "BSBmdSH_3_bag", "BSBmdSH_3_len"),
                                value =  c(as.character(input$BSBmd_seas1[1]), as.character(input$BSBmd_seas1[2]), as.character(input$BSBmd_1_bag), as.character(input$BSBmd_1_len), 
                                           as.character(input$BSBmd_seas2[1]), as.character(input$BSBmd_seas2[2]), as.character(input$BSBmd_2_bag), as.character(input$BSBmd_2_len), 
                                           as.character(input$BSBmdFH_seas3[1]), as.character(input$BSBmdFH_seas3[2]), as.character(input$BSBmdFH_3_bag), as.character(input$BSBmdFH_3_len), 
                                           as.character(input$BSBmdPR_seas3[1]), as.character(input$BSBmdPR_seas3[2]), as.character(input$BSBmdPR_3_bag), as.character(input$BSBmdPR_3_len),
                                           as.character(input$BSBmdSH_seas3[1]), as.character(input$BSBmdSH_seas3[2]), as.character(input$BSBmdSH_3_bag), as.character(input$BSBmdSH_3_len)))
      }else{
        bsbMDregs <-  data.frame(run_name = c(Run_Name()), 
                                 state = c("MD"),
                                 input =  c( "BSBmdFH_seas1_op", "BSBmdFH_seas1_cl", "BSBmdFH_1_bag", "BSBmdFH_1_len",
                                             "BSBmdPR_seas1_op", "BSBmdPR_seas1_cl", "BSBmdPR_1_bag", "BSBmdPR_1_len",
                                             "BSBmdSH_seas1_op", "BSBmdSH_seas1_cl", "BSBmdSH_1_bag", "BSBmdSH_1_len",
                                             "BSBmdFH_seas2_op", "BSBmdFH_seas2_cl", "BSBmdFH_2_bag", "BSBmdFH_2_len", 
                                             "BSBmdPR_seas2_op", "BSBmdPR_seas2_cl", "BSBmdPR_2_bag", "BSBmdPR_2_len",
                                             "BSBmdSH_seas2_op", "BSBmdSH_seas2_cl", "BSBmdSH_2_bag", "BSBmdSH_2_len",
                                             "BSBmdFH_seas3_op", "BSBmdFH_seas3_cl", "BSBmdFH_3_bag", "BSBmdFH_3_len", 
                                             "BSBmdPR_seas3_op", "BSBmdPR_seas3_cl", "BSBmdPR_3_bag", "BSBmdPR_3_len",
                                             "BSBmdSH_seas3_op", "BSBmdSH_seas3_cl", "BSBmdSH_3_bag", "BSBmdSH_3_len"),
                                 value = c( as.character(input$BSBmdFH_seas1[1]), as.character(input$BSBmdFH_seas1[2]), as.character(input$BSBmdFH_1_bag), as.character(input$BSBmdFH_1_len),
                                            as.character(input$BSBmdPR_seas1[1]), as.character(input$BSBmdPR_seas1[2]), as.character(input$BSBmdPR_1_bag), as.character(input$BSBmdPR_1_len), 
                                            as.character(input$BSBmdSH_seas1[1]), as.character(input$BSBmdSH_seas1[2]), as.character(input$BSBmdSH_1_bag), as.character(input$BSBmdSH_1_len), 
                                            as.character(input$BSBmdFH_seas2[1]), as.character(input$BSBmdFH_seas2[2]), as.character(input$BSBmdFH_2_bag), as.character(input$BSBmdFH_2_len), 
                                            as.character(input$BSBmdPR_seas2[1]), as.character(input$BSBmdPR_seas2[2]), as.character(input$BSBmdPR_2_bag), as.character(input$BSBmdPR_2_len),
                                            as.character(input$BSBmdSH_seas2[1]), as.character(input$BSBmdSH_seas2[2]), as.character(input$BSBmdSH_2_bag), as.character(input$BSBmdSH_2_len), 
                                            as.character(input$BSBmdFH_seas3[1]), as.character(input$BSBmdFH_seas3[2]), as.character(input$BSBmdFH_3_bag), as.character(input$BSBmdFH_3_len), 
                                            as.character(input$BSBmdPR_seas3[1]), as.character(input$BSBmdPR_seas3[2]), as.character(input$BSBmdPR_3_bag), as.character(input$BSBmdPR_3_len),
                                            as.character(input$BSBmdSH_seas3[1]), as.character(input$BSBmdSH_seas3[2]), as.character(input$BSBmdSH_3_bag), as.character(input$BSBmdSH_3_len)))
      }
      
      
      if(input$SCUP_MD_input_type == "All Modes Combined"){
        scupMDregs <- data.frame(run_name = c(Run_Name()), 
                                 state = c("MD"),
                                 input =  c("SCUPmd_seas1_op", "SCUPmd_seas1_cl", "SCUPmd_1_bag", "SCUPmd_1_len", 
                                            "SCUPmdFH_seas2_op", "SCUPmdFH_seas2_cl", "SCUPmdFH_2_bag", "SCUPmdFH_2_len", 
                                            "SCUPmdPR_seas2_op", "SCUPmdPR_seas2_cl", "SCUPmdPR_2_bag", "SCUPmdPR_2_len",
                                            "SCUPmdSH_seas2_op", "SCUPmdSH_seas2_cl", "SCUPmdSH_2_bag", "SCUPmdSH_2_len"),
                                 value =  c(as.character(input$SCUPmd_seas1[1]), as.character(input$SCUPmd_seas1[2]), as.character(input$SCUPmd_1_bag), as.character(input$SCUPmd_1_len), 
                                            as.character(input$SCUPmdFH_seas2[1]), as.character(input$SCUPmdFH_seas2[2]), as.character(input$SCUPmdFH_2_bag), as.character(input$SCUPmdFH_2_len), 
                                            as.character(input$SCUPmdPR_seas2[1]), as.character(input$SCUPmdPR_seas2[2]), as.character(input$SCUPmdPR_2_bag), as.character(input$SCUPmdPR_2_len),
                                            as.character(input$SCUPmdSH_seas2[1]), as.character(input$SCUPmdSH_seas2[2]), as.character(input$SCUPmdSH_2_bag), as.character(input$SCUPmdSH_2_len)))
      }else{
        scupMDregs <-  data.frame(run_name = c(Run_Name()), 
                                  state = c("MD"),
                                  input =  c( "SCUPmdFH_seas1_op", "SCUPmdFH_seas1_cl", "SCUPmdFH_1_bag", "SCUPmdFH_1_len", 
                                              "SCUPmdPR_seas1_op", "SCUPmdPR_seas1_cl", "SCUPmdPR_1_bag", "SCUPmdPR_1_len",
                                              "SCUPmdSH_seas1_op", "SCUPmdSH_seas1_cl", "SCUPmdSH_1_bag", "SCUPmdSH_1_len",
                                              "SCUPmdFH_seas2_op", "SCUPmdFH_seas2_cl", "SCUPmdFH_2_bag", "SCUPmdFH_2_len", 
                                              "SCUPmdPR_seas2_op", "SCUPmdPR_seas2_cl", "SCUPmdPR_2_bag", "SCUPmdPR_2_len",
                                              "SCUPmdSH_seas2_op", "SCUPmdSH_seas2_cl", "SCUPmdSH_2_bag", "SCUPmdSH_2_len"),
                                  value =  c(as.character(input$SCUPmdFH_seas1[1]), as.character(input$SCUPmdFH_seas1[2]), as.character(input$SCUPmdFH_1_bag), as.character(input$SCUPmdFH_1_len), 
                                             as.character(input$SCUPmdPR_seas1[1]), as.character(input$SCUPmdPR_seas1[2]), as.character(input$SCUPmdPR_1_bag), as.character(input$SCUPmdPR_1_len),
                                             as.character(input$SCUPmdSH_seas1[1]), as.character(input$SCUPmdSH_seas1[2]), as.character(input$SCUPmdSH_1_bag), as.character(input$SCUPmdSH_1_len),
                                             as.character(input$SCUPmdFH_seas2[1]), as.character(input$SCUPmdFH_seas2[2]), as.character(input$SCUPmdFH_2_bag), as.character(input$SCUPmdFH_2_len), 
                                             as.character(input$SCUPmdPR_seas2[1]), as.character(input$SCUPmdPR_seas2[2]), as.character(input$SCUPmdPR_2_bag), as.character(input$SCUPmdPR_2_len),
                                             as.character(input$SCUPmdSH_seas2[1]), as.character(input$SCUPmdSH_seas2[2]), as.character(input$SCUPmdSH_2_bag), as.character(input$SCUPmdSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfMDregs, bsbMDregs, scupMDregs)
    }
    
    if(any("VA" == input$state)){
      if(input$SF_VA_input_type == "All Modes Combined"){
        sfVAregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("VA"),
                               input =  c("SFva_seas1_op", "SFva_seas1_cl", "SFva_1_bag", "SFva_1_len", 
                                          "SFva_seas2_op", "SFva_seas2_cl", "SFva_2_bag", "SFva_2_len", 
                                          "SFvaFH_seas3_op", "SFvaFH_seas3_cl", "SFvaFH_3_bag", "SFvaFH_3_len", 
                                          "SFvaPR_seas3_op", "SFvaPR_seas3_cl", "SFvaPR_3_bag", "SFvaPR_3_len",
                                          "SFvaSH_seas3_op", "SFvaSH_seas3_cl", "SFvaSH_3_bag", "SFvaSH_3_len"),
                               value =  c(as.character(input$SFva_seas1[1]), as.character(input$SFva_seas1[2]), as.character(input$SFva_1_bag), as.character(input$SFva_1_len), 
                                          as.character(input$SFva_seas2[1]), as.character(input$SFva_seas2[2]), as.character(input$SFva_2_bag), as.character(input$SFva_2_len),
                                          as.character(input$SFvaFH_seas3[1]), as.character(input$SFvaFH_seas3[2]), as.character(input$SFvaFH_3_bag), as.character(input$SFvaFH_3_len), 
                                          as.character(input$SFvaPR_seas3[1]), as.character(input$SFvaPR_seas3[2]), as.character(input$SFvaPR_3_bag), as.character(input$SFvaPR_3_len),
                                          as.character(input$SFvaSH_seas3[1]), as.character(input$SFvaSH_seas3[2]), as.character(input$SFvaSH_3_bag), as.character(input$SFvaSH_3_len)))
      }else{
        sfVAregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("VA"),
                                input =  c("SFvaFH_seas1_op", "SFvaFH_seas1_cl", "SFvaFH_1_bag", "SFvaFH_1_len", 
                                           "SFvaPR_seas1_op", "SFvaPR_seas1_cl", "SFvaPR_1_bag", "SFvaPR_1_len",
                                           "SFvaSH_seas1_op", "SFvaSH_seas1_cl", "SFvaSH_1_bag", "SFvaSH_1_len",
                                           "SFvaFH_seas2_op", "SFvaFH_seas2_cl", "SFvaFH_2_bag", "SFvaFH_2_len", 
                                           "SFvaPR_seas2_op", "SFvaPR_seas2_cl", "SFvaPR_2_bag", "SFvaPR_2_len",
                                           "SFvaSH_seas2_op", "SFvaSH_seas2_cl", "SFvaSH_2_bag", "SFvaSH_2_len", 
                                           "SFvaFH_seas3_op", "SFvaFH_seas3_cl", "SFvaFH_3_bag", "SFvaFH_3_len", 
                                           "SFvaPR_seas3_op", "SFvaPR_seas3_cl", "SFvaPR_3_bag", "SFvaPR_3_len",
                                           "SFvaSH_seas3_op", "SFvaSH_seas3_cl", "SFvaSH_3_bag", "SFvaSH_3_len"),
                                value = c( as.character(input$SFvaFH_seas1[1]), as.character(input$SFvaFH_seas1[2]), as.character(input$SFvaFH_1_bag), as.character(input$SFvaFH_1_len), 
                                           as.character(input$SFvaPR_seas1[1]), as.character(input$SFvaPR_seas1[2]), as.character(input$SFvaPR_1_bag), as.character(input$SFvaPR_1_len),
                                           as.character(input$SFvaSH_seas1[1]), as.character(input$SFvaSH_seas1[2]), as.character(input$SFvaSH_1_bag), as.character(input$SFvaSH_1_len),
                                           as.character(input$SFvaFH_seas2[1]), as.character(input$SFvaFH_seas2[2]), as.character(input$SFvaFH_2_bag), as.character(input$SFvaFH_2_len), 
                                           as.character(input$SFvaPR_seas2[1]), as.character(input$SFvaPR_seas2[2]), as.character(input$SFvaPR_2_bag), as.character(input$SFvaPR_2_len),
                                           as.character(input$SFvaSH_seas2[1]), as.character(input$SFvaSH_seas2[2]), as.character(input$SFvaSH_2_bag), as.character(input$SFvaSH_2_len),
                                           as.character(input$SFvaFH_seas3[1]), as.character(input$SFvaFH_seas3[2]), as.character(input$SFvaFH_3_bag), as.character(input$SFvaFH_3_len), 
                                           as.character(input$SFvaPR_seas3[1]), as.character(input$SFvaPR_seas3[2]), as.character(input$SFvaPR_3_bag), as.character(input$SFvaPR_3_len),
                                           as.character(input$SFvaSH_seas3[1]), as.character(input$SFvaSH_seas3[2]), as.character(input$SFvaSH_3_bag), as.character(input$SFvaSH_3_len)))
      }
      
      
      if(input$BSB_VA_input_type == "All Modes Combined"){
        bsbVAregs <- data.frame(run_name = c(Run_Name()), 
                                state = c("VA"),
                                input =  c("BSBva_seas1_op", "BSBva_seas1_cl", "BSBva_1_bag", "BSBva_1_len", 
                                           "BSBva_seas2_op", "BSBva_seas2_cl", "BSBva_2_bag", "BSBva_2_len", 
                                           "BSBvaFH_seas3_op", "BSBvaFH_seas3_cl", "BSBvaFH_3_bag", "BSBvaFH_3_len", 
                                           "BSBvaPR_seas3_op", "BSBvaPR_seas3_cl", "BSBvaPR_3_bag", "BSBvaPR_3_len",
                                           "BSBvaSH_seas3_op", "BSBvaSH_seas3_cl", "BSBvaSH_3_bag", "BSBvaSH_3_len"),
                                value =  c(as.character(input$BSBva_seas1[1]), as.character(input$BSBva_seas1[2]), as.character(input$BSBva_1_bag), as.character(input$BSBva_1_len), 
                                           as.character(input$BSBva_seas2[1]), as.character(input$BSBva_seas2[2]), as.character(input$BSBva_2_bag), as.character(input$BSBva_2_len), 
                                           as.character(input$BSBvaFH_seas3[1]), as.character(input$BSBvaFH_seas3[2]), as.character(input$BSBvaFH_3_bag), as.character(input$BSBvaFH_3_len), 
                                           as.character(input$BSBvaPR_seas3[1]), as.character(input$BSBvaPR_seas3[2]), as.character(input$BSBvaPR_3_bag), as.character(input$BSBvaPR_3_len),
                                           as.character(input$BSBvaSH_seas3[1]), as.character(input$BSBvaSH_seas3[2]), as.character(input$BSBvaSH_3_bag), as.character(input$BSBvaSH_3_len)))
      }else{
        bsbVAregs <-  data.frame(run_name = c(Run_Name()), 
                                 state = c("VA"),
                                 input =  c( "BSBvaFH_seas1_op", "BSBvaFH_seas1_cl", "BSBvaFH_1_bag", "BSBvaFH_1_len",
                                             "BSBvaPR_seas1_op", "BSBvaPR_seas1_cl", "BSBvaPR_1_bag", "BSBvaPR_1_len",
                                             "BSBvaSH_seas1_op", "BSBvaSH_seas1_cl", "BSBvaSH_1_bag", "BSBvaSH_1_len",
                                             "BSBvaFH_seas2_op", "BSBvaFH_seas2_cl", "BSBvaFH_2_bag", "BSBvaFH_2_len", 
                                             "BSBvaPR_seas2_op", "BSBvaPR_seas2_cl", "BSBvaPR_2_bag", "BSBvaPR_2_len",
                                             "BSBvaSH_seas2_op", "BSBvaSH_seas2_cl", "BSBvaSH_2_bag", "BSBvaSH_2_len",
                                             "BSBvaFH_seas3_op", "BSBvaFH_seas3_cl", "BSBvaFH_3_bag", "BSBvaFH_3_len", 
                                             "BSBvaPR_seas3_op", "BSBvaPR_seas3_cl", "BSBvaPR_3_bag", "BSBvaPR_3_len",
                                             "BSBvaSH_seas3_op", "BSBvaSH_seas3_cl", "BSBvaSH_3_bag", "BSBvaSH_3_len"),
                                 value = c( as.character(input$BSBvaFH_seas1[1]), as.character(input$BSBvaFH_seas1[2]), as.character(input$BSBvaFH_1_bag), as.character(input$BSBvaFH_1_len),
                                            as.character(input$BSBvaPR_seas1[1]), as.character(input$BSBvaPR_seas1[2]), as.character(input$BSBvaPR_1_bag), as.character(input$BSBvaPR_1_len), 
                                            as.character(input$BSBvaSH_seas1[1]), as.character(input$BSBvaSH_seas1[2]), as.character(input$BSBvaSH_1_bag), as.character(input$BSBvaSH_1_len), 
                                            as.character(input$BSBvaFH_seas2[1]), as.character(input$BSBvaFH_seas2[2]), as.character(input$BSBvaFH_2_bag), as.character(input$BSBvaFH_2_len), 
                                            as.character(input$BSBvaPR_seas2[1]), as.character(input$BSBvaPR_seas2[2]), as.character(input$BSBvaPR_2_bag), as.character(input$BSBvaPR_2_len),
                                            as.character(input$BSBvaSH_seas2[1]), as.character(input$BSBvaSH_seas2[2]), as.character(input$BSBvaSH_2_bag), as.character(input$BSBvaSH_2_len), 
                                            as.character(input$BSBvaFH_seas3[1]), as.character(input$BSBvaFH_seas3[2]), as.character(input$BSBvaFH_3_bag), as.character(input$BSBvaFH_3_len), 
                                            as.character(input$BSBvaPR_seas3[1]), as.character(input$BSBvaPR_seas3[2]), as.character(input$BSBvaPR_3_bag), as.character(input$BSBvaPR_3_len),
                                            as.character(input$BSBvaSH_seas3[1]), as.character(input$BSBvaSH_seas3[2]), as.character(input$BSBvaSH_3_bag), as.character(input$BSBvaSH_3_len)))
      }
      
      
      if(input$SCUP_VA_input_type == "All Modes Combined"){
        scupVAregs <- data.frame(run_name = c(Run_Name()), 
                                 state = c("VA"),
                                 input =  c("SCUPva_seas1_op", "SCUPva_seas1_cl", "SCUPva_1_bag", "SCUPva_1_len", 
                                            "SCUPvaFH_seas2_op", "SCUPvaFH_seas2_cl", "SCUPvaFH_2_bag", "SCUPvaFH_2_len", 
                                            "SCUPvaPR_seas2_op", "SCUPvaPR_seas2_cl", "SCUPvaPR_2_bag", "SCUPvaPR_2_len",
                                            "SCUPvaSH_seas2_op", "SCUPvaSH_seas2_cl", "SCUPvaSH_2_bag", "SCUPvaSH_2_len"),
                                 value =  c(as.character(input$SCUPva_seas1[1]), as.character(input$SCUPva_seas1[2]), as.character(input$SCUPva_1_bag), as.character(input$SCUPva_1_len), 
                                            as.character(input$SCUPvaFH_seas2[1]), as.character(input$SCUPvaFH_seas2[2]), as.character(input$SCUPvaFH_2_bag), as.character(input$SCUPvaFH_2_len), 
                                            as.character(input$SCUPvaPR_seas2[1]), as.character(input$SCUPvaPR_seas2[2]), as.character(input$SCUPvaPR_2_bag), as.character(input$SCUPvaPR_2_len),
                                            as.character(input$SCUPvaSH_seas2[1]), as.character(input$SCUPvaSH_seas2[2]), as.character(input$SCUPvaSH_2_bag), as.character(input$SCUPvaSH_2_len)))
      }else{
        scupVAregs <-  data.frame(run_name = c(Run_Name()), 
                                  state = c("VA"),
                                  input =  c( "SCUPvaFH_seas1_op", "SCUPvaFH_seas1_cl", "SCUPvaFH_1_bag", "SCUPvaFH_1_len", 
                                              "SCUPvaPR_seas1_op", "SCUPvaPR_seas1_cl", "SCUPvaPR_1_bag", "SCUPvaPR_1_len",
                                              "SCUPvaSH_seas1_op", "SCUPvaSH_seas1_cl", "SCUPvaSH_1_bag", "SCUPvaSH_1_len",
                                              "SCUPvaFH_seas2_op", "SCUPvaFH_seas2_cl", "SCUPvaFH_2_bag", "SCUPvaFH_2_len", 
                                              "SCUPvaPR_seas2_op", "SCUPvaPR_seas2_cl", "SCUPvaPR_2_bag", "SCUPvaPR_2_len",
                                              "SCUPvaSH_seas2_op", "SCUPvaSH_seas2_cl", "SCUPvaSH_2_bag", "SCUPvaSH_2_len"),
                                  value =  c(as.character(input$SCUPvaFH_seas1[1]), as.character(input$SCUPvaFH_seas1[2]), as.character(input$SCUPvaFH_1_bag), as.character(input$SCUPvaFH_1_len), 
                                             as.character(input$SCUPvaPR_seas1[1]), as.character(input$SCUPvaPR_seas1[2]), as.character(input$SCUPvaPR_1_bag), as.character(input$SCUPvaPR_1_len),
                                             as.character(input$SCUPvaSH_seas1[1]), as.character(input$SCUPvaSH_seas1[2]), as.character(input$SCUPvaSH_1_bag), as.character(input$SCUPvaSH_1_len),
                                             as.character(input$SCUPvaFH_seas2[1]), as.character(input$SCUPvaFH_seas2[2]), as.character(input$SCUPvaFH_2_bag), as.character(input$SCUPvaFH_2_len), 
                                             as.character(input$SCUPvaPR_seas2[1]), as.character(input$SCUPvaPR_seas2[2]), as.character(input$SCUPvaPR_2_bag), as.character(input$SCUPvaPR_2_len),
                                             as.character(input$SCUPvaSH_seas2[1]), as.character(input$SCUPvaSH_seas2[2]), as.character(input$SCUPvaSH_2_bag), as.character(input$SCUPvaSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfVAregs, bsbVAregs, scupVAregs)
    }
    
    if(any("NC" == input$state)){
      if(input$SF_NC_input_type == "All Modes Combined"){
        sfNCregs <- data.frame(run_name = c(Run_Name()), 
                               state = c("NC"),
                               input =  c("SFnc_seas1_op", "SFnc_seas1_cl", "SFnc_1_bag", "SFnc_1_len",  
                                          "SFncFH_seas2_op", "SFncFH_seas2_cl", "SFncFH_2_bag", "SFncFH_2_len", 
                                          "SFncPR_seas2_op", "SFncPR_seas2_cl", "SFncPR_2_bag", "SFncPR_2_len",
                                          "SFncSH_seas2_op", "SFncSH_seas2_cl", "SFncSH_2_bag", "SFncSH_2_len"),
                               value =  c(as.character(input$SFnc_seas1[1]), as.character(input$SFnc_seas1[2]), as.character(input$SFnc_1_bag), as.character(input$SFnc_1_len), 
                                          as.character(input$SFncFH_seas2[1]), as.character(input$SFncFH_seas2[2]), as.character(input$SFncFH_2_bag), as.character(input$SFncFH_2_len), 
                                          as.character(input$SFncPR_seas2[1]), as.character(input$SFncPR_seas2[2]), as.character(input$SFncPR_2_bag), as.character(input$SFncPR_2_len),
                                          as.character(input$SFncSH_seas2[1]), as.character(input$SFncSH_seas2[2]), as.character(input$SFncSH_2_bag), as.character(input$SFncSH_2_len)))
      }else{
        sfNCregs <-  data.frame(run_name = c(Run_Name()), 
                                state = c("NC"),
                                input =  c("SFncFH_seas1_op", "SFncFH_seas1_cl", "SFncFH_1_bag", "SFncFH_1_len", 
                                           "SFncPR_seas1_op", "SFncPR_seas1_cl", "SFncPR_1_bag", "SFncPR_1_len",
                                           "SFncSH_seas1_op", "SFncSH_seas1_cl", "SFncSH_1_bag", "SFncSH_1_len",
                                           "SFncFH_seas2_op", "SFncFH_seas2_cl", "SFncFH_2_bag", "SFncFH_2_len", 
                                           "SFncPR_seas2_op", "SFncPR_seas2_cl", "SFncPR_2_bag", "SFncPR_2_len",
                                           "SFncSH_seas2_op", "SFncSH_seas2_cl", "SFncSH_2_bag", "SFncSH_2_len"),
                                value = c( as.character(input$SFncFH_seas1[1]), as.character(input$SFncFH_seas1[2]), as.character(input$SFncFH_1_bag), as.character(input$SFncFH_1_len), 
                                           as.character(input$SFncPR_seas1[1]), as.character(input$SFncPR_seas1[2]), as.character(input$SFncPR_1_bag), as.character(input$SFncPR_1_len),
                                           as.character(input$SFncSH_seas1[1]), as.character(input$SFncSH_seas1[2]), as.character(input$SFncSH_1_bag), as.character(input$SFncSH_1_len),
                                           as.character(input$SFncFH_seas2[1]), as.character(input$SFncFH_seas2[2]), as.character(input$SFncFH_2_bag), as.character(input$SFncFH_2_len), 
                                           as.character(input$SFncPR_seas2[1]), as.character(input$SFncPR_seas2[2]), as.character(input$SFncPR_2_bag), as.character(input$SFncPR_2_len),
                                           as.character(input$SFncSH_seas2[1]), as.character(input$SFncSH_seas2[2]), as.character(input$SFncSH_2_bag), as.character(input$SFncSH_2_len)))
      }
      
      
      if(input$BSB_NC_input_type == "All Modes Combined"){
        bsbNCregs <- data.frame(run_name = c(Run_Name()),
                              state = c("NC"),
                              input =  c("BSBnc_seas1_op", "BSBnc_seas1_cl", "BSBnc_1_bag", "BSBnc_1_len", 
                                         "BSBnc_seas2_op", "BSBnc_seas2_cl", "BSBnc_2_bag", "BSBnc_2_len", 
                                         "BSBncFH_seas3_op", "BSBncFH_seas3_cl", "BSBncFH_3_bag", "BSBncFH_3_len", 
                                         "BSBncPR_seas3_op", "BSBncPR_seas3_cl", "BSBncPR_3_bag", "BSBncPR_3_len",
                                         "BSBncSH_seas3_op", "BSBncSH_seas3_cl", "BSBncSH_3_bag", "BSBncSH_3_len"),
                              value =  c(as.character(input$BSBnc_seas1[1]), as.character(input$BSBnc_seas1[2]), as.character(input$BSBnc_1_bag), as.character(input$BSBnc_1_len), 
                                         as.character(input$BSBnc_seas2[1]), as.character(input$BSBnc_seas2[2]), as.character(input$BSBnc_2_bag), as.character(input$BSBnc_2_len), 
                                         as.character(input$BSBncFH_seas3[1]), as.character(input$BSBncFH_seas3[2]), as.character(input$BSBncFH_3_bag), as.character(input$BSBncFH_3_len), 
                                         as.character(input$BSBncPR_seas3[1]), as.character(input$BSBncPR_seas3[2]), as.character(input$BSBncPR_3_bag), as.character(input$BSBncPR_3_len),
                                         as.character(input$BSBncSH_seas3[1]), as.character(input$BSBncSH_seas3[2]), as.character(input$BSBncSH_3_bag), as.character(input$BSBncSH_3_len)))
      }else{
        bsbNCregs <-  data.frame(run_name = c(Run_Name()), 
                                 state = c("NC"),
                                 input =  c( "BSBncFH_seas1_op", "BSBncFH_seas1_cl", "BSBncFH_1_bag", "BSBncFH_1_len",
                                             "BSBncPR_seas1_op", "BSBncPR_seas1_cl", "BSBncPR_1_bag", "BSBncPR_1_len",
                                             "BSBncSH_seas1_op", "BSBncSH_seas1_cl", "BSBncSH_1_bag", "BSBncSH_1_len",
                                             "BSBncFH_seas2_op", "BSBncFH_seas2_cl", "BSBncFH_2_bag", "BSBncFH_2_len", 
                                             "BSBncPR_seas2_op", "BSBncPR_seas2_cl", "BSBncPR_2_bag", "BSBncPR_2_len",
                                             "BSBncSH_seas2_op", "BSBncSH_seas2_cl", "BSBncSH_2_bag", "BSBncSH_2_len",
                                             "BSBncFH_seas3_op", "BSBncFH_seas3_cl", "BSBncFH_3_bag", "BSBncFH_3_len", 
                                             "BSBncPR_seas3_op", "BSBncPR_seas3_cl", "BSBncPR_3_bag", "BSBncPR_3_len",
                                             "BSBncSH_seas3_op", "BSBncSH_seas3_cl", "BSBncSH_3_bag", "BSBncSH_3_len"),
                                 value = c( as.character(input$BSBncFH_seas1[1]), as.character(input$BSBncFH_seas1[2]), as.character(input$BSBncFH_1_bag), as.character(input$BSBncFH_1_len),
                                            as.character(input$BSBncPR_seas1[1]), as.character(input$BSBncPR_seas1[2]), as.character(input$BSBncPR_1_bag), as.character(input$BSBncPR_1_len), 
                                            as.character(input$BSBncSH_seas1[1]), as.character(input$BSBncSH_seas1[2]), as.character(input$BSBncSH_1_bag), as.character(input$BSBncSH_1_len), 
                                            as.character(input$BSBncFH_seas2[1]), as.character(input$BSBncFH_seas2[2]), as.character(input$BSBncFH_2_bag), as.character(input$BSBncFH_2_len), 
                                            as.character(input$BSBncPR_seas2[1]), as.character(input$BSBncPR_seas2[2]), as.character(input$BSBncPR_2_bag), as.character(input$BSBncPR_2_len),
                                            as.character(input$BSBncSH_seas2[1]), as.character(input$BSBncSH_seas2[2]), as.character(input$BSBncSH_2_bag), as.character(input$BSBncSH_2_len), 
                                            as.character(input$BSBncFH_seas3[1]), as.character(input$BSBncFH_seas3[2]), as.character(input$BSBncFH_3_bag), as.character(input$BSBncFH_3_len), 
                                            as.character(input$BSBncPR_seas3[1]), as.character(input$BSBncPR_seas3[2]), as.character(input$BSBncPR_3_bag), as.character(input$BSBncPR_3_len),
                                            as.character(input$BSBncSH_seas3[1]), as.character(input$BSBncSH_seas3[2]), as.character(input$BSBncSH_3_bag), as.character(input$BSBncSH_3_len)))
      }
      
      
      if(input$SCUP_NC_input_type == "All Modes Combined"){
        scupNCregs <- data.frame(run_name = c(Run_Name()), 
                                 state = c("NC"),
                                 input =  c("SCUPnc_seas1_op", "SCUPnc_seas1_cl", "SCUPnc_1_bag", "SCUPnc_1_len", 
                                            "SCUPncFH_seas2_op", "SCUPncFH_seas2_cl", "SCUPncFH_2_bag", "SCUPncFH_2_len", 
                                            "SCUPncPR_seas2_op", "SCUPncPR_seas2_cl", "SCUPncPR_2_bag", "SCUPncPR_2_len",
                                            "SCUPncSH_seas2_op", "SCUPncSH_seas2_cl", "SCUPncSH_2_bag", "SCUPncSH_2_len"),
                                 value =  c(as.character(input$SCUPnc_seas1[1]), as.character(input$SCUPnc_seas1[2]), as.character(input$SCUPnc_1_bag), as.character(input$SCUPnc_1_len), 
                                            as.character(input$SCUPncFH_seas2[1]), as.character(input$SCUPncFH_seas2[2]), as.character(input$SCUPncFH_2_bag), as.character(input$SCUPncFH_2_len), 
                                            as.character(input$SCUPncPR_seas2[1]), as.character(input$SCUPncPR_seas2[2]), as.character(input$SCUPncPR_2_bag), as.character(input$SCUPncPR_2_len),
                                            as.character(input$SCUPncSH_seas2[1]), as.character(input$SCUPncSH_seas2[2]), as.character(input$SCUPncSH_2_bag), as.character(input$SCUPncSH_2_len)))
      }else{
        scupNCregs <-  data.frame(run_name = c(Run_Name()), 
                                  state = c("NC"),
                                  input =  c( "SCUPncFH_seas1_op", "SCUPncFH_seas1_cl", "SCUPncFH_1_bag", "SCUPncFH_1_len", 
                                              "SCUPncPR_seas1_op", "SCUPncPR_seas1_cl", "SCUPncPR_1_bag", "SCUPncPR_1_len",
                                              "SCUPncSH_seas1_op", "SCUPncSH_seas1_cl", "SCUPncSH_1_bag", "SCUPncSH_1_len",
                                              "SCUPncFH_seas2_op", "SCUPncFH_seas2_cl", "SCUPncFH_2_bag", "SCUPncFH_2_len", 
                                              "SCUPncPR_seas2_op", "SCUPncPR_seas2_cl", "SCUPncPR_2_bag", "SCUPncPR_2_len",
                                              "SCUPncSH_seas2_op", "SCUPncSH_seas2_cl", "SCUPncSH_2_bag", "SCUPncSH_2_len"),
                                  value =  c(as.character(input$SCUPncFH_seas1[1]), as.character(input$SCUPncFH_seas1[2]), as.character(input$SCUPncFH_1_bag), as.character(input$SCUPncFH_1_len), 
                                             as.character(input$SCUPncPR_seas1[1]), as.character(input$SCUPncPR_seas1[2]), as.character(input$SCUPncPR_1_bag), as.character(input$SCUPncPR_1_len),
                                             as.character(input$SCUPncSH_seas1[1]), as.character(input$SCUPncSH_seas1[2]), as.character(input$SCUPncSH_1_bag), as.character(input$SCUPncSH_1_len),
                                             as.character(input$SCUPncFH_seas2[1]), as.character(input$SCUPncFH_seas2[2]), as.character(input$SCUPncFH_2_bag), as.character(input$SCUPncFH_2_len), 
                                             as.character(input$SCUPncPR_seas2[1]), as.character(input$SCUPncPR_seas2[2]), as.character(input$SCUPncPR_2_bag), as.character(input$SCUPncPR_2_len),
                                             as.character(input$SCUPncSH_seas2[1]), as.character(input$SCUPncSH_seas2[2]), as.character(input$SCUPncSH_2_bag), as.character(input$SCUPncSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfNCregs, bsbNCregs, scupNCregs)
      
    }
    
    
    readr::write_csv(regulations, file = here::here(paste0("saved_regs/regs_", input$Run_Name, ".csv")))
    print("saved_inputs")
    
    enqueue_simple_sas(input$Run_Name)
    
    return(regulations)
    
  })
  
  observeEvent(input$runmeplease, {
    output$message <- renderText("Regulations saved - we will run these soon be sure to change run name before clicking again.")
  })
  
  # Get list of files from the folder
  available_files <- reactive({
    folder_path <- here::here("output/")
    if (dir.exists(folder_path)) {
      files <- list.files(folder_path, full.names = FALSE)
      if (length(files) > 0) {
        return(files)
      }
    }
    return(character(0))
  })
  
  file_mapping <- reactive({
    files <- available_files()
    if (length(files) > 0) {
      # Remove file extensions for display names
      display_names <- files %>% 
        stringr::str_remove("^output_") %>%         # remove prefix
        stringr::str_remove("_[0-9]+")  %>% 
        stringr::str_remove("_[0-9]+") %>% 
        stringr::str_remove(".csv") 
      # Create named vector: display_name = full_filename
      names(files) <- display_names
      return(files)
    }
    return(character(0))
  })
  
  # Update dropdown choices when app starts
  observe({
    file_map <- file_mapping()
    if (length(file_map) > 0) {
      updateSelectInput(
        session,
        "file_choice",
        choices = file_map,
        selected = file_map[1]
      )
    } else {
      updateSelectInput(
        session,
        "file_choice",
        choices = "No files available",
        selected = NULL
      )
    }
  })
  
  # Display file information
  output$file_info <- renderText({
    if (is.null(input$file_choice) || input$file_choice == "No files available") {
      return("No file selected or no files available.")
    }
    
    file_path <- file.path("output", input$file_choice)
    
    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
      # Get the display name (without extension) for the selected file
      display_name <- tools::file_path_sans_ext(input$file_choice)
      paste(
        "Display name:", display_name,
        "\nFull filename:", input$file_choice,
        "\nFile size:", round(file_info$size / 1024, 2), "KB",
        "\nLast modified:", format(file_info$mtime, "%Y-%m-%d %H:%M:%S"),
        sep = "\n"
      )
    } else {
      "File not found."
    }
  })
  
  # Handle file download
  output$download_file <- downloadHandler(
    filename = function() {
      # Return the selected filename (full filename with extension)
      if (!is.null(input$file_choice) && input$file_choice != "No files available") {
        return(input$file_choice)
      } else {
        return("file.txt")  # Fallback filename
      }
    },
    content = function(file) {
      # Copy the selected file to the download location
      if (!is.null(input$file_choice) && input$file_choice != "No files available") {
        file_path <- file.path("output", input$file_choice)
        if (file.exists(file_path)) {
          file.copy(file_path, file)
        } else {
          # If file doesn't exist, create an error file
          writeLines("Error: File not found.", file)
        }
      } else {
        writeLines("Error: No file selected.", file)
      }
    }
  )
  
  
  
}

shiny::shinyApp(ui = ui, server = server)
