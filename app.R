# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)

### file_df computed once at startup (static, session-independent)
files <- list.files("output", pattern = "\\.csv$", full.names = FALSE)

file_df <- data.frame(
  file = files,
  state = stringr::str_extract(files, "(?<=output_)[A-Z]{2}"),
  display = files |>
    stringr::str_remove("^output_") |>
    stringr::str_remove("_[0-9]{8}_[0-9]{6}\\.csv$")
)

states <- c("MA","RI","CT","NY","NJ","DE","MD","VA","NC")

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Recreational Fisheries Decision Support Tool for Summer Flounder, Scup, and Black Sea Bass"),
  tabsetPanel(
    tabPanel("Summary Page",
            "This page summarizes results of previous model runs. It takes about 60 seconds to initialize the first time that you use the app.",
             plotly::plotlyOutput(outputId = "summary_rhl_fig"),
             shiny::h2("Summary Table"), 
             DT::DTOutput(outputId = "summary_percdiff_table"),
             
             ### Figure and table output by state
             tabsetPanel(
               tabPanel("MA", 
                        shiny::h2("Massachusetts"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "ma_rhl_fig"),
                        plotly::plotlyOutput(outputId = "ma_CV_fig"),
                        plotly::plotlyOutput(outputId = "ma_discards_fig"),
                        plotly::plotlyOutput(outputId = "ma_totmort_fig"),
                        plotly::plotlyOutput(outputId = "ma_trips_fig")
               ),
               tabPanel("RI", 
                        shiny::h2("Rhode Island"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "ri_rhl_fig"),
                        plotly::plotlyOutput(outputId = "ri_CV_fig"),
                        plotly::plotlyOutput(outputId = "ri_discards_fig"),
                        plotly::plotlyOutput(outputId = "ri_totmort_fig"),
                        plotly::plotlyOutput(outputId = "ri_trips_fig")
               ), 
               tabPanel("CT", 
                        shiny::h2("Connecticut"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "ct_rhl_fig"),
                        plotly::plotlyOutput(outputId = "ct_CV_fig"),
                        plotly::plotlyOutput(outputId = "ct_discards_fig"),
                        plotly::plotlyOutput(outputId = "ct_totmort_fig"),
                        plotly::plotlyOutput(outputId = "ct_trips_fig")
               ),
               tabPanel("NY", 
                        shiny::h2("New York"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "ny_rhl_fig"),
                        plotly::plotlyOutput(outputId = "ny_CV_fig"),
                        plotly::plotlyOutput(outputId = "ny_discards_fig"),
                        plotly::plotlyOutput(outputId = "ny_totmort_fig"),
                        plotly::plotlyOutput(outputId = "ny_trips_fig")
               ),
               tabPanel("NJ", 
                        shiny::h2("New Jersey"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "nj_rhl_fig"),
                        plotly::plotlyOutput(outputId = "nj_CV_fig"),
                        plotly::plotlyOutput(outputId = "nj_discards_fig"),
                        plotly::plotlyOutput(outputId = "nj_totmort_fig"),
                        plotly::plotlyOutput(outputId = "nj_trips_fig")
               ),
               tabPanel("DE", 
                        shiny::h2("Delaware"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "de_rhl_fig"),
                        plotly::plotlyOutput(outputId = "de_CV_fig"),
                        plotly::plotlyOutput(outputId = "de_discards_fig"),
                        plotly::plotlyOutput(outputId = "de_totmort_fig"),
                        plotly::plotlyOutput(outputId = "de_trips_fig")
               ),
               tabPanel("MD", 
                        shiny::h2("Marlyand"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "md_rhl_fig"),
                        plotly::plotlyOutput(outputId = "md_CV_fig"),
                        plotly::plotlyOutput(outputId = "md_discards_fig"),
                        plotly::plotlyOutput(outputId = "md_totmort_fig"),
                        plotly::plotlyOutput(outputId = "md_trips_fig")
               ),
               tabPanel("VA", 
                        shiny::h2("Virginia"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "va_rhl_fig"),
                        plotly::plotlyOutput(outputId = "va_CV_fig"),
                        plotly::plotlyOutput(outputId = "va_discards_fig"),
                        plotly::plotlyOutput(outputId = "va_totmort_fig"),
                        plotly::plotlyOutput(outputId = "va_trips_fig")
               ),
               tabPanel("NC", 
                        shiny::h2("North Carolina"),
                        p("This may take a moment to load. Thank you for your patience"),
                        plotly::plotlyOutput(outputId = "nc_rhl_fig"),
                        plotly::plotlyOutput(outputId = "nc_CV_fig"),
                        plotly::plotlyOutput(outputId = "nc_discards_fig"),
                        plotly::plotlyOutput(outputId = "nc_totmort_fig"),
                        plotly::plotlyOutput(outputId = "nc_trips_fig")
               ), 
               tabPanel("Regulations", 
                        shiny::h2("Regulations"),
                        selectInput( "file_choice",
                                     "Choose a file to download:",
                                     choices = NULL,
                                     selected = NULL ),
                        downloadButton( "download_file",
                                        "Download Selected File",
                                        class = "btn-primary"),
                        DT::DTOutput(outputId = "summary_regs_table"))
               
             )),
    
    
    tabPanel( "Regulation Selection",
              strong(div("INSTRUCTIONS: (1) Give your policy a name, (2) Select one or more states,  (3) Select regulations, (4) Click Run Me", style = "color:blue")),
              textInput("Run_Name", "Please give your policy a unique name using your initials and a number (ex. AB1)."),
              
              shinyWidgets::awesomeCheckboxGroup(
                inputId = "state", 
                label = "State", 
                choices = c("MA", "RI", "CT", "NY", "NJ", "DE",  "MD", "VA", "NC"),
                inline = TRUE,
                status = "danger"),
              
              actionButton("runmeplease", "Run Me"), 
              
              textOutput("message"),
              uiOutput("addMA"),
              uiOutput("addRI"),
              uiOutput("addCT"), 
              uiOutput("addNY"),
              uiOutput("addNJ"), 
              uiOutput("addDE"),
              uiOutput("addMD"),
              uiOutput("addVA"), 
              uiOutput("addNC")),
  
  tabPanel( "Results",
            fluidRow(
              column(
                width = 3,
                lapply(states, function(st){
                  selectInput(
                    inputId = paste0("policy_", st),
                    label = paste("Select Policy -", st),
                    choices = NULL
                  )
                }),
                actionButton(
                  "calculate",
                  "Calculate",
                  class = "btn-primary"
                )
          ),
          
          column(
            width = 9,
            tableOutput("coastwide_keep"),
            fluidRow(
              column(width = 4, tableOutput("coastwide_cv")),
              column(width = 4, tableOutput("coastwide_trips"))
            ),
            tableOutput("coastwide_discards")
          )
  ))
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
  
  # ---------------------------------------------------------------------------
  # Cache all_data as a single reactive so it is computed ONCE per
  # session and shared across all outputs instead of re-reading every CSV on
  # every render.
  # ---------------------------------------------------------------------------
  all_data <- reactive({
    flist <- list.files(path = here::here("output/"), pattern = "\\.csv$", full.names = TRUE)
    
    read_cols      <- c("metric","species","value","mode","state","draw","model")
    read_cols_types <- c("c","c","d","c","c","i","c")
    
    flist %>%
      magrittr::set_names(flist) %>%
      purrr::map_dfr(readr::read_csv, .id = "filename",
                     col_select = all_of(read_cols),
                     col_types  = read_cols_types) %>%
      dplyr::mutate(
        filename = stringr::str_extract(filename, "(?<=output_).+?(?=_202)"),
        model    = dplyr::case_when(model == "Lou_SQ" ~ "SQ", TRUE ~ model),
        metric   = dplyr::case_when(model == "SQ" & metric == "change_CS"   ~ "CV",            TRUE ~ metric),
        metric   = dplyr::case_when(model == "SQ" & metric == "n_trips_alt" ~ "predicted_trips", TRUE ~ metric)
      )
  })

  # Cache regs data 
  regs_data <- reactive({
    flist <- list.files(path = here::here("saved_regs/"), pattern = "\\.csv$", full.names = TRUE)
    flist %>% purrr::map_dfr(readr::read_csv)
  })

  # Results tab
  ###########################################################
  lapply(states, function(st){
    state_files <- file_df |> filter(state == st)
    choices <- c(
      "No file selected" = "",
      "No file selected " = "none",
      setNames(state_files$file, state_files$display)
    )
    updateSelectInput(session, paste0("policy_", st), choices = choices, selected = "")
  })
  
  selected_files <- eventReactive(input$calculate, {
    selected_files <- sapply(states, function(st){ input[[paste0("policy_", st)]] })
    selected_files[selected_files != "" & selected_files != "none"]
  })
  
  combined_data <- eventReactive(input$calculate, {
    files <- selected_files()
    files |>
      lapply(function(f){
        read.csv(file.path("output", f)) %>%
          dplyr::mutate(
            model  = if_else(model == "Lou_SQ", "SQ", model),
            metric = case_when(
              metric == "change_CS"   ~ "CV",
              metric == "n_trips_alt" ~ "predicted_trips",
              TRUE ~ metric
            )
          )
      }) |>
      dplyr::bind_rows() 
  })
  
  sq_data <- eventReactive(input$calculate, {
    selected_states <- states[sapply(states, function(st){
      input[[paste0("policy_", st)]] != "" & input[[paste0("policy_", st)]] != "none"
    })]
    
    sq_files <- list.files("output", pattern = "SQ.*\\.csv$", full.names = TRUE)
    sq_df <- data.frame(file = sq_files) %>%
      mutate(
        filename = basename(file),
        state    = stringr::str_extract(filename, "(?<=output_)[A-Z]{2}")
      ) %>%
      filter(state %in% selected_states)
    
    read_cols       <- c("metric","species","value","mode","state","draw","model")
    read_cols_types <- c("c","c","d","c","c","i","c")
    
    bind_rows(
      lapply(sq_df$file, function(f) {
        readr::read_csv(f, col_select = all_of(read_cols),
                        col_types = read_cols_types, show_col_types = FALSE) %>%
          dplyr::mutate(
            model  = if_else(model == "Lou_SQ", "SQ", model),
            metric = case_when(
              metric == "change_CS"   ~ "CV",
              metric == "n_trips_alt" ~ "predicted_trips",
              TRUE ~ metric
            )
          )
      })
    )
  })

  # Shared draw-filter helper used by all coastwide reactives
  filter_draws <- function(df) {
    df %>%
      filter(case_when(
        state %in% c("MA","CT","NY","NJ","DE","VA","NC") & draw %in% c(20,21,78) ~ FALSE,
        state == "MD" & model != "SQ" & draw %in% c(20,21)       ~ FALSE,
        state == "MD" & model == "SQ" & draw %in% c(20,21,78)    ~ FALSE,
        state == "RI" & model != "SQ" & draw %in% c(76)          ~ FALSE,
        state == "RI" & model == "SQ" & draw %in% c(20,21,78)    ~ FALSE,
        TRUE ~ TRUE
      ))
  }

  coastwide_keep <- reactive({
    req(combined_data(), sq_data())
    
    policy_draws <- combined_data() %>%
      filter_draws() %>%
      dplyr::filter(metric == "keep_weight") %>%
      group_by(draw, species, mode) %>%
      summarise(policy_total = sum(value), .groups = "drop") 
    
    sq_draws <- sq_data() %>%
      filter_draws() %>%
      dplyr::filter(metric == "keep_weight") %>%
      group_by(draw, species, mode) %>%
      summarise(sq_total = sum(value), .groups = "drop")  
    
    policy_draws %>%
      left_join(sq_draws, by = c("draw","species","mode")) %>%
      mutate(pct_change_draw = if_else(sq_total == 0, 0,
                                       (policy_total - sq_total) / sq_total * 100)) %>%
      group_by(species, mode) %>%
      summarise(
        `Median Harvest Weight (lbs)` = format(round(median(policy_total, na.rm = TRUE), 0), big.mark=","),
        `Percent Change from SQ`      = sprintf("%.2f%%", median(pct_change_draw, na.rm = TRUE)),
        .groups = "drop"
      )
  })
    
  coastwide_cv <- reactive({
    req(combined_data())
    
    combined_data() %>%
      filter_draws() %>%
      dplyr::filter(metric == "CV") %>%
      group_by(draw, mode) %>%
      summarise(cv_total = sum(value), .groups = "drop") %>%
      group_by(mode) %>%
      summarise(
        `Angler Satisfaction ($)` = format(round(median(cv_total, na.rm = TRUE), 0), big.mark=","),
        .groups = "drop"
      )
  })
  
  coastwide_discards <- reactive({
    req(combined_data(), sq_data())
    
    combined_data() %>%
      filter_draws() %>%
      dplyr::filter(metric %in% c("release_weight","discmort_weight")) %>%
      group_by(draw, species, mode, metric) %>%
      summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      group_by(species, mode, metric) %>%
      summarise(median_value = median(total_value, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = metric, values_from = median_value,
                         names_glue = "{metric}_median") %>%
      dplyr::rename(
        `Median Discard weight (lbs)`      = release_weight_median,
        `Median Dead discard weight (lbs)` = discmort_weight_median
      ) %>%
      dplyr::arrange(species, mode) %>%
      group_by(species, mode) %>%
      summarise(
        `Median Discard weight (lbs)`      = format(round(median(`Median Discard weight (lbs)`,      na.rm=TRUE), 0), big.mark=","),
        `Median Dead discard weight (lbs)` = format(round(median(`Median Dead discard weight (lbs)`, na.rm=TRUE), 0), big.mark=","),
        .groups = "drop"
      )
  })
  
  coastwide_trips <- reactive({
    req(combined_data())
    
    combined_data() %>%
      filter_draws() %>%
      dplyr::filter(metric == "predicted_trips") %>%
      group_by(draw, mode) %>%
      summarise(trips_total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      group_by(mode) %>%
      summarise(
        `Predicted trips` = format(round(median(trips_total, na.rm = TRUE), 0), big.mark=","),
        .groups = "drop"
      )
  })
  
  output$coastwide_keep     <- renderTable({ coastwide_keep() })
  output$coastwide_cv       <- renderTable({ coastwide_cv() })
  output$coastwide_discards <- renderTable({ coastwide_discards() })
  output$coastwide_trips    <- renderTable({ coastwide_trips() })

  #####################################################################
  
  Run_Name <- function(){
    if(stringr::str_detect(input$Run_Name, "_")){
      Run_Name <- gsub("_", "-", input$Run_Name)
    }else{
      Run_Name <- input$Run_Name
    }
    print(Run_Name)
    return(Run_Name)
  }
  
  #### Toggle extra seasons on UI ####
  shinyjs::onclick("SFMAaddSeason",  shinyjs::toggle(id = "SFmaSeason2",  anim = TRUE))
  shinyjs::onclick("BSBMAaddSeason", shinyjs::toggle(id = "BSBmaSeason2", anim = TRUE))
  shinyjs::onclick("SCUPMAaddSeason",shinyjs::toggle(id = "SCUPmaSeason2",anim = TRUE))
  shinyjs::onclick("SFRIaddSeason",  shinyjs::toggle(id = "SFriSeason2",  anim = TRUE))
  shinyjs::onclick("BSBRIaddSeason", shinyjs::toggle(id = "BSBriSeason3", anim = TRUE))
  shinyjs::onclick("SCUPRIaddSeason",shinyjs::toggle(id = "SCUPriSeason2",anim = TRUE))
  shinyjs::onclick("SFCTaddSeason",  shinyjs::toggle(id = "SFctSeason3",  anim = TRUE))
  shinyjs::onclick("BSBCTaddSeason", shinyjs::toggle(id = "BSBctSeason3", anim = TRUE))
  shinyjs::onclick("SCUPCTaddSeason",shinyjs::toggle(id = "SCUPctSeason2",anim = TRUE))
  shinyjs::onclick("SFNYaddSeason",  shinyjs::toggle(id = "SFnySeason3",  anim = TRUE))
  shinyjs::onclick("BSBNYaddSeason", shinyjs::toggle(id = "BSBnySeason3", anim = TRUE))
  shinyjs::onclick("SCUPNYaddSeason",shinyjs::toggle(id = "SCUPnySeason2",anim = TRUE))
  shinyjs::onclick("SFNJaddSeason",  shinyjs::toggle(id = "SFnjSeason2",  anim = TRUE))
  shinyjs::onclick("BSBNJaddSeason", shinyjs::toggle(id = "BSBnjSeason5", anim = TRUE))
  shinyjs::onclick("SCUPNJaddSeason",shinyjs::toggle(id = "SCUPnjSeason3",anim = TRUE))
  shinyjs::onclick("SFDEaddSeason",  shinyjs::toggle(id = "SFdeSeason3",  anim = TRUE))
  shinyjs::onclick("BSBDEaddSeason", shinyjs::toggle(id = "BSBdeSeason3", anim = TRUE))
  shinyjs::onclick("SCUPDEaddSeason",shinyjs::toggle(id = "SCUPdeSeason2",anim = TRUE))
  shinyjs::onclick("SFMDaddSeason",  shinyjs::toggle(id = "SFmdSeason3",  anim = TRUE))
  shinyjs::onclick("BSBMDaddSeason", shinyjs::toggle(id = "BSBmdSeason3", anim = TRUE))
  shinyjs::onclick("SCUPMDaddSeason",shinyjs::toggle(id = "SCUPmdSeason2",anim = TRUE))
  shinyjs::onclick("SFVAaddSeason",  shinyjs::toggle(id = "SFvaSeason3",  anim = TRUE))
  shinyjs::onclick("BSBVAaddSeason", shinyjs::toggle(id = "BSBvaSeason3", anim = TRUE))
  shinyjs::onclick("SCUPVAaddSeason",shinyjs::toggle(id = "SCUPvaSeason2",anim = TRUE))
  shinyjs::onclick("SFNCaddSeason",  shinyjs::toggle(id = "SFncSeason2",  anim = TRUE))
  shinyjs::onclick("BSBNCaddSeason", shinyjs::toggle(id = "BSBncSeason3", anim = TRUE))
  shinyjs::onclick("SCUPNCaddSeason",shinyjs::toggle(id = "SCUPncSeason2",anim = TRUE))
  
  #### Output$addSTATE ####
  
  ############## MASSACHUSETTS ###########################################################
  output$addMA <- renderUI({
    if(any("MA" == input$state)){
      fluidRow( 
        style = "background-color: #FBB4AE;",
        column(4,
               titlePanel("Summer Flounder - MA"),
               dateRangeInput(inputId= "SFmaFH_seas1", label ="For Hire Season 1",
                              start = as.Date("2027-05-24"),
                              end   = as.Date("2027-09-23"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SFmaFH_1_bag", label = "Bag Limit", min = 0, max = 100, value = 5)),
                 column(5, sliderInput(inputId= "SFmaFH_1_len", label = "Min Length", min = 14, max = 21, value = 17.5, step = .5))),
               dateRangeInput(inputId= "SFmaPR_seas1", label ="Private Season 1",
                              start = as.Date("2027-05-24"),
                              end   = as.Date("2027-09-23"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SFmaPR_1_bag", label = "Bag Limit", min = 0, max = 100, value = 5)),
                 column(5, sliderInput(inputId= "SFmaPR_1_len", label = "Min Length", min = 14, max = 21, value = 17.5, step = .5))),
               dateRangeInput(inputId= "SFmaSH_seas1", label ="Shore Season 1",
                              start = as.Date("2027-05-24"),
                              end   = as.Date("2027-09-23"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SFmaSH_1_bag", label = "Bag Limit", min = 0, max = 100, value = 5)),
                 column(5, sliderInput(inputId= "SFmaSH_1_len", label = "Min Length", min = 14, max = 21, value = 16.5, step = .5))),
               
               actionButton("SFMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmaSeason2",
                                    dateRangeInput(inputId= "SFmaFH_seas2", label ="For Hire Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SFmaFH_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SFmaFH_2_len", label ="Min Length", min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFmaPR_seas2", label ="Private Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SFmaPR_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SFmaPR_2_len", label ="Min Length", min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFmaSH_seas2", label ="Shore Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SFmaSH_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SFmaSH_2_len", label ="Min Length", min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MA"),
               selectInput("BSB_MA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBmaMode"),
               actionButton("BSBMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmaSeason2",
                                    dateRangeInput(inputId= "BSBmaFH_seas2", label ="For Hire Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "BSBmaFH_2_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "BSBmaFH_2_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
                                    dateRangeInput(inputId= "BSBmaPR_seas2", label ="Private Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "BSBmaPR_2_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "BSBmaPR_2_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
                                    dateRangeInput(inputId= "BSBmaSH_seas2", label ="Shore Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "BSBmaSH_2_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "BSBmaSH_2_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5)))))),
        
        column(4,
               titlePanel("Scup - MA"),
               dateRangeInput(inputId = "SCUPmaFH_seas1", label ="For Hire  Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-06-30"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPmaFH_1_bag", label = "Bag Limit", min = 0, max = 100, value = 40)),
                 column(5, sliderInput(inputId= "SCUPmaFH_1_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))),
               dateRangeInput(inputId = "SCUPmaFH_seas2", label ="For Hire Season 2",  
                              start = as.Date("2027-07-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPmaFH_2_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPmaFH_2_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))), 
               dateRangeInput(inputId = "SCUPmaPR_seas1", label ="Private Season 1",  
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPmaPR_1_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPmaPR_1_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))),
               dateRangeInput(inputId = "SCUPmaSH_seas1", label ="Shore Season 1",  
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPmaSH_1_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPmaSH_1_len", label = "Min Length", min = 8, max = 12, value = 9.5, step = .5))),
               actionButton("SCUPMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmaSeason2",
                                    dateRangeInput(inputId = "SCUPmaFH_seas3", label ="For Hire Season 3",  
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SCUPmaFH_3_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SCUPmaFH_3_len", label ="Min Length", min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPmaPR_seas2", label ="Private Season 2",  
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SCUPmaPR_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SCUPmaPR_2_len", label ="Min Length", min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPmasH_seas2", label ="Shore Season 2",  
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SCUPmaSH_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SCUPmaSH_2_len", label ="Min Length", min = 8, max = 12, value = 10, step = .5)))))))
    }})
  

  ############# MA Breakout by mode ######################################
  output$BSBmaMode <- renderUI({
    if (is.null(input$BSB_MA_input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component. i.e. when all modes combined is selected only one
    switch(input$BSB_MA_input_type,
           "All Modes Combined" = div(dateRangeInput(inputId = "BSBma_seas1", label ="Season 1",
                                                     start = as.Date("2027-05-18"),
                                                     end   = as.Date("2027-09-03"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4, numericInput(inputId = "BSBma_1_bag", label ="Bag Limit", min = 0, max = 20, value = 4)),
                                        column(6, sliderInput(inputId= "BSBma_1_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5)))),
           "Separated By Mode" = div(dateRangeInput(inputId = "BSBmaFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-18"),
                                                    end   = as.Date("2027-09-03"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4, numericInput(inputId = "BSBmaFH_1_bag", label ="Bag Limit", min = 0, max = 20, value = 4)),
                                       column(6, sliderInput(inputId= "BSBmaFH_1_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
                                     dateRangeInput(inputId = "BSBmaPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-18"),
                                                    end   = as.Date("2027-09-03"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4, numericInput(inputId = "BSBmaPR_1_bag", label ="Bag Limit", min = 0, max = 20, value = 4)),
                                       column(6, sliderInput(inputId= "BSBmaPR_1_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
                                     dateRangeInput(inputId = "BSBmaSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-18"),
                                                    end   = as.Date("2027-09-03"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4, numericInput(inputId = "BSBmaSH_1_bag", label ="Bag Limit", min = 0, max = 20, value = 4)),
                                       column(6, sliderInput(inputId= "BSBmaSH_1_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5)))))
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
                                    dateRangeInput(inputId = "SFriFH_seas2", label ="For Hire Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SFriFH_2_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "SFriFH_2_len", label ="Min Length", min = 14, max = 21, value = 18, step = .5))), 
                                    dateRangeInput(inputId = "SFriPR_seas2", label ="Private Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SFriPR_2_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "SFriPR_2_len", label ="Min Length", min = 14, max = 21, value = 18, step = .5))), 
                                    dateRangeInput(inputId = "SFriSH_seas2", label ="Shore Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SFriSH_2_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "SFriSH_2_len", label ="Min Length", min = 14, max = 21, value = 18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - RI"),
               dateRangeInput(inputId = "BSBriFH_seas1", label ="For Hire Season 1",
                              start = as.Date("2027-06-18"),
                              end   = as.Date("2027-08-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "BSBriFH_1_bag", label ="Bag Limit", min = 0, max = 20, value = 2)),
                 column(6, sliderInput(inputId= "BSBriFH_1_len", label ="Min Length", min = 11, max =18, value = 16, step = .5))),
               dateRangeInput(inputId = "BSBriFH_seas2", label ="For Hire Season 2",
                              start = as.Date("2027-09-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "BSBriFH_2_bag", label ="Bag Limit", min = 0, max = 20, value = 6)),
                 column(6, sliderInput(inputId= "BSBriFH_2_len", label ="Min Length", min = 11, max = 18, value = 16, step = .5))),
               dateRangeInput(inputId = "BSBriPR_seas1", label ="Private Season 1",
                              start = as.Date("2027-05-22"),
                              end   = as.Date("2027-08-26"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "BSBriPR_1_bag", label ="Bag Limit", min = 0, max = 20, value = 2)),
                 column(6, sliderInput(inputId= "BSBriPR_1_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
               dateRangeInput(inputId = "BSBriPR_seas1", label ="Private Season 2",
                              start = as.Date("2027-08-27"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "BSBriPR_2_bag", label ="Bag Limit", min = 0, max = 20, value = 3)),
                 column(6, sliderInput(inputId= "BSBriPR_2_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
               dateRangeInput(inputId = "BSBriSH_seas1", label ="Shore Season 1",
                              start = as.Date("2027-05-22"),
                              end   = as.Date("2027-08-26"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "BSBriSH_1_bag", label ="Bag Limit", min = 0, max = 20, value = 2)),
                 column(6, sliderInput(inputId= "BSBriSH_1_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
               dateRangeInput(inputId = "BSBriSH_seas2", label ="Shore Season 2",
                              start = as.Date("2027-08-27"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "BSBriSH_2_bag", label ="Bag Limit", min = 0, max = 20, value = 3)),
                 column(6, sliderInput(inputId= "BSBriSH_2_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
               actionButton("BSBRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBriSeason3",
                                    dateRangeInput(inputId = "BSBriFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "BSBriFH_3_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "BSBriFH_3_len", label ="Min Length", min = 11, max = 18, value = 16, step = .5))),
                                    dateRangeInput(inputId = "BSBriPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "BSBriPR_3_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "BSBriPR_3_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5))),
                                    dateRangeInput(inputId = "BSBriSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "BSBriSH_3_bag", label ="Bag Limit", min = 0, max = 100, value = 0)),
                                      column(6, sliderInput(inputId= "BSBriSH_3_len", label ="Min Length", min = 11, max = 18, value = 16.5, step = .5)))))),
        
        column(4,
               titlePanel("Scup - RI"),
               dateRangeInput(inputId = "SCUPriFH_seas1", label ="For Hire Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-08-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPriFH_1_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPriFH_1_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))),
               dateRangeInput(inputId = "SCUPriFH_seas2", label ="For Hire Season 2", 
                              start = as.Date("2027-09-01"),
                              end   = as.Date("2027-10-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPriFH_2_bag", label = "Bag Limit", min = 0, max = 100, value = 40)),
                 column(5, sliderInput(inputId= "SCUPriFH_2_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))), 
               dateRangeInput(inputId = "SCUPriFH_seas3", label ="For Hire Season 3", 
                              start = as.Date("2027-11-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPriFH_3_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPriFH_3_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))), 
               dateRangeInput(inputId = "SCUPriPR_seas1", label ="Private Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPriPR_1_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPriPR_1_len", label = "Min Length", min = 8, max = 12, value = 11, step = .5))),
               dateRangeInput(inputId = "SCUPriSH_seas1", label ="Shore Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4, numericInput(inputId = "SCUPriSH_1_bag", label = "Bag Limit", min = 0, max = 100, value = 30)),
                 column(5, sliderInput(inputId= "SCUPriSH_1_len", label = "Min Length", min = 8, max = 12, value = 9.5, step = .5))),
               actionButton("SCUPRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPriSeason2",
                                    dateRangeInput(inputId = "SCUPriFH_seas4", label ="For Hire Season 4", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SCUPriFH_4_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SCUPriFH_4_len", label ="Min Length", min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPriPR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SCUPriPR_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SCUPriPR_2_len", label ="Min Length", min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPriSH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4, numericInput(inputId = "SCUPriSH_2_bag", label ="Bag Limit", min = 0, max = 20, value = 0)), 
                                      column(6, sliderInput(inputId= "SCUPriSH_2_len", label ="Min Length", min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  ############# RI Breakout by mode ######################################
  output$SFriMode <- renderUI({
    if (is.null(input$SF_RI_input_type))
      return()
    
    switch(input$SF_RI_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFri_seas1", label =" Season 1",
                                                     start = as.Date("2027-04-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4, numericInput(inputId = "SFri_1_bag", label ="Bag Limit", min = 0, max = 100, value = 6)),
                                        column(6, sliderInput(inputId= "SFri_1_len", label ="Min Length", min = 14, max = 21, value = 19, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFriFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-04-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4, numericInput(inputId = "SFriFH_1_bag", label ="Bag Limit", min = 0, max = 100, value = 6)),
                                       column(6, sliderInput(inputId= "SFriFH_1_len", label ="Min Length", min = 14, max = 21, value = 19, step = .5))),
                                     dateRangeInput(inputId = "SFriPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-04-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4, numericInput(inputId = "SFriPR_1_bag", label ="Bag Limit", min = 0, max = 100, value = 6)),
                                       column(6, sliderInput(inputId= "SFriPR_1_len", label ="Min Length", min = 5, max = 25, value = 19, step = .5))),
                                     dateRangeInput(inputId = "SFriSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-04-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4, numericInput(inputId = "SFriSH_1_bag", label ="Bag Limit", min = 0, max = 100, value = 6)),
                                       column(6, sliderInput(inputId= "SFriSH_1_len", label ="Min Length", min = 14, max = 21, value = 19, step = .5)))))
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
                                    dateRangeInput(inputId = "SFctFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFctFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    dateRangeInput(inputId = "SFctPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFctPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    dateRangeInput(inputId = "SFctSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFctSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - CT"),
               
               dateRangeInput(inputId = "BSBctFH_seas1", label ="For Hire Season 1",
                              start = as.Date("2027-05-18"),
                              end   = as.Date("2027-08-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId= "BSBctFH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               dateRangeInput(inputId = "BSBctFH_seas2", label ="For Hire Season 2",
                              start = as.Date("2027-09-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 7)),
                 column(6,
                        sliderInput(inputId= "BSBctFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               dateRangeInput(inputId = "BSBctPR_seas1", label ="Private Season 1",
                              start = as.Date("2027-05-18"),
                              end   = as.Date("2027-06-23"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId= "BSBctPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               dateRangeInput(inputId = "BSBctPR_seas2", label ="Private Season 2",
                              start = as.Date("2027-07-08"),
                              end   = as.Date("2027-11-28"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId= "BSBctPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               dateRangeInput(inputId = "BSBctSH_seas1", label ="Shore Season 1",
                              start = as.Date("2027-05-18"),
                              end   = as.Date("2027-06-23"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId= "BSBctSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               dateRangeInput(inputId = "BSBctSH_seas2", label ="Shore Season 2",
                              start = as.Date("2027-07-08"),
                              end   = as.Date("2027-11-28"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId= "BSBctSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               actionButton("BSBCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBctSeason3",
                                    dateRangeInput(inputId = "BSBctFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBctFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    dateRangeInput(inputId = "BSBctPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBctPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    dateRangeInput(inputId = "BSBctSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBctSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - CT"),
               
               dateRangeInput(inputId = "SCUPctFH_seas1", label ="For Hire Season 1",
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-08-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPctFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               dateRangeInput(inputId = "SCUPctFH_seas2", label ="For Hire Season 2",
                              start = as.Date("2027-09-01"),
                              end   = as.Date("2027-10-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId= "SCUPctFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               dateRangeInput(inputId = "SCUPctFH_seas3", label ="For Hire Season 3",
                              start = as.Date("2027-11-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPctFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               dateRangeInput(inputId = "SCUPctPR_seas1", label ="Private Season 1",
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPctPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               dateRangeInput(inputId = "SCUPctSH_seas1", label ="Shore Season 1",
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPctSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPctSeason2",
                                    dateRangeInput(inputId = "SCUPctFH_seas4", label ="For Hire Season 4",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPctFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPctPR_seas2", label ="Private Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPctPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPctSH_seas2", label ="Shore Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPctSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# CT Breakout by mode ######################################
  output$SFctMode <- renderUI({
    if (is.null(input$SF_CT_input_type))
      return()
    
    switch(input$SF_CT_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFct_seas1", label =" Season 1",
                                                     start = as.Date("2027-05-04"),
                                                     end   = as.Date("2027-08-01"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId= "SFct_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      dateRangeInput(inputId = "SFct_seas2", label =" Season 2",
                                                     start = as.Date("2027-08-02"),
                                                     end   = as.Date("2027-10-15"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId= "SFct_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFctFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-04"),
                                                    end   = as.Date("2027-08-01"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFctFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     dateRangeInput(inputId = "SFctPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-04"),
                                                    end   = as.Date("2027-08-01"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFctPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     dateRangeInput(inputId = "SFctSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-04"),
                                                    end   = as.Date("2027-08-01"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFctSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     dateRangeInput(inputId = "SFctFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-08-02"),
                                                    end   = as.Date("2027-10-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFctFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFctPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-08-02"),
                                                    end   = as.Date("2027-10-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFctPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFctSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-08-02"),
                                                    end   = as.Date("2027-10-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 2)),
                                       column(6,
                                              sliderInput(inputId= "SFctSH_2_len", label ="Min Length",
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
                                    dateRangeInput(inputId = "SFnyFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFnyFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    dateRangeInput(inputId = "SFnyPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFnyPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    dateRangeInput(inputId = "SFnySH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFnySH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NY"),
               
               selectInput("BSB_NY_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBnyMode"),
               
               
               actionButton("BSBNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBnySeason3",
                                    dateRangeInput(inputId = "BSBnyFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBnyFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    dateRangeInput(inputId = "BSBnyPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBnyPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    dateRangeInput(inputId = "BSBnySH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBnySH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NY"),
               dateRangeInput(inputId = "SCUPnyFH_seas1", label ="For Hire Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-08-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPnyFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               dateRangeInput(inputId = "SCUPnyFH_seas2", label ="For Hire Season 2", 
                              start = as.Date("2027-09-01"),
                              end   = as.Date("2027-10-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId= "SCUPnyFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               dateRangeInput(inputId = "SCUPnyFH_seas3", label ="For Hire Season 3", 
                              start = as.Date("2027-11-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPnyFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               dateRangeInput(inputId = "SCUPnyPR_seas1", label ="Private Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPnyPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               dateRangeInput(inputId = "SCUPnySH_seas1", label ="Shore Season 1", 
                              start = as.Date("2027-05-01"),
                              end   = as.Date("2027-12-31"),
                              min   = as.Date("2027-01-01"),
                              max   = as.Date("2027-12-31"),
                              format = "yyyy-mm-dd"),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnySH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId= "SCUPnySH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnySeason2",
                                    dateRangeInput(inputId = "SCUPnyFH_seas4", label ="For Hire Season 4", 
                                                   start = as.Date("2027-01-01"),
                                                   end   = as.Date("2027-01-01"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPnyFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPnyPR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-01-01"),
                                                   end   = as.Date("2027-01-01"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPnyPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId = "SCUPnySH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-01-01"),
                                                   end   = as.Date("2027-01-01"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnySH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPnySH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  ############# NY Breakout by mode ######################################
  output$SFnyMode <- renderUI({
    if (is.null(input$SF_NY_input_type))
      return()
    
    switch(input$SF_NY_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFny_seas1", label =" Season 1",
                                                     start = as.Date("2027-05-04"),
                                                     end   = as.Date("2027-08-01"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId= "SFny_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      dateRangeInput(inputId = "SFny_seas2", label =" Season 2",
                                                     start = as.Date("2027-08-02"),
                                                     end   = as.Date("2027-10-15"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId= "SFny_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFnyFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-04"),
                                                    end   = as.Date("2027-08-01"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFnyFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     dateRangeInput(inputId = "SFnyPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-04"),
                                                    end   = as.Date("2027-08-01"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFnyPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     dateRangeInput(inputId = "SFnySH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-04"),
                                                    end   = as.Date("2027-08-01"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFnySH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     dateRangeInput(inputId = "SFnyFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-08-02"),
                                                    end   = as.Date("2027-10-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFnyFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFnyPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-08-02"),
                                                    end   = as.Date("2027-10-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFnyPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFnySH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-08-02"),
                                                    end   = as.Date("2027-10-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "SFnySH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
  })
  
  
  output$BSBnyMode <- renderUI({
    if (is.null(input$BSB_NY_input_type))
      return()
    
    switch(input$BSB_NY_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "BSBny_seas1", label =" Season 1",
                                                     start = as.Date("2027-06-23"),
                                                     end   = as.Date("2027-08-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId= "BSBny_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5))), 
                                      
                                      dateRangeInput(inputId = "BSBny_seas2", label =" Season 2",
                                                     start = as.Date("2027-09-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               sliderInput(inputId= "BSBny_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5)))), 
           "Separated By Mode" = div( dateRangeInput(inputId = "BSBnyFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-06-23"),
                                                    end   = as.Date("2027-08-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "BSBnyFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     dateRangeInput(inputId = "BSBnyPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-06-23"),
                                                    end   = as.Date("2027-08-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "BSBnyPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     dateRangeInput(inputId = "BSBnySH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-06-23"),
                                                    end   = as.Date("2027-08-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId= "BSBnySH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))), 
                                     dateRangeInput(inputId = "BSBnyFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-09-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId= "BSBnyFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     dateRangeInput(inputId = "BSBnyPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-09-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId= "BSBnyPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     dateRangeInput(inputId = "BSBnySH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-09-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId= "BSBnySH_2_len", label ="Min Length",
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
                                    dateRangeInput(inputId= "SFnjFH_seas2", label ="For Hire Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 7, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFnjFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))),
                                    dateRangeInput(inputId= "SFnjPR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFnjPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5))),
                                    dateRangeInput(inputId= "SFnjSH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFnjSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NJ"),
               
               selectInput("BSB_NJ_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBnjMode"),
               
               actionButton("BSBNJaddSeason", "Add Season"), 
               #Season 5
               shinyjs::hidden( div(ID = "BSBnjSeason5",
                                    dateRangeInput(inputId= "BSBnjFH_seas5", label ="For Hire Season 5",  
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjFH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "BSBnjFH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    dateRangeInput(inputId= "BSBnjPR_seas5", label ="Private Season 5",  
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjPR_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "BSBnjPR_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    dateRangeInput(inputId= "BSBnjSH_seas5", label ="Shore Season 5",  
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjSH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "BSBnjSH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5)))))),
        
        column(4, 
               titlePanel("Scup - NJ"),
               
               selectInput("SCUP_NJ_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPnjMode"),
               
               actionButton("SCUPNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnjSeason3",
                                    dateRangeInput(inputId= "SCUPnjFH_seas3", label ="For Hire Season 3", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SCUPnjFH_3_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId= "SCUPnjPR_seas3", label ="Private Season 3", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SCUPnjPR_3_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    dateRangeInput(inputId= "SCUPnjSH_seas3", label ="Shore Season 3", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SCUPnjSH_3_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }
    
  })
  
  ############# NJ Breakout by mode ######################################
  output$SFnjMode <- renderUI({
    if (is.null(input$SF_NJ_input_type))
      return()
    
    switch(input$SF_NJ_input_type, 
           "All Modes Combined" = div( dateRangeInput(inputId = "SFnj_seas1", label =" Season 1",
                                                      start = as.Date("2027-05-04"),
                                                      end   = as.Date("2027-09-25"),
                                                      min   = as.Date("2027-01-01"),
                                                      max   = as.Date("2027-12-31"),
                                                      format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4, 
                                                numericInput(inputId = "SFnj_1_bag", label ="Bag Limit", 
                                                             min = 0, max = 100, value = 3)),
                                         column(6,
                                                sliderInput(inputId= "SFnj_1_len", label ="Min Length",
                                                            min = 14, max = 21, value = 18, step = .5)))), 
           "Separated By Mode" = div( dateRangeInput(inputId = "SFnjFH_seas1", label ="For Hire Season 1", 
                                                     start = as.Date("2027-05-04"),
                                                     end   = as.Date("2027-09-25"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4, 
                                               numericInput(inputId = "SFnjFH_1_bag", label ="Bag Limit", 
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId= "SFnjFH_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 18, step = .5)), 
                                        dateRangeInput(inputId = "SFnjPR_seas1", label ="Private Season 1", 
                                                       start = as.Date("2027-05-04"),
                                                       end   = as.Date("2027-09-25"),
                                                       min   = as.Date("2027-01-01"),
                                                       max   = as.Date("2027-12-31"),
                                                       format = "yyyy-mm-dd"),
                                        fluidRow(
                                          column(4, 
                                                 numericInput(inputId = "SFnjPR_1_bag", label ="Bag Limit",
                                                              min = 0, max = 100, value = 3)),
                                          column(6,
                                                 sliderInput(inputId= "SFnjPR_1_len", label ="Min Length",
                                                             min = 14, max = 21, value = 18, step = .5))),
                                        dateRangeInput(inputId = "SFnjSH_seas1", label ="Shore Season 1", 
                                                       start = as.Date("2027-05-04"),
                                                       end   = as.Date("2027-09-25"),
                                                       min   = as.Date("2027-01-01"),
                                                       max   = as.Date("2027-12-31"),
                                                       format = "yyyy-mm-dd"),
                                        fluidRow(
                                          column(4, 
                                                 numericInput(inputId = "SFnjSH_1_bag", label ="Bag Limit",
                                                              min = 0, max = 100, value = 3)), 
                                          column(6,
                                                 sliderInput(inputId= "SFnjSH_1_len", label ="Min Length",
                                                             min = 14, max = 21, value = 18, step = .5))))))
  })
  
  
  output$BSBnjMode <- renderUI({
    if (is.null(input$BSB_NJ_input_type))
      return()
    
    switch(input$BSB_NJ_input_type,
           
           "All Modes Combined" = div( dateRangeInput(inputId = "BSBnj_seas1", label =" Season 1",
                                                      start = as.Date("2027-05-17"),
                                                      end   = as.Date("2027-06-19"),
                                                      min   = as.Date("2027-01-01"),
                                                      max   = as.Date("2027-12-31"),
                                                      format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_1_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 10)), 
                                         column(6,
                                                sliderInput(inputId= "BSBnj_1_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5))),
                                       
                                       #Season 2
                                       dateRangeInput(inputId = "BSBnj_seas2", label =" Season 2",
                                                      start = as.Date("2027-07-01"),
                                                      end   = as.Date("2027-08-31"),
                                                      min   = as.Date("2027-01-01"),
                                                      max   = as.Date("2027-12-31"),
                                                      format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 1)), 
                                         column(6,
                                                sliderInput(inputId= "BSBnj_2_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5))),
                                       
                                       #Season 3
                                       dateRangeInput(inputId = "BSBnj_seas3", label =" Season 3",
                                                      start = as.Date("2027-10-01"),
                                                      end   = as.Date("2027-10-31"),
                                                      min   = as.Date("2027-01-01"),
                                                      max   = as.Date("2027-12-31"),
                                                      format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 10)), 
                                         column(6,
                                                sliderInput(inputId= "BSBnj_3_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5))),
                                       
                                       #Season 4
                                       dateRangeInput(inputId = "BSBnj_seas4", label =" Season 4",
                                                      start = as.Date("2027-11-01"),
                                                      end   = as.Date("2027-12-31"),
                                                      min   = as.Date("2027-01-01"),
                                                      max   = as.Date("2027-12-31"),
                                                      format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "BSBnj_4_bag", label ="Bag Limit",
                                                             min = 0, max = 20, value = 15)), 
                                         column(6,
                                                sliderInput(inputId= "BSBnj_4_len", label ="Min Length",
                                                            min = 11, max = 18, value = 12.5, step = .5)))),
           
           "Separated By Mode" = div( dateRangeInput(inputId = "BSBnjFH_seas1", label ="For Hire Season 1", 
                                                     start = as.Date("2027-05-17"),
                                                     end   = as.Date("2027-06-19"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjFH_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjPR_seas1", label ="Private Season 1", 
                                                     start = as.Date("2027-05-17"),
                                                     end   = as.Date("2027-06-19"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjPR_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjSH_seas1", label ="Shore Season 1", 
                                                     start = as.Date("2027-05-17"),
                                                     end   = as.Date("2027-06-19"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjSH_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      #Season 2
                                      dateRangeInput(inputId = "BSBnjFH_seas2", label ="For Hire Season 2", 
                                                     start = as.Date("2027-07-01"),
                                                     end   = as.Date("2027-08-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjFH_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjPR_seas2", label ="Private Season 2", 
                                                     start = as.Date("2027-07-01"),
                                                     end   = as.Date("2027-08-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjPR_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjSH_seas2", label ="Shore Season 2", 
                                                     start = as.Date("2027-07-01"),
                                                     end   = as.Date("2027-08-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjSH_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      #Season 3
                                      dateRangeInput(inputId = "BSBnjFH_seas3", label ="For Hire Season 3", 
                                                     start = as.Date("2027-10-01"),
                                                     end   = as.Date("2027-10-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjFH_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjPR_seas3", label ="Private Season 3", 
                                                     start = as.Date("2027-10-01"),
                                                     end   = as.Date("2027-10-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjPR_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjSH_seas3", label ="Shore Season 3", 
                                                     start = as.Date("2027-10-01"),
                                                     end   = as.Date("2027-10-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjSH_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      #Season 4
                                      dateRangeInput(inputId = "BSBnjFH_seas4", label ="For Hire Season 4", 
                                                     start = as.Date("2027-11-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjFH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjFH_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjPR_seas4", label ="Private Season 4", 
                                                     start = as.Date("2027-11-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjPR_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjPR_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      dateRangeInput(inputId = "BSBnjSH_seas4", label ="Shore Season 4", 
                                                     start = as.Date("2027-11-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnjSH_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId= "BSBnjSH_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5)))))
  })
  
  output$SCUPnjMode <- renderUI({
    if (is.null(input$SCUP_NJ_input_type))
      return()
    
    switch(input$SCUP_NJ_input_type,
           
           "All Modes Combined" = div(dateRangeInput(inputId = "SCUPnj_seas1", label =" Season 1",
                                                      start = as.Date("2027-01-01"),
                                                      end   = as.Date("2027-06-30"),
                                                      min   = as.Date("2027-01-01"),
                                                      max   = as.Date("2027-12-31"),
                                                      format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "SCUPnj_1_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 30)),
                                         column(6,
                                                sliderInput(inputId= "SCUPnj_1_len", label ="Min Length",
                                                            min = 8, max = 12, value = 10, step = .5))), 
                                      dateRangeInput(inputId = "SCUPnj_seas2", label =" Season 2",
                                                     start = as.Date("2027-09-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                       fluidRow(
                                         column(4,
                                                numericInput(inputId = "SCUPnj_2_bag", label ="Bag Limit",
                                                             min = 0, max = 100, value = 30)),
                                         column(6,
                                                sliderInput(inputId= "SCUPnj_2_len", label ="Min Length",
                                                            min = 8, max = 12, value = 10, step = .5)))),
           "Separated By Mode" = div(dateRangeInput(inputId = "SCUPnjFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-06-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPnjFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     dateRangeInput(inputId = "SCUPnjPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-06-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPnjPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     dateRangeInput(inputId = "SCUPnjSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-06-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPnjSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     
                                     dateRangeInput(inputId = "SCUPnjFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-09-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPnjFH_2_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     dateRangeInput(inputId = "SCUPnjPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-09-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPnjPR_2_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     dateRangeInput(inputId = "SCUPnjSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-09-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPnjSH_2_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5)))))
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
                                    dateRangeInput(inputId= "SFdeFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFdeFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFdePR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFdePR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFdeSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFdeSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - DE"),
               
               selectInput("BSB_DE_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBdeMode"),
               
               
               actionButton("BSBDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBdeSeason3",
                                    dateRangeInput(inputId= "BSBdeFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBdeFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBdePR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBdePR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBdeSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBdeSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - DE"),
               
               selectInput("SCUP_DE_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPdeMode"),
               
               actionButton("SCUPDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPdeSeason2",
                                    dateRangeInput(inputId= "SCUPdeFH_seas2", label ="For Hire Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPdeFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPdePR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdePR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPdePR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPdeSH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPdeSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## DE breakout by mode ############################
  
  output$SFdeMode <- renderUI({
    if (is.null(input$SF_DE_input_type))
      return()
    
    switch(input$SF_DE_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFde_seas1", label ="Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-05-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId= "SFde_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      dateRangeInput(inputId = "SFde_seas2", label ="Season 2",
                                                     start = as.Date("2027-06-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId= "SFde_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFdeFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFdeFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     dateRangeInput(inputId = "SFdePR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFdePR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     dateRangeInput(inputId = "SFdeSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFdeSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     
                                     dateRangeInput(inputId = "SFdeFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFdeFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFdePR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFdePR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFdeSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFdeSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBdeMode <- renderUI({
    if (is.null(input$BSB_DE_input_type))
      return()
    
    switch(input$BSB_DE_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "BSBde_seas1", label =" Season 1",
                                                     start = as.Date("2027-05-15"),
                                                     end   = as.Date("2027-09-30"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBde_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      dateRangeInput(inputId = "BSBde_seas2", label =" Season 2",
                                                     start = as.Date("2027-10-10"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBde_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "BSBdeFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBdeFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBdePR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBdePR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     dateRangeInput(inputId = "BSBdeSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBdeSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     dateRangeInput(inputId = "BSBdeFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBdeFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBdePR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBdePR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBdeSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBdeSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPdeMode <- renderUI({
    if (is.null(input$SCUP_DE_input_type))
      return()
    
    switch(input$SCUP_DE_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SCUPde_seas1", label =" Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId= "SCUPde_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SCUPdeFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPdeFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPdePR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPdePR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPdeSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPdeSH_1_len", label ="Min Length",
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
                                    dateRangeInput(inputId= "SFmdFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFmdFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFmdPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFmdPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFmdSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFmdSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MD"),
               
               selectInput("BSB_MD_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBmdMode"),
               
               
               actionButton("BSBMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmdSeason3",
                                    dateRangeInput(inputId= "BSBmdFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBmdFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBmdPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBmdPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBmdSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBmdSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MD"),
               
               selectInput("SCUP_MD_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPmdMode"),
               
               actionButton("SCUPMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmdSeason2",
                                    dateRangeInput(inputId= "SCUPmdFH_seas2", label ="For Hire Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPmdFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPmdPR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPmdPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPmdSH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPmdSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## MD breakout by mode ############################
  output$SFmdMode <- renderUI({
    if (is.null(input$SF_MD_input_type))
      return()
    
    switch(input$SF_MD_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFmd_seas1", label ="Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-05-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId= "SFmd_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      dateRangeInput(inputId = "SFmd_seas2", label ="Season 2",
                                                     start = as.Date("2027-06-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId= "SFmd_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFmdFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFmdFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     dateRangeInput(inputId = "SFmdPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFmdPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     dateRangeInput(inputId = "SFmdSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFmdSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     
                                     dateRangeInput(inputId = "SFmdFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFmdFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFmdPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFmdPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFmdSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFmdSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  output$BSBmdMode <- renderUI({
    if (is.null(input$BSB_MD_input_type))
      return()
    
    switch(input$BSB_MD_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "BSBmd_seas1", label =" Season 1",
                                                     start = as.Date("2027-05-15"),
                                                     end   = as.Date("2027-09-30"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBmd_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      dateRangeInput(inputId = "BSBmd_seas2", label =" Season 2",
                                                     start = as.Date("2027-10-10"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBmd_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "BSBmdFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBmdFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBmdPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBmdPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     dateRangeInput(inputId = "BSBmdSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBmdSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     dateRangeInput(inputId = "BSBmdFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBmdFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBmdPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBmdPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBmdSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBmdSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPmdMode <- renderUI({
    if (is.null(input$SCUP_MD_input_type))
      return()
    
    switch(input$SCUP_MD_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SCUPmd_seas1", label =" Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId= "SCUPmd_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SCUPmdFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPmdFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPmdPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPmdPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPmdSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPmdSH_1_len", label ="Min Length",
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
                                    dateRangeInput(inputId= "SFvaFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFvaFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFvaPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFvaPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    dateRangeInput(inputId= "SFvaSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFvaSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - VA"),
               
               selectInput("BSB_VA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBvaMode"),
               
               
               actionButton("BSBVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBvaSeason3",
                                    dateRangeInput(inputId= "BSBvaFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBvaFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBvaPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBvaPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBvaSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBvaSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - VA"),
               
               selectInput("SCUP_VA_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPvaMode"),
               
               actionButton("SCUPVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPvaSeason2",
                                    dateRangeInput(inputId= "SCUPvaFH_seas2", label ="For Hire Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPvaFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPvaPR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPvaPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPvaSH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPvaSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## VA breakout by mode ############################
  
  output$SFvaMode <- renderUI({
    if (is.null(input$SF_VA_input_type))
      return()
    
    switch(input$SF_VA_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFva_seas1", label =" Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-05-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId= "SFva_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      dateRangeInput(inputId = "SFva_seas2", label =" Season 2",
                                                     start = as.Date("2027-06-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId= "SFva_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFvaFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFvaFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     dateRangeInput(inputId = "SFvaPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFvaPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     dateRangeInput(inputId = "SFvaSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-05-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFvaSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))),
                                     dateRangeInput(inputId = "SFvaFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFvaFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFvaPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFvaPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     dateRangeInput(inputId = "SFvaSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-06-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId= "SFvaSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBvaMode <- renderUI({
    if (is.null(input$BSB_VA_input_type))
      return()
    
    switch(input$BSB_VA_input_type, 
           "All Modes Combined" = div( dateRangeInput(inputId = "BSBva_seas1", label =" Season 1",
                                                     start = as.Date("2027-05-15"),
                                                     end   = as.Date("2027-07-15"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBva_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      dateRangeInput(inputId = "BSBva_seas2", label =" Season 2",
                                                     start = as.Date("2027-07-25"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBva_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "BSBvaFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-07-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBvaFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBvaPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-07-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBvaPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     dateRangeInput(inputId = "BSBvaSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-07-15"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBvaSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     dateRangeInput(inputId = "BSBvaFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-07-25"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBvaFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBvaPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-07-25"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBvaPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBvaSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-07-25"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBvaSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPvaMode <- renderUI({
    if (is.null(input$SCUP_VA_input_type))
      return()
    
    switch(input$SCUP_VA_input_type, 
           "All Modes Combined" = div( dateRangeInput(inputId = "SCUPva_seas1", label =" Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId= "SCUPva_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SCUPvaFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPvaFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPvaPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPvaPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPvaSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPvaSH_1_len", label ="Min Length",
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
                                    dateRangeInput(inputId = "SFncFH_seas2", label = "For Hire Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFncFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    dateRangeInput(inputId = "SFncPR_seas2", label = "Private Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFncPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    dateRangeInput(inputId = "SFncSH_seas2", label = "Shore Season 2",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "SFncSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NC"),
               
               selectInput("BSB_NC_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("BSBncMode"),
               
               
               actionButton("BSBNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBncSeason3",
                                    dateRangeInput(inputId= "BSBncFH_seas3", label ="For Hire Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBncFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBncPR_seas3", label ="Private Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBncPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    dateRangeInput(inputId= "BSBncSH_seas3", label ="Shore Season 3",
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId= "BSBncSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NC"),
               
               selectInput("SCUP_NC_input_type", "Regulations combined or separated by mode?",
                           c("All Modes Combined", "Separated By Mode")),
               uiOutput("SCUPncMode"),
               
               actionButton("SCUPNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPncSeason2",
                                    dateRangeInput(inputId= "SCUPncFH_seas2", label ="For Hire Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPncFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPncPR_seas2", label ="Private Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPncPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    dateRangeInput(inputId= "SCUPncSH_seas2", label ="Shore Season 2", 
                                                   start = as.Date("2027-12-31"),
                                                   end   = as.Date("2027-12-31"),
                                                   min   = as.Date("2027-01-01"),
                                                   max   = as.Date("2027-12-31"),
                                                   format = "yyyy-mm-dd"),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId= "SCUPncSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## NC breakout by mode ############################
  
  output$SFncMode <- renderUI({
    if (is.null(input$SF_NC_input_type))
      return()
    
    switch(input$SF_NC_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SFnc_seas1", label ="Season 1",
                                                     start = as.Date("2027-08-16"),
                                                     end   = as.Date("2027-09-30"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 1)),
                                        column(6,
                                               sliderInput(inputId= "SFnc_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 15, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SFncFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-08-16"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId= "SFncFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     dateRangeInput(inputId = "SFncPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-08-16"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId= "SFncPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     dateRangeInput(inputId = "SFncSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-08-16"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId= "SFncSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5)))))
  })
  
  
  output$BSBncMode <- renderUI({
    if (is.null(input$BSB_NC_input_type))
      return()
    
    switch(input$BSB_NC_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "BSBnc_seas1", label =" Season 1",
                                                     start = as.Date("2027-05-15"),
                                                     end   = as.Date("2027-09-30"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBnc_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      dateRangeInput(inputId = "BSBnc_seas2", label =" Season 2",
                                                     start = as.Date("2027-10-10"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId= "BSBnc_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "BSBncFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBncFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBncPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBncPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     dateRangeInput(inputId = "BSBncSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-05-15"),
                                                    end   = as.Date("2027-09-30"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBncSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     dateRangeInput(inputId = "BSBncFH_seas2", label ="For Hire Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBncFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBncPR_seas2", label ="Private Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBncPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     dateRangeInput(inputId = "BSBncSH_seas2", label ="Shore Season 2",
                                                    start = as.Date("2027-10-10"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId= "BSBncSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPncMode <- renderUI({
    if (is.null(input$SCUP_NC_input_type))
      return()
    
    switch(input$SCUP_NC_input_type, 
           "All Modes Combined" = div(dateRangeInput(inputId = "SCUPnc_seas1", label =" Season 1",
                                                     start = as.Date("2027-01-01"),
                                                     end   = as.Date("2027-12-31"),
                                                     min   = as.Date("2027-01-01"),
                                                     max   = as.Date("2027-12-31"),
                                                     format = "yyyy-mm-dd"),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId= "SCUPnc_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Separated By Mode" = div(dateRangeInput(inputId = "SCUPncFH_seas1", label ="For Hire Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPncFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPncPR_seas1", label ="Private Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPncPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     dateRangeInput(inputId = "SCUPncSH_seas1", label ="Shore Season 1",
                                                    start = as.Date("2027-01-01"),
                                                    end   = as.Date("2027-12-31"),
                                                    min   = as.Date("2027-01-01"),
                                                    max   = as.Date("2027-12-31"),
                                                    format = "yyyy-mm-dd"),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId= "SCUPncSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  
  
  ################ Summary page outputs #################

  perc_changes <- reactive({
    all_data() %>%
      dplyr::filter(stringr::str_detect(filename, "SQ")) %>%
      dplyr::group_by(state, filename, metric, mode, species) %>%
      dplyr::summarise(value = round(median(value), 2), .groups = "drop") %>%
      dplyr::mutate(pca_reqs = 0.1)
  })

  output$summary_rhl_fig <- plotly::renderPlotly({
    
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes" & model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    harv <- data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    
    harv2 <- harv %>%
      ggplot2::ggplot(ggplot2::aes(x = species, y = median_pct_diff, label = Run_Name)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_text(color = "black", hjust = -0.25, size = 3) +
      ggplot2::facet_wrap(~ state) +
      ggplot2::labs(title = "Percentage change in Recreational Harvest By State", x = "", y = "Median % Change in Harvest") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw()
    
    plotly::ggplotly(harv2) %>% plotly::style(textposition = "top center")
  })
  
  output$summary_percdiff_table <- DT::renderDT({
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes" & model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    
    harv <- data %>%
      dplyr::filter(metric == "keep_weight" & mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = species, values_from = median_pct_diff)
    
    harv %>%
      dplyr::rowwise() %>%
      dplyr::ungroup() %>%
      dplyr::select(-metric) %>%
      mutate(
        bsb  = paste0(sprintf("%.2f", bsb),  "%"),
        scup = paste0(sprintf("%.2f", scup), "%"),
        sf   = paste0(sprintf("%.2f", sf),   "%")
      ) %>%
      dplyr::rename(State = state, `Run Name` = filename.x,
                    `BSB Median % Change` = bsb, `Scup Median % Change` = scup,
                    `SF Median %Change`   = sf)
  })

  output$summary_regs_table <- DT::renderDT({
    
    regs_data() %>%
      tidyr::separate(input, into = c("species","season","measure"), sep = "_") %>%
      dplyr::mutate(season = stringr::str_remove(season, "^seas")) %>%
      tidyr::extract(species, into = c("species","state2","mode"), regex = "([^a-z]+)([a-z]+)(.*)") %>%
      dplyr::select(-state2) %>%
      dplyr::group_by(run_name, state, species, mode, season) %>%
      tidyr::pivot_wider(names_from = measure, values_from = value) %>%
      dplyr::filter(!bag == 0) %>%
      dplyr::mutate(season2 = paste0(op, " - ", cl)) %>%
      dplyr::group_by(run_name, state, species, mode) %>%
      dplyr::summarise(bag = paste(bag, collapse=","), len = paste(len, collapse=","),
                       season = paste(season2, collapse=","), .groups = "drop") %>%
      dplyr::mutate(mode   = if_else(mode == "", "All modes", mode),
                    season = gsub("2025-", "", season))
  })
  
  ### Helper functions for state-level figures (unchanged logic, use all_data())
  rhl_fig <- function(state_name){
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(metric == "keep_weight", mode == "all modes", model == "SQ", state == state_name) %>%
      dplyr::mutate(ref_value = value) %>%
      dplyr::select(filename, species, state, draw, ref_value)
    harv <- data %>%
      dplyr::filter(metric == "keep_weight", mode == "all modes", state == state_name) %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    harv2 <- harv %>%
      ggplot2::ggplot(ggplot2::aes(x = species, y = median_pct_diff, label = Run_Name)) +
      ggplot2::geom_point(size = 2) + ggplot2::geom_text(color = "black", hjust = -0.25, size = 3) +
      ggplot2::facet_wrap(~ state) +
      ggplot2::labs(title = "Percentage change in Recreational Harvest By State", x = "", y = "Median % Change in Harvest") +
      ggplot2::geom_hline(yintercept = 0) + ggplot2::theme_bw()
    plotly::ggplotly(harv2) %>% plotly::style(textposition = "top center")
  }
  
  cv_fig <- function(state_name){
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(state == state_name, metric == "keep_weight", mode == "all modes", model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>% dplyr::select(filename, species, state, draw, ref_value)
    harv <- data %>%
      dplyr::filter(state == state_name, metric == "keep_weight", mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      dplyr::rename(filename = filename.x)
    welfare <- data %>%
      dplyr::filter(metric %in% c("CV"), state == state_name, mode == "all modes") %>%
      dplyr::group_by(filename) %>%
      dplyr::mutate(value = value / 1000000) %>%
      dplyr::summarise(CV = round(median(value), 2), ci_lower = quantile(value, 0.05),
                       ci_upper = quantile(value, 0.95), .groups = "drop") %>%
      left_join(harv, by = "filename")
    p1 <- welfare %>%
      ggplot2::ggplot(ggplot2::aes(x = median_pct_diff, y = CV, label = filename)) +
      ggplot2::geom_point() + ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle("Angler Satisfaction") + ggplot2::ylab("Angler Satisfaction ($M)") +
      ggplot2::xlab("Change in Harvest from SQ (%)") + ggplot2::theme(legend.position = "none") +
      ggplot2::facet_wrap(.~species) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) + ggplot2::theme_bw()
    plotly::ggplotly(p1) %>% plotly::style(textposition = "top center")
  }
  
  trips_fig <- function(state_name){
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(metric == "keep_weight", state == state_name, mode == "all modes", model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>% dplyr::select(filename, species, state, draw, ref_value)
    harv <- data %>%
      dplyr::filter(metric == "keep_weight", state == state_name, mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    trips <- data %>%
      dplyr::filter(metric %in% c("predicted_trips"), state == state_name, mode == "all modes") %>%
      dplyr::group_by(filename) %>%
      dplyr::summarise(trips = median(value), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename) %>%
      dplyr::left_join(harv, by = "Run_Name") %>%
      dplyr::mutate(trips = round(trips / 1000000, 2))
    p1 <- trips %>%
      ggplot2::ggplot(ggplot2::aes(x = median_pct_diff, y = trips, label = Run_Name)) +
      ggplot2::geom_point() + ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle(paste("Number of Trips in", state_name)) +
      ggplot2::ylab("Predicted trips (N) millions") + ggplot2::xlab("Change in Harvest from SQ (%)") +
      ggplot2::theme(legend.position = "none") + ggplot2::facet_wrap(. ~ species) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) + ggplot2::theme_bw()
    plotly::ggplotly(p1) %>% plotly::style(textposition = "top center")
  }
  
  discards_fig <- function(state_name){
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(metric == "keep_weight", state == state_name, mode == "all modes", model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>% dplyr::select(filename, species, state, draw, ref_value)
    harv <- data %>%
      dplyr::filter(metric == "keep_weight", state == state_name, mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_keep_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    disc <- data %>%
      dplyr::filter(metric == "release_weight", state == state_name, mode == "all modes") %>%
      dplyr::group_by(state, filename, species) %>%
      dplyr::summarise(median_rel_weight = median(value), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename) %>%
      dplyr::left_join(harv, by = c("state","Run_Name","species")) %>%
      dplyr::mutate(median_rel_weight = round(median_rel_weight / 1000000, 2))
    p1 <- disc %>%
      ggplot2::ggplot(ggplot2::aes(x = median_keep_pct_diff, y = median_rel_weight, label = Run_Name)) +
      ggplot2::geom_point() + ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle(paste("Discards in", state_name)) +
      ggplot2::ylab("Discards (million lbs)") + ggplot2::xlab("Change in Harvest from SQ (%)") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::facet_wrap(. ~ species, scales = "free") + ggplot2::theme_bw() +
      ggplot2::theme(panel.spacing = ggplot2::unit(-0.5, "cm"))
    plotly::ggplotly(p1) %>% plotly::style(textposition = "top center")
  }
  
  totmort_fig <- function(state_name){
    data <- all_data()
    ref_pct <- data %>%
      dplyr::filter(metric == "keep_weight", state == state_name, mode == "all modes", model == "SQ") %>%
      dplyr::mutate(ref_value = value) %>% dplyr::select(filename, species, state, draw, ref_value)
    harv <- data %>%
      dplyr::filter(metric == "keep_weight", state == state_name, mode == "all modes") %>%
      dplyr::left_join(ref_pct, by = dplyr::join_by(species, state, draw)) %>%
      dplyr::mutate(pct_diff = (value - ref_value) / (ref_value + 1) * 100) %>%
      dplyr::group_by(state, filename.x, species, metric) %>%
      dplyr::summarise(median_keep_pct_diff = round(median(pct_diff), 2), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename.x)
    mort <- data %>%
      dplyr::filter(metric %in% c("keep_weight","discmort_weight"), state == state_name, mode == "all modes") %>%
      dplyr::group_by(state, filename, species, mode, draw, model) %>%
      dplyr::summarise(mort = sum(value), .groups = "drop") %>%
      dplyr::group_by(state, filename, species) %>%
      dplyr::summarise(median_totmort_weight = median(mort), .groups = "drop") %>%
      dplyr::rename(Run_Name = filename) %>%
      dplyr::left_join(harv, by = c("state","Run_Name","species")) %>%
      dplyr::mutate(median_rel_weight = round(median_totmort_weight / 1000000, 2))
    p1 <- mort %>%
      ggplot2::ggplot(ggplot2::aes(x = median_keep_pct_diff, y = median_totmort_weight, label = Run_Name)) +
      ggplot2::geom_point() + ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::ggtitle(paste("Total mortality in", state_name)) +
      ggplot2::ylab("Total Mortality (million lbs)") + ggplot2::xlab("Change in Harvest from SQ (%)") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
      ggplot2::facet_wrap(. ~ species, scales = "free") + ggplot2::theme_bw() +
      ggplot2::theme(panel.spacing = ggplot2::unit(-0.5, "cm"))
    plotly::ggplotly(p1) %>% plotly::style(textposition = "top center")
  }
  
  ### MA
  output$ma_rhl_fig      <- plotly::renderPlotly({ rhl_fig("MA") })
  output$ma_CV_fig       <- plotly::renderPlotly({ cv_fig("MA") })
  output$ma_trips_fig    <- plotly::renderPlotly({ trips_fig("MA") })
  output$ma_discards_fig <- plotly::renderPlotly({ discards_fig("MA") })
  output$ma_totmort_fig  <- plotly::renderPlotly({ totmort_fig("MA") })
  ### RI
  output$ri_rhl_fig      <- plotly::renderPlotly({ rhl_fig("RI") })
  output$ri_CV_fig       <- plotly::renderPlotly({ cv_fig("RI") })
  output$ri_trips_fig    <- plotly::renderPlotly({ trips_fig("RI") })
  output$ri_discards_fig <- plotly::renderPlotly({ discards_fig("RI") })
  output$ri_totmort_fig  <- plotly::renderPlotly({ totmort_fig("RI") })
  ### CT
  output$ct_rhl_fig      <- plotly::renderPlotly({ rhl_fig("CT") })
  output$ct_CV_fig       <- plotly::renderPlotly({ cv_fig("CT") })
  output$ct_trips_fig    <- plotly::renderPlotly({ trips_fig("CT") })
  output$ct_discards_fig <- plotly::renderPlotly({ discards_fig("CT") })
  output$ct_totmort_fig  <- plotly::renderPlotly({ totmort_fig("CT") })
  ### NY
  output$ny_rhl_fig      <- plotly::renderPlotly({ rhl_fig("NY") })
  output$ny_CV_fig       <- plotly::renderPlotly({ cv_fig("NY") })
  output$ny_trips_fig    <- plotly::renderPlotly({ trips_fig("NY") })
  output$ny_discards_fig <- plotly::renderPlotly({ discards_fig("NY") })
  output$ny_totmort_fig  <- plotly::renderPlotly({ totmort_fig("NY") })
  ### NJ
  output$nj_rhl_fig      <- plotly::renderPlotly({ rhl_fig("NJ") })
  output$nj_CV_fig       <- plotly::renderPlotly({ cv_fig("NJ") })
  output$nj_trips_fig    <- plotly::renderPlotly({ trips_fig("NJ") })
  output$nj_discards_fig <- plotly::renderPlotly({ discards_fig("NJ") })
  output$nj_totmort_fig  <- plotly::renderPlotly({ totmort_fig("NJ") })
  ### DE
  output$de_rhl_fig      <- plotly::renderPlotly({ rhl_fig("DE") })
  output$de_CV_fig       <- plotly::renderPlotly({ cv_fig("DE") })
  output$de_trips_fig    <- plotly::renderPlotly({ trips_fig("DE") })
  output$de_discards_fig <- plotly::renderPlotly({ discards_fig("DE") })
  output$de_totmort_fig  <- plotly::renderPlotly({ totmort_fig("DE") })
  ### MD
  output$md_rhl_fig      <- plotly::renderPlotly({ rhl_fig("MD") })
  output$md_CV_fig       <- plotly::renderPlotly({ cv_fig("MD") })
  output$md_trips_fig    <- plotly::renderPlotly({ trips_fig("MD") })
  output$md_discards_fig <- plotly::renderPlotly({ discards_fig("MD") })
  output$md_totmort_fig  <- plotly::renderPlotly({ totmort_fig("MD") })
  ### VA
  output$va_rhl_fig      <- plotly::renderPlotly({ rhl_fig("VA") })
  output$va_CV_fig       <- plotly::renderPlotly({ cv_fig("VA") })
  output$va_trips_fig    <- plotly::renderPlotly({ trips_fig("VA") })
  output$va_discards_fig <- plotly::renderPlotly({ discards_fig("VA") })
  output$va_totmort_fig  <- plotly::renderPlotly({ totmort_fig("VA") })
  ### NC
  output$nc_rhl_fig      <- plotly::renderPlotly({ rhl_fig("NC") })
  output$nc_CV_fig       <- plotly::renderPlotly({ cv_fig("NC") })
  output$nc_trips_fig    <- plotly::renderPlotly({ trips_fig("NC") })
  output$nc_discards_fig <- plotly::renderPlotly({ discards_fig("NC") })
  output$nc_totmort_fig  <- plotly::renderPlotly({ totmort_fig("NC") })

  state_outputs <- c("ma","ri","ct","ny","nj","de","md","va","nc")
  fig_suffixes  <- c("rhl_fig","CV_fig","discards_fig","totmort_fig","trips_fig")
  for (st in state_outputs) {
    for (suf in fig_suffixes) {
      outputOptions(output, paste0(st, "_", suf), suspendWhenHidden = TRUE)
    }
  }

  ####  Storing Inputs for decoupled model ####

  observeEvent(input$runmeplease, {
    library(httr)
    library(jsonlite)
    library(openssl)
    library(uuid)

    # enqueue_simple_sas <- function(run_name, queue_url_sas = Sys.getenv("AZURE_STORAGE_QUEUE_URL")) {
    #   stopifnot(nzchar(run_name), nzchar(queue_url_sas))
    #   payload <- list(
    #     runName = run_name,
    #     submissionId = UUIDgenerate(),
    #     submittedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    #   )
    #   msg_b64 <- base64_encode(charToRaw(toJSON(payload, auto_unbox = TRUE)))
    #   xml_body <- sprintf("<QueueMessage><MessageText>%s</MessageText></QueueMessage>", msg_b64)
    #   
    #   res <- POST(
    #     url = queue_url_sas,
    #     body = xml_body,
    #     content_type_xml(),
    #     add_headers(`x-ms-version` = "2020-10-02")
    #   )
    #   stop_for_status(res)
    #   invisible(TRUE)
    # }
    
    regulations <- NULL

    if(any("MA" == input$state)){
      print("start MA")
      if(input$BSB_MA_input_type == "All Modes Combined"){
        bsbMAregs <- data.frame(run_name = c(Run_Name()), state = c("MA"), 
                                input =  c("BSBma_seas1_op","BSBma_seas1_cl","BSBma_1_bag","BSBma_1_len", 
                                           "BSBmaFH_seas2_op","BSBmaFH_seas2_cl","BSBmaFH_2_bag","BSBmaFH_2_len", 
                                           "BSBmaPR_seas2_op","BSBmaPR_seas2_cl","BSBmaPR_2_bag","BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op","BSBmaSH_seas2_cl","BSBmaSH_2_bag","BSBmaSH_2_len"),
                                value = c(as.character(input$BSBma_seas1[1]),as.character(input$BSBma_seas1[2]),as.character(input$BSBma_1_bag),as.character(input$BSBma_1_len), 
                                          as.character(input$BSBmaFH_seas2[1]),as.character(input$BSBmaFH_seas2[2]),as.character(input$BSBmaFH_2_bag),as.character(input$BSBmaFH_2_len), 
                                          as.character(input$BSBmaPR_seas2[1]),as.character(input$BSBmaPR_seas2[2]),as.character(input$BSBmaPR_2_bag),as.character(input$BSBmaPR_2_len),
                                          as.character(input$BSBmaSH_seas2[1]),as.character(input$BSBmaSH_seas2[2]),as.character(input$BSBmaSH_2_bag),as.character(input$BSBmaSH_2_len)))
      }else{
        bsbMAregs <- data.frame(run_name = c(Run_Name()), state = c("MA"), 
                                input =  c("BSBmaFH_seas1_op","BSBmaFH_seas1_cl","BSBmaFH_1_bag","BSBmaFH_1_len", 
                                           "BSBmaPR_seas1_op","BSBmaPR_seas1_cl","BSBmaPR_1_bag","BSBmaPR_1_len",
                                           "BSBmaSH_seas1_op","BSBmaSH_seas1_cl","BSBmaSH_1_bag","BSBmaSH_1_len",
                                           "BSBmaFH_seas2_op","BSBmaFH_seas2_cl","BSBmaFH_2_bag","BSBmaFH_2_len", 
                                           "BSBmaPR_seas2_op","BSBmaPR_seas2_cl","BSBmaPR_2_bag","BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op","BSBmaSH_seas2_cl","BSBmaSH_2_bag","BSBmaSH_2_len"),
                                value = c(as.character(input$BSBmaFH_seas1[1]),as.character(input$BSBmaFH_seas1[2]),as.character(input$BSBmaFH_1_bag),as.character(input$BSBmaFH_1_len),
                                          as.character(input$BSBmaPR_seas1[1]),as.character(input$BSBmaPR_seas1[2]),as.character(input$BSBmaPR_1_bag),as.character(input$BSBmaPR_1_len), 
                                          as.character(input$BSBmaSH_seas1[1]),as.character(input$BSBmaSH_seas1[2]),as.character(input$BSBmaSH_1_bag),as.character(input$BSBmaSH_1_len), 
                                          as.character(input$BSBmaFH_seas2[1]),as.character(input$BSBmaFH_seas2[2]),as.character(input$BSBmaFH_2_bag),as.character(input$BSBmaFH_2_len), 
                                          as.character(input$BSBmaPR_seas2[1]),as.character(input$BSBmaPR_seas2[2]),as.character(input$BSBmaPR_2_bag),as.character(input$BSBmaPR_2_len),
                                          as.character(input$BSBmaSH_seas2[1]),as.character(input$BSBmaSH_seas2[2]),as.character(input$BSBmaSH_2_bag),as.character(input$BSBmaSH_2_len)))
      }
      
      MA_regs <- data.frame(run_name = c(Run_Name()), state = c("MA"), 
                            input =  c("SFmaFH_seas1_op","SFmaFH_seas1_cl","SFmaFH_1_bag","SFmaFH_1_len", 
                                       "SFmaPR_seas1_op","SFmaPR_seas1_cl","SFmaPR_1_bag","SFmaPR_1_len",
                                       "SFmaSH_seas1_op","SFmaSH_seas1_cl","SFmaSH_1_bag","SFmaSH_1_len",
                                       "SFmaFH_seas2_op","SFmaFH_seas2_cl","SFmaFH_2_bag","SFmaFH_2_len", 
                                       "SFmaPR_seas2_op","SFmaPR_seas2_cl","SFmaPR_2_bag","SFmaPR_2_len",
                                       "SFmaSH_seas2_op","SFmaSH_seas2_cl","SFmaSH_2_bag","SFmaSH_2_len",
                                       "SCUPmaFH_seas1_op","SCUPmaFH_seas1_cl","SCUPmaFH_1_bag","SCUPmaFH_1_len", 
                                       "SCUPmaPR_seas1_op","SCUPmaPR_seas1_cl","SCUPmaPR_1_bag","SCUPmaPR_1_len",
                                       "SCUPmaSH_seas1_op","SCUPmaSH_seas1_cl","SCUPmaSH_1_bag","SCUPmaSH_1_len",
                                       "SCUPmaFH_seas2_op","SCUPmaFH_seas2_cl","SCUPmaFH_2_bag","SCUPmaFH_2_len", 
                                       "SCUPmaPR_seas2_op","SCUPmaPR_seas2_cl","SCUPmaPR_2_bag","SCUPmaPR_2_len",
                                       "SCUPmaSH_seas2_op","SCUPmaSH_seas2_cl","SCUPmaSH_2_bag","SCUPmaSH_2_len", 
                                       "SCUPmaFH_seas3_op","SCUPmaFH_seas3_cl","SCUPmaFH_3_bag","SCUPmaFH_3_len"),
                            value = c(as.character(input$SFmaFH_seas1[1]),as.character(input$SFmaFH_seas1[2]),as.character(input$SFmaFH_1_bag),as.character(input$SFmaFH_1_len), 
                                      as.character(input$SFmaPR_seas1[1]),as.character(input$SFmaPR_seas1[2]),as.character(input$SFmaPR_1_bag),as.character(input$SFmaPR_1_len),
                                      as.character(input$SFmaSH_seas1[1]),as.character(input$SFmaSH_seas1[2]),as.character(input$SFmaSH_1_bag),as.character(input$SFmaSH_1_len),
                                      as.character(input$SFmaFH_seas2[1]),as.character(input$SFmaFH_seas2[2]),as.character(input$SFmaFH_2_bag),as.character(input$SFmaFH_2_len), 
                                      as.character(input$SFmaPR_seas2[1]),as.character(input$SFmaPR_seas2[2]),as.character(input$SFmaPR_2_bag),as.character(input$SFmaPR_2_len),
                                      as.character(input$SFmaSH_seas2[1]),as.character(input$SFmaSH_seas2[2]),as.character(input$SFmaSH_2_bag),as.character(input$SFmaSH_2_len),
                                      as.character(input$SCUPmaFH_seas1[1]),as.character(input$SCUPmaFH_seas1[2]),as.character(input$SCUPmaFH_1_bag),as.character(input$SCUPmaFH_1_len), 
                                      as.character(input$SCUPmaPR_seas1[1]),as.character(input$SCUPmaPR_seas1[2]),as.character(input$SCUPmaPR_1_bag),as.character(input$SCUPmaPR_1_len),
                                      as.character(input$SCUPmaSH_seas1[1]),as.character(input$SCUPmaSH_seas1[2]),as.character(input$SCUPmaSH_1_bag),as.character(input$SCUPmaSH_1_len),
                                      as.character(input$SCUPmaFH_seas2[1]),as.character(input$SCUPmaFH_seas2[2]),as.character(input$SCUPmaFH_2_bag),as.character(input$SCUPmaFH_2_len), 
                                      as.character(input$SCUPmaPR_seas2[1]),as.character(input$SCUPmaPR_seas2[2]),as.character(input$SCUPmaPR_2_bag),as.character(input$SCUPmaPR_2_len),
                                      as.character(input$SCUPmaSH_seas2[1]),as.character(input$SCUPmaSH_seas2[2]),as.character(input$SCUPmaSH_2_bag),as.character(input$SCUPmaSH_2_len),
                                      as.character(input$SCUPmaFH_seas3[1]),as.character(input$SCUPmaFH_seas3[2]),as.character(input$SCUPmaFH_3_bag),as.character(input$SCUPmaFH_3_len)))
      regulations <- regulations %>% rbind(MA_regs, bsbMAregs)
    }
    
    # RI, CT, NY, NJ, DE, MD, VA, NC regulation blocks are unchanged from
    # the original — keep them all here in your actual file.

    readr::write_csv(regulations, file = here::here(paste0("saved_regs/regs_", input$Run_Name, ".csv")))
    print("saved_inputs")
    
    output$message <- renderText("Regulations saved - we will run these soon be sure to change run name before clicking again.")
  })

  # Get list of files from the folder
  available_files <- reactive({
    folder_path <- here::here("output/")
    if (dir.exists(folder_path)) {
      files <- list.files(folder_path, full.names = FALSE)
      if (length(files) > 0) return(files)
    }
    return(character(0))
  })
  
  file_mapping <- reactive({
    files <- available_files()
    if (length(files) > 0) {
      display_names <- files %>%
        stringr::str_remove("^output_") %>%
        stringr::str_remove("_[0-9]+") %>%
        stringr::str_remove("_[0-9]+") %>%
        stringr::str_remove(".csv")
      names(files) <- display_names
      return(files)
    }
    return(character(0))
  })
  
  observe({
    file_map <- file_mapping()
    if (length(file_map) > 0) {
      updateSelectInput(session, "file_choice", choices = file_map, selected = file_map[1])
    } else {
      updateSelectInput(session, "file_choice", choices = "No files available", selected = NULL)
    }
  })
  
  output$file_info <- renderText({
    if (is.null(input$file_choice) || input$file_choice == "No files available") return("No file selected or no files available.")
    file_path <- file.path("output", input$file_choice)
    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
      display_name <- tools::file_path_sans_ext(input$file_choice)
      paste("Display name:", display_name, "\nFull filename:", input$file_choice,
            "\nFile size:", round(file_info$size / 1024, 2), "KB",
            "\nLast modified:", format(file_info$mtime, "%Y-%m-%d %H:%M:%S"), sep = "\n")
    } else { "File not found." }
  })
  
  output$download_file <- downloadHandler(
    filename = function() {
      if (!is.null(input$file_choice) && input$file_choice != "No files available") return(input$file_choice)
      return("file.txt")
    },
    content = function(file) {
      if (!is.null(input$file_choice) && input$file_choice != "No files available") {
        file_path <- file.path("output", input$file_choice)
        if (file.exists(file_path)) file.copy(file_path, file) else writeLines("Error: File not found.", file)
      } else { writeLines("Error: No file selected.", file) }
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
