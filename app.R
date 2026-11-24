
## SOUTH SUDAN ISS SUPERVISION AND EPIDEMIOLOGICAL SURVEILLANCE DASHBOARD
## Synthetic training dataset: 21,528 supervision records
## Keep app.R and south_sudan_iss_synthetic_epidemiological_dataset.csv in the same folder.

library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)
library(DT)
library(scales)

## DATA PATH
data_path <- "south_sudan_iss_synthetic_epidemiological_dataset.csv"

## HELPER FUNCTIONS

clean_chr <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x %in% c("", "NA", "N/A", "n/a", "NULL", "null",
             "Unknown", "unknown")] <- NA_character_
  x
}

get_col <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(clean_chr(data[[column_name]]))
  }
  rep(NA_character_, nrow(data))
}

get_num <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(suppressWarnings(as.numeric(data[[column_name]])))
  }
  rep(NA_real_, nrow(data))
}

parse_visit_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  
  parsed <- suppressWarnings(
    parse_date_time(
      as.character(x),
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "dmy HMS", "dmy HM", "dmy",
        "mdy HMS", "mdy HM", "mdy"
      ),
      quiet = TRUE
    )
  )
  
  as.Date(parsed)
}

normalize_facility <- function(x) {
  x <- clean_chr(x)
  stringr::str_to_lower(stringr::str_squish(x))
}

safe_sum <- function(x) {
  sum(x, na.rm = TRUE)
}

safe_percent <- function(numerator, denominator) {
  if (denominator == 0) return(NA_real_)
  100 * numerator / denominator
}

## LOAD DATA

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Dataset not found: ", data_path,
      ". Place the CSV file in the same folder as app.R."
    )
  )
}

raw_data <- readr::read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  janitor::clean_names()

if (!"date_of_visit" %in% names(raw_data)) {
  stop("The dataset must contain the date_of_visit column.")
}

## PREPARE DATA

data <- raw_data %>%
  mutate(
    state = get_col(raw_data, "states_calc"),
    district = get_col(raw_data, "districts_calc"),
    supervision_type = get_col(raw_data, "supervision_type"),
    priority_level = get_col(raw_data, "priority_level"),
    designation = get_col(raw_data, "designation"),
    
    facility = get_col(raw_data, "name_of_facility_visited"),
    
    facility_type_calc = get_col(
      raw_data, "type_of_facility_visited_calc"
    ),
    
    facility_type_original = get_col(
      raw_data, "type_of_facility_visited"
    ),
    
    facility_type = coalesce(
      facility_type_calc,
      facility_type_original
    ),
    
    facility_status = get_col(
      raw_data, "facility_status_calc"
    ),
    
    visit_date = parse_visit_date(raw_data$date_of_visit),
    
    idsr_functional = get_col(raw_data, "idsr_functional"),
    
    weekly_reporting_status = get_col(
      raw_data, "weekly_reporting_status"
    ),
    
    case_definition_status = get_col(
      raw_data, "case_definition_status"
    ),
    
    surveillance_focal_person_status = get_col(
      raw_data, "surveillance_focal_person_status"
    ),
    
    patient_register_available = get_col(
      raw_data, "patient_register_available"
    ),
    
    zero_reporting_status = get_col(
      raw_data, "zero_reporting_status"
    ),
    
    alert_detected = get_col(
      raw_data, "alert_detected"
    ),
    
    alert_reporting_timeliness = get_col(
      raw_data, "alert_reporting_timeliness"
    ),
    
    outbreak_investigation_status = get_col(
      raw_data, "outbreak_investigation_status"
    ),
    
    specimen_transport_status = get_col(
      raw_data, "specimen_transport_status"
    ),
    
    feedback_provided = get_col(
      raw_data, "feedback_provided"
    ),
    
    action_plan_status = get_col(
      raw_data, "action_plan_status"
    ),
    
    followup_required = get_col(
      raw_data, "followup_required"
    ),
    
    unreported_afp_cases = get_num(
      raw_data, "unreported_afp_cases"
    ),
    
    unreported_measles_cases = get_num(
      raw_data, "unreported_measles_cases"
    ),
    
    unreported_acute_jaundice_cases = get_num(
      raw_data, "unreported_acute_jaundice_cases"
    ),
    
    unreported_acute_flaccid_paralysis_cases = get_num(
      raw_data, "unreported_acute_flaccid_paralysis_cases"
    ),
    
    supervision_duration_minutes = get_num(
      raw_data, "supervision_duration_minutes"
    ),
    
    year = lubridate::year(visit_date),
    
    quarter = if_else(
      !is.na(visit_date),
      paste0(
        lubridate::year(visit_date),
        "-Q",
        lubridate::quarter(visit_date)
      ),
      NA_character_
    ),
    
    month = if_else(
      !is.na(visit_date),
      format(visit_date, "%Y-%m"),
      NA_character_
    ),
    
    week = if_else(
      !is.na(visit_date),
      paste0(
        lubridate::isoyear(visit_date),
        "-W",
        sprintf("%02d", lubridate::isoweek(visit_date))
      ),
      NA_character_
    ),
    
    facility_id = normalize_facility(facility)
  )

## FILTER VARIABLES
## These variables are used to filter records only.
## They are not used as chart grouping dimensions or comparison axes.

filter_ids <- c(
  "year",
  "quarter",
  "month",
  "week",
  "state",
  "district",
  "facility",
  "supervision_type",
  "priority_level",
  "facility_type",
  "facility_status",
  "designation"
)

## UI

ui <- dashboardPage(
  
  dashboardHeader(
    title = "ISS & EPIDEMIOLOGY",
    titleWidth = 290
  ),
  
  dashboardSidebar(
    
    width = 290,
    
    sidebarMenu(
      id = "tabs",
      
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Surveillance Performance",
        tabName = "surveillance",
        icon = icon("heartbeat")
      ),
      
      menuItem(
        "Disease Events",
        tabName = "disease_events",
        icon = icon("exclamation-triangle")
      ),
      
      menuItem(
        "Facility Detail",
        tabName = "facility_detail",
        icon = icon("hospital")
      ),
      
      menuItem(
        "Data Quality",
        tabName = "data_quality",
        icon = icon("database")
      )
    ),
    
    tags$hr(),
    
    h4(
      "DASHBOARD FILTERS",
      style = "color:white;padding-left:15px;font-weight:bold;"
    ),
    
    selectInput(
      "year", "Year",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "quarter", "Quarter",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "month", "Month",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "week", "Week",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "state", "State",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "district", "District",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "facility", "Facility",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "supervision_type", "Supervision Type",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "priority_level", "Priority Level",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "facility_type", "Facility Type",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "facility_status", "Facility Status",
      choices = NULL, multiple = TRUE
    ),
    
    selectInput(
      "designation", "Designation",
      choices = NULL, multiple = TRUE
    ),
    
    br(),
    
    actionButton(
      "reset",
      "Reset Filters",
      icon = icon("refresh"),
      class = "btn-warning"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(
        HTML("
          body {
            font-family: Arial, sans-serif;
          }

          .content-wrapper {
            background-color: #f4f6f9;
          }

          .small-box {
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,.15);
          }

          .box {
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,.10);
          }

          .box-header {
            font-weight: bold;
          }

          .sidebar .form-group {
            margin-bottom: 12px;
          }

          .sidebar label {
            color: white;
          }

          .small-box h3 {
            font-size: 27px;
            font-weight: bold;
          }

          table.dataTable thead th {
            background-color: #2c3e50;
            color: white;
          }

          .content {
            padding-top: 15px;
          }

          .dashboard-note {
            background: #eef5fb;
            padding: 12px;
            border-left: 4px solid #3c8dbc;
            margin-bottom: 15px;
            border-radius: 4px;
          }
        ")
      )
    ),
    
    tabItems(
      
      ## OVERVIEW
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("kpi_records", width = 3),
          valueBoxOutput("kpi_facilities", width = 3),
          valueBoxOutput("kpi_idsr", width = 3),
          valueBoxOutput("kpi_reporting", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Surveillance Indicator Performance Profile",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            p(
              "Shows the percentage of records meeting selected surveillance performance indicators."
            ),
            
            plotlyOutput(
              "indicator_profile",
              height = "480px"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "IDSR Functionality",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "idsr_chart",
              height = "370px"
            )
          ),
          
          box(
            title = "Weekly Reporting Performance",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "reporting_chart",
              height = "370px"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Alert Detection",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "alerts_chart",
              height = "370px"
            )
          ),
          
          box(
            title = "Outbreak Investigation",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "outbreak_chart",
              height = "370px"
            )
          )
        )
      ),
      
      ## SURVEILLANCE PERFORMANCE
      
      tabItem(
        tabName = "surveillance",
        
        fluidRow(
          valueBoxOutput("kpi_case_definition", width = 3),
          valueBoxOutput("kpi_focal_person", width = 3),
          valueBoxOutput("kpi_register", width = 3),
          valueBoxOutput("kpi_followup", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Case Definition Availability",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "case_definition_chart",
              height = "360px"
            )
          ),
          
          box(
            title = "Surveillance Focal Person",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "focal_person_chart",
              height = "360px"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Patient Register Availability",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "register_chart",
              height = "360px"
            )
          ),
          
          box(
            title = "Zero Reporting Status",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "zero_reporting_chart",
              height = "360px"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Alert Reporting Timeliness",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "alert_timeliness_chart",
              height = "360px"
            )
          ),
          
          box(
            title = "Specimen Transportation Status",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "specimen_chart",
              height = "360px"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Feedback Provided",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "feedback_chart",
              height = "360px"
            )
          ),
          
          box(
            title = "Action Plan Implementation",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "action_plan_chart",
              height = "360px"
            )
          )
        )
      ),
      
      ## DISEASE EVENTS
      
      tabItem(
        tabName = "disease_events",
        
        fluidRow(
          valueBoxOutput("kpi_alerts", width = 4),
          valueBoxOutput("kpi_outbreaks", width = 4),
          valueBoxOutput("kpi_unreported", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Unreported Priority Disease Events",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            
            p(
              "These are simulated supervision-level indicators, not population incidence estimates."
            ),
            
            plotlyOutput(
              "unreported_chart",
              height = "430px"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Alert Reporting Timeliness",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "alert_timeliness_events_chart",
              height = "360px"
            )
          ),
          
          box(
            title = "Outbreak Investigation Status",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "outbreak_events_chart",
              height = "360px"
            )
          )
        )
      ),
      
      ## FACILITY DETAIL
      
      tabItem(
        tabName = "facility_detail",
        
        fluidRow(
          box(
            title = "Facility-Level Supervision and Surveillance Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            p(
              "Use the sidebar filters to narrow this table to a state, district, facility or reporting period."
            ),
            
            DTOutput("facility_table")
          )
        )
      ),
      
      ## DATA QUALITY
      
      tabItem(
        tabName = "data_quality",
        
        fluidRow(
          valueBoxOutput("dq_total", width = 3),
          valueBoxOutput("dq_date", width = 3),
          valueBoxOutput("dq_state", width = 3),
          valueBoxOutput("dq_facility", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Data Quality Summary",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("data_quality_table")
          )
        )
      )
    )
  )
)

## SERVER

server <- function(input, output, session) {
  
  ## POPULATE FILTER CHOICES
  
  observe({
    
    choices <- list(
      
      year = sort(
        unique(data$year[!is.na(data$year)]),
        decreasing = TRUE
      ),
      
      quarter = sort(
        unique(data$quarter[!is.na(data$quarter)])
      ),
      
      month = sort(
        unique(data$month[!is.na(data$month)])
      ),
      
      week = sort(
        unique(data$week[!is.na(data$week)])
      ),
      
      state = sort(
        unique(data$state[!is.na(data$state)])
      ),
      
      district = sort(
        unique(data$district[!is.na(data$district)])
      ),
      
      facility = sort(
        unique(data$facility[!is.na(data$facility)])
      ),
      
      supervision_type = sort(
        unique(data$supervision_type[
          !is.na(data$supervision_type)
        ])
      ),
      
      priority_level = sort(
        unique(data$priority_level[
          !is.na(data$priority_level)
        ])
      ),
      
      facility_type = sort(
        unique(data$facility_type[
          !is.na(data$facility_type)
        ])
      ),
      
      facility_status = sort(
        unique(data$facility_status[
          !is.na(data$facility_status)
        ])
      ),
      
      designation = sort(
        unique(data$designation[
          !is.na(data$designation)
        ])
      )
    )
    
    for (id in filter_ids) {
      updateSelectInput(
        session,
        id,
        choices = choices[[id]]
      )
    }
  })
  
  ## RESET FILTERS
  
  observeEvent(input$reset, {
    
    for (id in filter_ids) {
      updateSelectInput(
        session,
        id,
        selected = character(0)
      )
    }
  })
  
  ## FILTERED DATA
  
  filtered_data <- reactive({
    
    df <- data
    
    if (!is.null(input$year) && length(input$year) > 0) {
      df <- df %>%
        filter(year %in% as.numeric(input$year))
    }
    
    if (!is.null(input$quarter) && length(input$quarter) > 0) {
      df <- df %>%
        filter(quarter %in% input$quarter)
    }
    
    if (!is.null(input$month) && length(input$month) > 0) {
      df <- df %>%
        filter(month %in% input$month)
    }
    
    if (!is.null(input$week) && length(input$week) > 0) {
      df <- df %>%
        filter(week %in% input$week)
    }
    
    if (!is.null(input$state) && length(input$state) > 0) {
      df <- df %>%
        filter(state %in% input$state)
    }
    
    if (!is.null(input$district) && length(input$district) > 0) {
      df <- df %>%
        filter(district %in% input$district)
    }
    
    if (!is.null(input$facility) && length(input$facility) > 0) {
      df <- df %>%
        filter(facility %in% input$facility)
    }
    
    if (!is.null(input$supervision_type) &&
        length(input$supervision_type) > 0) {
      df <- df %>%
        filter(supervision_type %in% input$supervision_type)
    }
    
    if (!is.null(input$priority_level) &&
        length(input$priority_level) > 0) {
      df <- df %>%
        filter(priority_level %in% input$priority_level)
    }
    
    if (!is.null(input$facility_type) &&
        length(input$facility_type) > 0) {
      df <- df %>%
        filter(facility_type %in% input$facility_type)
    }
    
    if (!is.null(input$facility_status) &&
        length(input$facility_status) > 0) {
      df <- df %>%
        filter(facility_status %in% input$facility_status)
    }
    
    if (!is.null(input$designation) &&
        length(input$designation) > 0) {
      df <- df %>%
        filter(designation %in% input$designation)
    }
    
    df
  })
  
  ## GENERIC SURVEILLANCE INDICATOR CHART
  
  make_indicator_plot <- function(df, variable, chart_title) {
    
    validate(
      need(
        nrow(df) > 0,
        "No records available for the selected filters."
      )
    )
    
    plot_data <- df %>%
      filter(!is.na(.data[[variable]])) %>%
      count(
        category = .data[[variable]],
        name = "records"
      ) %>%
      arrange(records)
    
    validate(
      need(
        nrow(plot_data) > 0,
        paste(
          "No non-missing data available for",
          chart_title
        )
      )
    )
    
    p <- ggplot(
      plot_data,
      aes(
        x = records,
        y = reorder(category, records),
        text = paste0(
          chart_title, ": ", category,
          "<br>Supervision records: ", comma(records),
          "<br>Percentage: ",
          percent(
            records / sum(records),
            accuracy = 0.1
          )
        )
      )
    ) +
      geom_col(fill = "#2878A8") +
      theme_minimal(base_size = 12) +
      labs(
        x = "Number of supervision records",
        y = NULL
      ) +
      scale_x_continuous(
        labels = comma
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
  }
  
  ## OVERVIEW KPIs
  
  output$kpi_records <- renderValueBox({
    
    valueBox(
      comma(nrow(filtered_data())),
      "Supervision Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$kpi_facilities <- renderValueBox({
    
    n <- n_distinct(
      na.omit(filtered_data()$facility_id)
    )
    
    valueBox(
      comma(n),
      "Facilities Represented",
      icon = icon("hospital-o"),
      color = "purple"
    )
  })
  
  output$kpi_idsr <- renderValueBox({
    
    df <- filtered_data()
    
    denominator <- sum(
      !is.na(df$idsr_functional)
    )
    
    numerator <- sum(
      df$idsr_functional == "Yes",
      na.rm = TRUE
    )
    
    pct <- safe_percent(
      numerator,
      denominator
    )
    
    valueBox(
      ifelse(
        is.na(pct),
        "NA",
        paste0(round(pct, 1), "%")
      ),
      "IDSR Functional",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$kpi_reporting <- renderValueBox({
    
    df <- filtered_data()
    
    denominator <- sum(
      !is.na(df$weekly_reporting_status)
    )
    
    numerator <- sum(
      df$weekly_reporting_status == "On time",
      na.rm = TRUE
    )
    
    pct <- safe_percent(
      numerator,
      denominator
    )
    
    valueBox(
      ifelse(
        is.na(pct),
        "NA",
        paste0(round(pct, 1), "%")
      ),
      "Weekly Reports On Time",
      icon = icon("calendar-check-o"),
      color = "aqua"
    )
  })
  
  ## OVERVIEW CHART: INDICATOR PERFORMANCE PROFILE
  
  output$indicator_profile <- renderPlotly({
    
    df <- filtered_data()
    
    indicator_data <- tibble(
      
      indicator = c(
        "IDSR functionality",
        "Weekly reporting on time",
        "Case definition available",
        "Surveillance focal person present",
        "Patient register available",
        "Alerts detected",
        "Feedback provided",
        "Action plan completed"
      ),
      
      numerator = c(
        sum(df$idsr_functional == "Yes", na.rm = TRUE),
        
        sum(
          df$weekly_reporting_status == "On time",
          na.rm = TRUE
        ),
        
        sum(
          df$case_definition_status == "Yes",
          na.rm = TRUE
        ),
        
        sum(
          df$surveillance_focal_person_status == "Yes",
          na.rm = TRUE
        ),
        
        sum(
          df$patient_register_available == "Yes",
          na.rm = TRUE
        ),
        
        sum(
          df$alert_detected == "Yes",
          na.rm = TRUE
        ),
        
        sum(
          df$feedback_provided == "Yes",
          na.rm = TRUE
        ),
        
        sum(
          df$action_plan_status == "Completed",
          na.rm = TRUE
        )
      ),
      
      denominator = c(
        sum(!is.na(df$idsr_functional)),
        sum(!is.na(df$weekly_reporting_status)),
        sum(!is.na(df$case_definition_status)),
        sum(!is.na(df$surveillance_focal_person_status)),
        sum(!is.na(df$patient_register_available)),
        sum(!is.na(df$alert_detected)),
        sum(!is.na(df$feedback_provided)),
        sum(!is.na(df$action_plan_status))
      )
    ) %>%
      mutate(
        percentage = if_else(
          denominator > 0,
          100 * numerator / denominator,
          NA_real_
        )
      ) %>%
      filter(!is.na(percentage))
    
    validate(
      need(
        nrow(indicator_data) > 0,
        "No surveillance indicator data available."
      )
    )
    
    p <- ggplot(
      indicator_data,
      aes(
        x = percentage,
        y = reorder(indicator, percentage),
        text = paste0(
          "Indicator: ", indicator,
          "<br>Positive records: ", comma(numerator),
          "<br>Records with data: ", comma(denominator),
          "<br>Percentage: ", round(percentage, 1), "%"
        )
      )
    ) +
      geom_col(fill = "#2878A8") +
      theme_minimal(base_size = 12) +
      labs(
        x = "Percentage of records meeting indicator",
        y = NULL
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        labels = function(x) paste0(x, "%")
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  ## OVERVIEW SURVEILLANCE CHARTS
  
  output$idsr_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "idsr_functional",
      "IDSR functionality"
    )
  })
  
  output$reporting_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "weekly_reporting_status",
      "Weekly reporting status"
    )
  })
  
  output$alerts_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "alert_detected",
      "Alert detection"
    )
  })
  
  output$outbreak_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "outbreak_investigation_status",
      "Outbreak investigation status"
    )
  })
  
  ## SURVEILLANCE PERFORMANCE KPIs
  
  output$kpi_case_definition <- renderValueBox({
    
    df <- filtered_data()
    
    denominator <- sum(
      !is.na(df$case_definition_status)
    )
    
    numerator <- sum(
      df$case_definition_status == "Yes",
      na.rm = TRUE
    )
    
    pct <- safe_percent(
      numerator,
      denominator
    )
    
    valueBox(
      ifelse(
        is.na(pct),
        "NA",
        paste0(round(pct, 1), "%")
      ),
      "Case Definition Available",
      icon = icon("file-text"),
      color = "blue"
    )
  })
  
  output$kpi_focal_person <- renderValueBox({
    
    df <- filtered_data()
    
    denominator <- sum(
      !is.na(df$surveillance_focal_person_status)
    )
    
    numerator <- sum(
      df$surveillance_focal_person_status == "Yes",
      na.rm = TRUE
    )
    
    pct <- safe_percent(
      numerator,
      denominator
    )
    
    valueBox(
      ifelse(
        is.na(pct),
        "NA",
        paste0(round(pct, 1), "%")
      ),
      "Focal Person Present",
      icon = icon("user-md"),
      color = "green"
    )
  })
  
  output$kpi_register <- renderValueBox({
    
    df <- filtered_data()
    
    denominator <- sum(
      !is.na(df$patient_register_available)
    )
    
    numerator <- sum(
      df$patient_register_available == "Yes",
      na.rm = TRUE
    )
    
    pct <- safe_percent(
      numerator,
      denominator
    )
    
    valueBox(
      ifelse(
        is.na(pct),
        "NA",
        paste0(round(pct, 1), "%")
      ),
      "Patient Register Available",
      icon = icon("book"),
      color = "purple"
    )
  })
  
  output$kpi_followup <- renderValueBox({
    
    n <- sum(
      filtered_data()$followup_required == "Yes",
      na.rm = TRUE
    )
    
    valueBox(
      comma(n),
      "Records Requiring Follow-up",
      icon = icon("clipboard"),
      color = "yellow"
    )
  })
  
  ## SURVEILLANCE PERFORMANCE CHARTS
  
  output$case_definition_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "case_definition_status",
      "Case definition availability"
    )
  })
  
  output$focal_person_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "surveillance_focal_person_status",
      "Surveillance focal person"
    )
  })
  
  output$register_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "patient_register_available",
      "Patient register availability"
    )
  })
  
  output$zero_reporting_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "zero_reporting_status",
      "Zero reporting status"
    )
  })
  
  output$alert_timeliness_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "alert_reporting_timeliness",
      "Alert reporting timeliness"
    )
  })
  
  output$specimen_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "specimen_transport_status",
      "Specimen transportation"
    )
  })
  
  output$feedback_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "feedback_provided",
      "Supervision feedback"
    )
  })
  
  output$action_plan_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "action_plan_status",
      "Action plan implementation"
    )
  })
  
  ## DISEASE EVENT KPIs
  
  output$kpi_alerts <- renderValueBox({
    
    n <- sum(
      filtered_data()$alert_detected == "Yes",
      na.rm = TRUE
    )
    
    valueBox(
      comma(n),
      "Alerts Detected",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$kpi_outbreaks <- renderValueBox({
    
    x <- filtered_data()$outbreak_investigation_status
    
    n <- sum(
      !is.na(x) & x != "Not required",
      na.rm = TRUE
    )
    
    valueBox(
      comma(n),
      "Outbreak Investigations Recorded",
      icon = icon("medkit"),
      color = "yellow"
    )
  })
  
  output$kpi_unreported <- renderValueBox({
    
    df <- filtered_data()
    
    total <- safe_sum(df$unreported_afp_cases) +
      safe_sum(df$unreported_measles_cases) +
      safe_sum(df$unreported_acute_jaundice_cases) +
      safe_sum(df$unreported_acute_flaccid_paralysis_cases)
    
    valueBox(
      comma(total),
      "Simulated Unreported Disease Events",
      icon = icon("warning"),
      color = "red"
    )
  })
  
  ## UNREPORTED DISEASE EVENTS
  
  output$unreported_chart <- renderPlotly({
    
    df <- filtered_data()
    
    disease_data <- tibble(
      
      disease_indicator = c(
        "AFP cases",
        "Measles cases",
        "Acute jaundice cases",
        "Acute flaccid paralysis cases"
      ),
      
      cases = c(
        safe_sum(df$unreported_afp_cases),
        safe_sum(df$unreported_measles_cases),
        safe_sum(df$unreported_acute_jaundice_cases),
        safe_sum(df$unreported_acute_flaccid_paralysis_cases)
      )
    )
    
    validate(
      need(
        nrow(df) > 0,
        "No records available for the selected filters."
      )
    )
    
    p <- ggplot(
      disease_data,
      aes(
        x = cases,
        y = reorder(disease_indicator, cases),
        text = paste0(
          "Disease indicator: ", disease_indicator,
          "<br>Simulated count: ", comma(cases)
        )
      )
    ) +
      geom_col(fill = "#C0392B") +
      theme_minimal(base_size = 12) +
      labs(
        x = "Simulated unreported case count",
        y = NULL
      ) +
      scale_x_continuous(
        labels = comma
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  output$alert_timeliness_events_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "alert_reporting_timeliness",
      "Alert reporting timeliness"
    )
  })
  
  output$outbreak_events_chart <- renderPlotly({
    
    make_indicator_plot(
      filtered_data(),
      "outbreak_investigation_status",
      "Outbreak investigation status"
    )
  })
  
  ## FACILITY DETAIL TABLE
  
  output$facility_table <- renderDT({
    
    df <- filtered_data() %>%
      filter(!is.na(facility_id)) %>%
      group_by(
        state,
        district,
        facility,
        facility_type,
        facility_status
      ) %>%
      summarise(
        
        supervision_records = n(),
        
        idsr_functional_records = sum(
          idsr_functional == "Yes",
          na.rm = TRUE
        ),
        
        reports_on_time = sum(
          weekly_reporting_status == "On time",
          na.rm = TRUE
        ),
        
        alerts_detected = sum(
          alert_detected == "Yes",
          na.rm = TRUE
        ),
        
        followup_required = sum(
          followup_required == "Yes",
          na.rm = TRUE
        ),
        
        first_visit = if (all(is.na(visit_date))) {
          as.Date(NA)
        } else {
          min(visit_date, na.rm = TRUE)
        },
        
        last_visit = if (all(is.na(visit_date))) {
          as.Date(NA)
        } else {
          max(visit_date, na.rm = TRUE)
        },
        
        .groups = "drop"
      ) %>%
      arrange(desc(supervision_records))
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })
  
  ## DATA QUALITY KPIs
  
  output$dq_total <- renderValueBox({
    
    valueBox(
      comma(nrow(filtered_data())),
      "Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$dq_date <- renderValueBox({
    
    valueBox(
      comma(sum(is.na(filtered_data()$visit_date))),
      "Missing Visit Dates",
      icon = icon("calendar-times-o"),
      color = "yellow"
    )
  })
  
  output$dq_state <- renderValueBox({
    
    valueBox(
      comma(sum(is.na(filtered_data()$state))),
      "Missing States",
      icon = icon("map"),
      color = "red"
    )
  })
  
  output$dq_facility <- renderValueBox({
    
    valueBox(
      comma(sum(is.na(filtered_data()$facility))),
      "Missing Facility Names",
      icon = icon("hospital-o"),
      color = "purple"
    )
  })
  
  ## DATA QUALITY TABLE
  
  output$data_quality_table <- renderDT({
    
    df <- filtered_data()
    
    variables <- list(
      
      "Visit Date" = df$visit_date,
      
      "State" = df$state,
      
      "District" = df$district,
      
      "Facility" = df$facility,
      
      "Supervision Type" = df$supervision_type,
      
      "Priority Level" = df$priority_level,
      
      "Facility Type" = df$facility_type,
      
      "Facility Status" = df$facility_status,
      
      "Designation" = df$designation,
      
      "IDSR Functionality" = df$idsr_functional,
      
      "Weekly Reporting Status" = df$weekly_reporting_status,
      
      "Case Definition Status" = df$case_definition_status,
      
      "Alert Detected" = df$alert_detected,
      
      "Outbreak Investigation Status" =
        df$outbreak_investigation_status,
      
      "Specimen Transportation" =
        df$specimen_transport_status
    )
    
    quality <- tibble(
      
      Variable = names(variables),
      
      Records = nrow(df),
      
      Missing = purrr::map_int(
        variables,
        ~ sum(is.na(.x))
      ),
      
      Missing_Percent = if (nrow(df) == 0) {
        0
      } else {
        Missing / Records * 100
      }
    ) %>%
      mutate(
        Missing_Percent = paste0(
          round(Missing_Percent, 1),
          "%"
        )
      )
    
    datatable(
      quality,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
}

## RUN APPLICATION

shinyApp(
  ui = ui,
  server = server
)
