library("shiny")
library("bslib")
library("htmltools")
library("shinyTime")
library("plotly")
library("kableExtra")

# Analysis tab cards ----

card1 <- card(card_header(paste("Plot of All Monitor Data"), class = "text-success"),
              textOutput("analyte"),
              plotlyOutput("sensor_data_plot"),
              full_screen = TRUE, max_height = 450)

card2 <- card(card_header("Plot of Monitoring Data", class = "text-success"),
              plotlyOutput("sampling_time_plot"),
              full_screen = TRUE, max_height = 450)

card3 <- card(card_header("Event Log", class = "text-success"), 
              tableOutput("event_text"), 
              full_screen = TRUE, max_height = 450)

card4 <- card(card_header("Results", class = "text-success"),
              textOutput("site_result_text"),
              textOutput("NTP_result_text"),
              max_height = 450)

# CCV tab cards ----

card5 <- card(card_header("Plot of All Monitor Data", class = "text-success"),
              plotlyOutput("qc_sensor_data_plot"),
              full_screen = TRUE, max_height = 450)

card6 <- card(card_header("Calibration", class = "text-success"),
              plotlyOutput("calibration_time_plot"),
              full_screen = TRUE, max_height = 450)

card7 <- card(card_header("Pre Monitoring CCV", class = "text-success"), 
              textOutput("ccv1_result_text"),
              plotlyOutput("ccv1_time_plot"), 
              full_screen = TRUE, max_height = 450)

card8 <- card(card_header("Post Monitoring CCV", class = "text-success"),
              textOutput("ccv2_result_text"),
              plotlyOutput("ccv2_time_plot"),
              max_height = 450
)

# Steady state tab cards----

card9 <- card(card_header("First Derivative Plot (@NTP)", class = "text-success"),
              plotlyOutput("derivative_plot"),
              full_screen = TRUE, max_height = 450)

card10 <- card(card_header("First Derivative Data  (Times with 3+ consecutive zero first derivatives)", 
                           class = "text-success"),
                           tableOutput("derivative_table"),
                           full_screen = TRUE, max_height = 450)

# Raw data tab card----

card11 <- card(card_header("", class = "text-success"),
               tableOutput("raw_data_table"),
               full_screen = TRUE, max_height = 900)
             
# navbar ----

page_navbar(
  
  title = "OSHA Direct-Reading Data Analysis Application (Version 1.3e)",
  bg = "black",

  theme = bs_theme(version = 5, bootswatch = "darkly", font_scale =0.85, 
                   success = "#70b7e0", "table-color" = "#70b7e0", "card-color" = "#70b7e0", 
                   "nav_tabs_border_color" = "#70b7e0", "navbar-light-brand-color" = "#70b7e0 !important",
                   "navbar-light-brand-hover-color" = "#70b7e0 !important", "navbar-brand-font-size" = "1.8rem",
                   "border-color" = "#70b7e0", "input-border-color" = "#feb9b9"), 
  
  navset_tab(
    
    nav_panel(title = "Analysis",
              
              page_sidebar(
                
                sidebar = sidebar(

                  fileInput("file", "File:", multiple = FALSE,
                            accept = c("text/csv", "text/comma-separated-values,text/plain",
                                       ".csv")),
                  tags$style("#channel {background-color: white;}"),
                  selectInput("channel", "Channel:", 
                              choices = c("Channel_0","Channel_1","Channel_2","Channel_3","Channel_4","Channel_5","Channel_6"),
                              selectize = FALSE),  
                  
                  hr(style = "border-top: 2px solid #70b7e0"),
                  textInput("sample_number", "Sample Number:", value = ""),
                  dateInput("sample_start_date", "Start Date (yyyy-mm-dd):", value = "2000-01-01"),
                  timeInput("TWA_start_time", "Start Time (hh:mm:ss, 24hr):", value = "00:00:01"),
                  dateInput("sample_stop_date", "Stop Date (yyyy-mm-dd):", value = "2000-01-01"),
                  timeInput("TWA_stop_time", "Stop Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  numericInput("pressure", "Atm Pressure (mbar):", value = "", min = 500, max = 800),
                  numericInput("temperature", "Temperature (C):", value = "", min = 0, max = 50),
                  hr(style = "border-top: 2px solid #70b7e0"),
                  
                  downloadButton("TWA_report","Generate TWA Report")
                ),
                  layout_columns(
                    col_widths = c(7, 5, 7, 5),
                    card1, card3, card2, card4),
              ),
    ),

    nav_panel(title = "CCV",
              
              page_sidebar(
                
                sidebar = sidebar(

                  h4("Calibration:"),
                  timeInput("calibration_start_time", "Start Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  timeInput("calibration_stop_time", "Stop Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  
                  hr(style = "border-top: 2px solid #70b7e0"),
                  
                  h4("Pre Monitoring CCV:"),
                  timeInput("ccv1_start_time", "Start Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  timeInput("ccv1_stop_time", "Stop Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  textInput("ccv1", "CCV Value:", value = ""),
                  
                  hr(style = "border-top: 2px solid #70b7e0"),
                  
                  h4("Post Monitoring CCV:"),
                  timeInput("ccv2_start_time", "Start Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  timeInput("ccv2_stop_time", "Stop Time (hh:mm:ss, 24hr):", value = "00:00:00"),
                  textInput("ccv2", "CCV Value:", value = ""),
                ),
                layout_columns(
                  col_widths = c(6, 6, 6, 6),
                  card5, card6, card7, card8),
              ),
    ),
    
    nav_panel(title = "Steady State Search",
              
              page_sidebar(
                
                sidebar = sidebar(
                  
                  numericInput("ceiling_value", "Ceiling/IDLH Value:", value = "0", min = 0),
                  numericInput("SecDer_multiplication_factor", "First Derivative Multiplication Factor:", value = "1", min = 0),
                  hr(style = "border-top: 2px solid #70b7e0"),
                  timeInput("ceiling_start_time", "Start Time (h:m:s):", value = "00:00:00"),
                  timeInput("ceiling_stop_time", "Stop Time (h:m:s):", value = "00:00:00"),
                  hr(style = "border-top: 2px solid #70b7e0"),
                  downloadButton("steady_state_report","Generate Steady State Report")
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  card9, card10),
              ),
    ),
              
    nav_panel(title = "Raw Data",
              card11),
  )
)