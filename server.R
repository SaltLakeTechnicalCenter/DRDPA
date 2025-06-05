library("shiny")
library("tidyverse")
library("lubridate")
library("knitr")
library("plotly")
library("kableExtra")
library("data.table")

function(input, output, session) {

# Inputs------------------------------------------------------------------------

  # Loads Drager X-am 5600 or PAC .txt file into dataframe "df"
  df <- reactive ({
    inFile <-input$file
    req(input$file)
    
    #Remove any previous plots from file directory 
    file.remove(c('ccv1_time_plot.png', 'ccv2_time_plot.png',
                  'calibration_time_plot.png'))
    file.remove(c('sensor_data_plot.png', 'sampling_time_plot.png',
                  'second_derivative_plot.png'))
    
    names <- c("x1", "time", "Channel_0", "Channel_1", "Channel_2", "Channel_3", "Channel_4", "Channel_5", "Channel_6", "Ver")
    
    if (grepl("X-am", toString(inFile), fixed=TRUE)){
      #df <- read.delim(input$file$datapath, header = FALSE, sep =";", col.names = names)
      df <- fread(input$file$datapath, header = FALSE, sep = ";", col.names = names, fill = TRUE)
    } else if (grepl("PAC", toString(inFile), fixed=TRUE)){
      df <- fread(input$file$datapath, header = FALSE, sep = ";", col.names = names, fill = TRUE)
      df <- df %>% mutate(time =  as.character(as.POSIXct(time,format="%Y%m%d%H%M%S"))) %>% 
                   mutate(time = format(as_datetime(time), "%m/%d/%Y %I:%M:%S %p")) 
    }
  })
  
  # Sets selected input channel data to dataframe "data"
  data <- reactive ({df() %>% rename(concn = input$channel)
  })
  
  sample_number <- reactive ({
    input$sample_number
  }) 
  
  sample_start_date <- reactive ({
    as.POSIXlt(input$sample_start_date)
  }) 
  
  sample_stop_date <- reactive ({
    as.POSIXlt(input$sample_stop_date)
  }) 
  
  TWA_start_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_start_date()), input$TWA_start_time))
  }) 
  
  TWA_stop_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_stop_date()), input$TWA_stop_time))
  })
  
  pressure <- reactive ({
    round(input$pressure * 0.750062, digits = 2)
  }) 
  
  temperature <- reactive ({
    input$temperature
  }) 
  
  calibration_start_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_start_date()), input$calibration_start_time))
  }) 
  
  calibration_stop_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_start_date()), input$calibration_stop_time))
  })
  
  ccv1_start_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_start_date()), input$ccv1_start_time))
  }) 
  
  ccv1_stop_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_start_date()), input$ccv1_stop_time))
  })
  
  ccv2_start_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_stop_date()), input$ccv2_start_time))
  }) 
  
  ccv2_stop_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_stop_date()), input$ccv2_stop_time))
  })
  
  ccv1 <- reactive ({
    input$ccv1
  }) 
  
  ccv2 <- reactive ({
    input$ccv2
  }) 
  
  ceiling_value <- reactive ({
    input$ceiling_value
  }) 
  
  factor <- reactive ({
    input$SecDer_multiplication_factor
  }) 
  
  ceiling_start_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_start_date()), input$ceiling_start_time))
  }) 
  
  ceiling_stop_time <- reactive ({
    as.POSIXlt(sub("\\S+", as.character(sample_stop_date()), input$ceiling_stop_time))
  })
  
# Analyte Name of Selected Channel---------------------------------------------- 
  
  # Analyte name (e.g., CO, CO2, etc)
  analyte <- reactive ({
    analyte <- data() %>% 
      filter(x1 == "GASTYPE") %>% 
      select(concn)
    analyte <- analyte[[1]][1]
  }) 
  
  output$analyte <- renderText({
    paste("Analyte:", analyte())
  }) 
  
  analyte_1 <- reactive ({
    analyte_1 <- analyte()
  }) 
  
  output$analyte_1 <- renderText({
    paste("Analyte:", analyte_1())
  }) 
  
# Drager X-am Serial No and Monitor ID------------------------------------------  
  
  # Gets monitor serial number
  serial_number <- reactive ({
    serial_number <- df() %>% 
      filter(x1 == "SESSION_HEADER") %>% 
      select(Channel_4) 
    serial_number <- serial_number[[1]][1] 
  }) 
  
  # Gets monitor serial number
  monitor_id <- reactive ({
    monitor_id <- df() %>% 
      filter(x1 == "SESSION_HEADER") %>% 
      select(Channel_6) 
    monitor_id <- monitor_id[[1]][1] 
  }) 
  
# Sensor Event Log--------------------------------------------------------------
  
  event_data <- reactive({
    cal_data <- df() %>%  filter(x1 =="EVE") %>% 
      select(time, Channel_0)%>% 
      rename(Date_Time = time, Event = Channel_0)
  })
  
  output$event_text <- renderTable({
    event_data()
  }, hover = TRUE)
  
# Unit type of selected channel (e.g., ppm, %)----------------------------------
  
  unit_name <- reactive ({
    unit_name <- data() %>% 
      filter(x1 == "UNITNAME") %>% 
      select(concn)
    unit_name <- unit_name[[1]][1]
    if(unit_name == "Vol%") {
      unit_name <- "percent vol"
    } else {
      unit_name <- unit_name
    }
  }) 
 
# Sensor Data Plot and TWA Sampling Time Plot-----------------------------------
  
  # Sensor data for plotting (time, concn)
  plot_data <- reactive ({
    plot_data <- data() %>% 
      filter(x1 =="VAL") %>% 
      filter(concn != "INV") %>% 
      mutate(concn = ifelse(concn == "OVR", case_when(analyte() == "CO" ~ 2000,
                                                      analyte() == "co2" ~ 5,
                                                      analyte() == "NO" ~ 200,
                                                      analyte() == "Cl2" ~ 20,
                                                      analyte() == "H2S" ~ 200), concn)) %>% 
      filter(!is.numeric(concn)) %>% 
      mutate(concn = as.numeric(as.character(concn))) %>% 
      mutate(time = as.POSIXct(time,tryFormats = c("%Y%m%d%H%M%S", "%m/%d/%Y %I:%M:%S %p"))) %>% 
      select(time,concn)
  }) 
  
  # Plot of all sensor data
  sensor_data_plot <- reactive ({
    sensor_data_plot <- ggplot(plot_data(), aes(x=time, y=concn, group = 1)) +
      geom_line(color="blue") +
      theme_bw() +
      ylab(paste0("Concentration (", unit_name(),")")) + xlab("Time") + 
      geom_vline(xintercept = as.POSIXct(TWA_start_time()), color = "red", alpha=1) +
      geom_vline(xintercept = as.POSIXct(TWA_stop_time()), color = "red", alpha=1) +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red", hjust = 0.5 ))
    
  })
  
  # Sampling time data for plotting (time, concn)
  sampling_time_plot_data <- reactive ({
    sampling_time_plot_data <- plot_data() %>% 
      filter(time >= TWA_start_time() & time <= TWA_stop_time()) %>%        
      mutate(concn = replace(concn, concn < 0, 0))

  })  
  
  #Plot of sampling time data
  sampling_time_plot <- reactive ({
    sampling_time_plot <- ggplot(sampling_time_plot_data(), aes(x=time, y=concn, group = 1)) +
      geom_line(color="blue") +
      theme_bw() +
      ylab(paste0("Concentration (", unit_name(),")")) + xlab("Time") +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red", hjust = 0.5 ))
    
  }) 
  
# Save and Output Senor Data Plot & TWA Plot------------------------------------
  
  output$sensor_data_plot <- renderPlotly({
    ggsave("sensor_data_plot.png", sensor_data_plot(), height = 7, width = 18, units = "cm")
    sensor_data_plot <- ggplotly(sensor_data_plot())
    sensor_data_plot()
  })
  
  output$sampling_time_plot <- renderPlotly({
    ggsave("sampling_time_plot.png", sampling_time_plot(), height = 7, width = 18, units = "cm")
    sampling_time_plot <- ggplotly(sampling_time_plot()) 
    sampling_time_plot
  })
  
# Calibration and QC Plots------------------------------------------------------
  
  # Sampling time data for calibration plot (time, concn)
  calibration_time_plot_data <- reactive ({
    calibration_time_plot_data <- plot_data() %>% 
      filter(time >= calibration_start_time() & time <= calibration_stop_time())
  })  
  
  #Plot of calibration data
  calibration_time_plot <- reactive ({
    calibration_time_plot <- ggplot(calibration_time_plot_data(), aes(x=time, y=concn, group = 1)) +
      geom_line(color="blue") +
      theme_bw() +
      scale_x_datetime(date_labels="%I:%M:%S")+
      ylab(paste0("Concentration (", unit_name(),")")) + xlab("Time") +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red", hjust = 0.5 ))
    
  })
  
  # Sampling time data for ccv1 (time, concn)
  ccv1_time_plot_data <- reactive ({
    ccv1_time_plot_data <- plot_data() %>% 
      filter(time >= ccv1_start_time() & time <= ccv1_stop_time())
  })  
  
  #Plot of ccv1
  ccv1_time_plot <- reactive ({
    ccv1_time_plot <- ggplot(ccv1_time_plot_data(), aes(x=time, y=concn, group = 1)) +
      geom_line(color="blue") +
      theme_bw() +
      scale_x_datetime(date_labels="%I:%M:%S")+
      ylab(paste0("Concentration (", unit_name(),")")) + xlab("Time") +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red", hjust = 0.5 ))
    
  })
  
  # Sampling time data for ccv2 (time, concn)
  ccv2_time_plot_data <- reactive ({
    ccv2_time_plot_data <- plot_data() %>% 
      filter(time >= ccv2_start_time() & time <= ccv2_stop_time())
  })  
  
  #Plot of ccv2
  ccv2_time_plot <- reactive ({
    ccv2_time_plot <- ggplot(ccv2_time_plot_data(), aes(x=time, y=concn, group = 1)) +
      geom_line(color="blue") +
      theme_bw() +
      scale_x_datetime(date_labels="%I:%M:%S")+
      ylab(paste0("Concentration (", unit_name(),")")) + xlab("Time") +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red", hjust = 0.5 ))
    
  })
  
# Save and Output QC Plots------------------------------------------------------  
  
  output$qc_sensor_data_plot <- renderPlotly({
    ggsave("sensor_data_plot.png", sensor_data_plot(), height = 7, width = 18, units = "cm")
    sensor_data_plot <- ggplotly(sensor_data_plot())
  })
  
  output$calibration_time_plot <- renderPlotly({
    ggsave("calibration_time_plot.png", calibration_time_plot(), height = 7, width = 18, units = "cm")
    calibration_time_plot <- ggplotly(calibration_time_plot()) 
    calibration_time_plot
  })
  
  output$ccv1_time_plot <- renderPlotly({
    ggsave("ccv1_time_plot.png", ccv1_time_plot(), height = 7, width = 18, units = "cm")
    ccv1_time_plot <- ggplotly(ccv1_time_plot()) 
    ccv1_time_plot
  })
  
  output$ccv2_time_plot <- renderPlotly({
    ggsave("ccv2_time_plot.png", ccv2_time_plot(), height = 7, width = 18, units = "cm")
    ccv2_time_plot <- ggplotly(ccv2_time_plot()) 
    ccv2_time_plot
  })
  
# First Derivative Calculations -----------------------------------------------
  
  # Calculate First Derivatives for Selected Ceiling Stop/Start Times
  derivative <- reactive ({
    derivative <- plot_data() %>% 
      mutate(first_der = replace_na((lag(concn,2) - 8*lag(concn,1) + 8*lead(concn,1) - lead(concn,2))/12), replace = 0) %>% 
      select(time,concn, first_der) %>% 
      filter(as.POSIXct(time,tryFormats = c("%Y%m%d%H%M%S", "%m/%d/%Y %I:%M:%S %p")) >= ceiling_start_time() & as.POSIXct(time,tryFormats = c("%Y%m%d%H%M%S", "%m/%d/%Y %I:%M:%S %p")) <= ceiling_stop_time()) %>% 
      mutate(NTP_concn = concn * (pressure()/760) *(298.15/(273.15 + temperature())))
  }) 
  
  # Finds times with at least 3 first derivatives in a row that are above ceiling value
  zero_derivative <- reactive ({
    zero_derivative <- derivative() %>% 
      filter(first_der == 0 & NTP_concn >= ceiling_value()) %>% 
      mutate(sample_time = as.POSIXct(time,tryFormats = c("%Y%m%d%H%M%S", "%m/%d/%Y %I:%M:%S %p"))) %>%  
      mutate(three_d_inrow = case_when((lag(sample_time) +1) == sample_time & (lead(sample_time) -1) == sample_time ~ 3,
                                           (lag(sample_time) +1) == sample_time & (lag(sample_time,2) +2) == sample_time ~ 3,
                                           (lead(sample_time) -1) == sample_time & (lead(sample_time,2) -2) == sample_time ~ 3
      )) %>% 
      filter(three_d_inrow == 3) %>%  
      select(time,concn, NTP_concn, first_der)
  })
  
# Save and Output first Derivative Plot and Table------------------------------
  
  # Data for plotting 
  derivative_plot_data <- reactive ({
    derivative_plot_data <- derivative() %>% 
      mutate(time = as.POSIXct(time,tryFormats = c("%Y%m%d%H%M%S", "%m/%d/%Y %I:%M:%S %p")))  
  })  
  
  # First derivative Plot
  derivative_plot <- reactive ({
    derivative_plot <- ggplot(derivative_plot_data(), aes(x=time)) +
      geom_line(aes(y = NTP_concn), color="blue") +
      geom_line(aes(y = (first_der * factor())), color="orange") +
      theme_bw() +
      ylab(paste0("Concentration (", unit_name(),")")) + xlab("Time") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
    
  }) 
  
  # Save and output first derivative plot
  output$derivative_plot <- renderPlotly({
    ggsave("derivative_plot.png", derivative_plot(), height = 7, width = 18, units = "cm")
    derivative_plot <- ggplotly(derivative_plot())
    derivative_plot
  })

# Raw Data Output---------------------------------------------------------------
  
  # Raw data output
  output$raw_data_table <- renderTable({
    df()
  }, hover = TRUE, fixedHeader=TRUE)
  
# Calculations -----------------------------------------------------------------
  
  # TWA concentration
  mean_concn  <- reactive ({
    round(mean((sampling_time_plot_data())$concn, na.rm = TRUE), digits = 4)
  }) 
  
  min_concn  <- reactive ({
    min((sampling_time_plot_data())$concn, na.rm = TRUE)
  }) 
  
  max_concn  <- reactive ({
    max((sampling_time_plot_data())$concn, na.rm = TRUE)
  }) 
  
  sd_concn  <- reactive ({
    round(sd((sampling_time_plot_data())$concn, na.rm = TRUE), digits = 4)
  }) 
  
  cv_concn  <- reactive ({
    round((((sd_concn()/mean_concn())*100)), digits = 1)
  })   
  
  # TWA total time
  total_time  <- reactive ({
    sampling_time_data_points()/60
  }) 
  
  # NTP TWA concentration
  corrected_concn  <- reactive ({
    corrected_concn <- round(mean_concn() * (pressure()/760) *(298.15/(273.15 + temperature())), digits = 4)
  })   
  
  # NTP Peak concentration
  corrected_peak_concn  <- reactive ({
    corrected_peak_concn <- round(max_concn()  * (pressure()/760) *(298.15/(273.15 + temperature())), digits = 4)
  })
  
  # Number of data pints collected during sampling for TWA
  sampling_time_data_points  <- reactive ({
    sampling_time_data_points <- nrow(sampling_time_plot_data()-1)
  })
  
  # Ceiling/IDLH NTP TWA Concentration
  ceiling_concn_NTP  <- reactive ({
    ceiling_concn_NTP <- round(mean((derivative())$NTP_concn, na.rm = TRUE), digits = 4)
  })
  
  # CCV Results

  ccv1_max_result  <- reactive ({
    ccv1_max_result<- round(max((ccv1_time_plot_data())$concn, na.rm = TRUE), digits = 4)
  })
  
  ccv2_max_result  <- reactive ({
    ccv2_max_result<- round(max((ccv2_time_plot_data())$concn, na.rm = TRUE), digits = 4)
  })
 
# User Interface Result Outputs-------------------------------------------------
  
  # Site Result
  output$site_result_text <- renderText({
    paste("The TWA concn is", mean_concn(),  
          unit_name(), analyte(), "at", pressure(), "mmHg", "and", temperature(), "C.")
  })
  
  # NTP Result
  output$NTP_result_text <- renderText({
    paste("The TWA concn is", corrected_concn(),  
          unit_name(), analyte(), "at 760 mmHg and 25 C.")
  })
  
  # First derivative data table
  output$derivative_table <- renderTable({
    zero_derivative <- zero_derivative() %>% mutate(time = as.character(time))
    zero_derivative
  })
  
  # First derivative NTP TWA data output
  output$ceiling_TWA <- renderText({
    paste("TWA concn is", ceiling_concn_NTP(), 
          unit_name(), analyte(), "at 760 mmHg and 25 C.")
  })
  
  # CCV Results
  output$ccv1_result_text <- renderText({
    paste("Maximum CCV reading is", ccv1_max_result(),unit_name())
  })
  
  # CCV Results
  output$ccv2_result_text <- renderText({
    paste("Maximum CCV reading is", ccv2_max_result(), unit_name())
  })
  
# Report Generation -``---------------------------------------------------------

  # https://gist.github.com/yihui/6091942
  
  # TWA report
  output$TWA_report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('TWAreportTemplate.Rnw', clean = TRUE)
      file.rename(out, paste(sample_number(), analyte(), "TWA", serial_number(), Sys.Date(),".pdf")) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )
  
  # Ceiling/IDLH report
  output$steady_state_report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('SteadyStateTemplate.Rnw', clean = TRUE)
      file.rename(out, paste(sample_number(), analyte(), "Steady_State", serial_number(), Sys.Date(),".pdf")) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )  
  
}
