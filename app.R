rm(list = ls())

library(dplyr)
library(DT)
library(ggplot2)
library(ggsci)
library(knitr)
library(rpact)
library(shiny)
library(shinyvalidate)
library(stringr)
library(tidyr)




### SUPPLEMENTARY FUNCTIONS ####################################################
lapply(
  X = c("app_parameters.R",
        "graphics_parameters.R",
        "shiny_input_validation.R",
        "precision_adaptive_planning.R"),
  FUN = source
)




## USER INTERFACE ##############################################################
ui <- 
  shiny::fluidPage(
    title = "Sample Size: Precision Adaptive Trials",
    lang = "en",
    # Application title
    shiny::titlePanel(
      title = 
        shiny::HTML(text = "<b>PICARD</b>: Planning Information-monitored Covariate Adjusted RCT Designs")
    ),
    
    
    shiny::sidebarLayout(
      sidebarPanel = 
        shiny::sidebarPanel(
          width = 4,
          shiny::selectInput(
            inputId = "outcome_type",
            label = "Outcome Type:",
            # choices = c("Continuous", "Binary", "Ordinal", "Time-to-Event"),
            choices = c("Continuous", "Binary", "Ordinal",
                        "Time-to-Event (Not Yet Implemented)"),
            selected = c("Continuous"),
            multiple = FALSE
          ),
          
          
          
          
          #### Continuous Parameter Interface ####################################
          shiny::uiOutput(
            outputId = "ui_cts_dim"
          ),
          
          shiny::uiOutput(
            outputId = "ui_cts_sd_0"
          ),
          
          shiny::uiOutput(
            outputId = "ui_cts_sd_1"
          ),
          
          
          
          
          #### Binary Parameter Interface ########################################
          shiny::uiOutput(
            outputId = "ui_bin_dim"
          ),
          
          shiny::uiOutput(
            outputId = "ui_bin_p0"
          ),
          
          
          
          
          #### Ordinal Parameter Interface #######################################
          shiny::uiOutput(
            outputId = "ui_ord_input_type"
          ),
          
          shiny::uiOutput(
            outputId = "ui_ord_input_1_parameters"
          ),
          
          shiny::uiOutput(
            outputId = "ui_ord_input_2_parameters"
          ),
          
          
          
          
          #### Covaraite Adjustment Interface ####################################
          shiny::uiOutput(
            outputId = "precision_gain"
          ),
          
          
          
          
          #### Design Parameter Interface ########################################
          shiny::uiOutput(
            outputId = "ui_gsd_alpha_spending"
          ),
          
          shiny::uiOutput(
            outputId = "ui_gsd_alpha_spending_params"
          ),
          
          shiny::uiOutput(
            outputId = "ui_gsd_beta_spending"
          ),
          
          shiny::uiOutput(
            outputId = "ui_gsd_beta_spending_params"
          ),
          
          shiny::uiOutput(
            outputId = "ui_information_fractions"
          ),
          
          shiny::numericInput(
            inputId = "power",
            label = "Power",
            value = 0.9
          ),
          
          shiny::numericInput(
            inputId = "alpha",
            label = "Type I Error Rate:",
            value = 0.05
          ),
          
          shiny::selectInput(
            inputId = "test_type",
            label = "Test Type:",
            choices = c("1-Sided", "2-Sided"),
            selected = "2-Sided"
          ),
          
          shiny::uiOutput(
            outputId = "run_button"
          )
        ),
      
      
      mainPanel = 
        shiny::mainPanel(
          width = 8,
          shiny::tabsetPanel(
            id = "tabset",
            
            tabPanel(
              title = "Design Input",
              value = "Input",
              shiny::h3("Study Design"),
              shiny::textOutput(
                outputId = "ui_input_string"
              ),
              shiny::hr(),
              shiny::h3("Trial Scenarios"),
              DT::DTOutput(
                outputId = "table_scenarios_input"
              )
            ),
            
            tabPanel(
              title = "Instructions",
              value = "Instructions",
              shiny::withMathJax(
                shiny::includeHTML(
                  path = file.path("Instructions.html")
                ) 
              ),
            ),
            
            
            tabPanel(
              title = "Background",
              value = "Background",
              shiny::withMathJax(
                shiny::includeHTML(
                  path = file.path("Information_Adaptive_Designs.html")
                ) 
              ),
            ),
            
            shiny::tabPanel(
              title = "Information vs. Sample Size",
              value = "Plot",
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::sliderInput(
                    inputId = "info_plot_height",
                    label = "Plot Height:",
                    min = 200,
                    max = 1200,
                    value = 600
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::sliderInput(
                    inputId = "info_plot_width",
                    label = "Plot Height:",
                    min = 200,
                    max = 1200,
                    value = 600
                  ) 
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  DT::DTOutput(
                    outputId = "table_scenarios_plot"
                  )
                )
              ),
              shiny::hr(),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  shiny::plotOutput(
                    outputId = "plot_information_by_n"
                  )
                )
              )
            ),
            
            
            shiny::tabPanel(
              title = "Table: Analysis Times",
              value = "Table",
              DT::DTOutput(
                outputId = "table_analyses"
              )
            )
          )
        )
    )
  )




## SERVER LOGIC ################################################################
server <- 
  function(input, output) {
    ### USER INPUT #############################################################
    
    #### Continuous User Input ##################################################
    output$ui_cts_dim <-
      shiny::renderUI({
        if(input$outcome_type == "Continuous"){
          shiny::textInput(
            inputId = "cts_dim",
            label = "Difference in Means:",
            value = "",
            placeholder = "Enter non-zero values separated by commas."
          )
        }
      })
    
    
    output$ui_cts_sd_0 <-
      shiny::renderUI({
        if(input$outcome_type == "Continuous"){
          shiny::textInput(
            inputId = "cts_sd_0",
            label = "SD of Outcome: Controls",
            value = "",
            placeholder = "Enter positive values separated by commas."
          )
        }
      })
    
    
    output$ui_cts_sd_1 <-
      shiny::renderUI({
        if(input$outcome_type == "Continuous"){
          shiny::textInput(
            inputId = "cts_sd_1",
            label = "SD of Outcome: Treatment",
            value = "",
            placeholder = "Enter positive values separated by commas."
          )
        }
      })
    
    
    
    
    #### Binary User Input #####################################################
    output$ui_bin_dim <-
      shiny::renderUI({
        if(input$outcome_type == "Binary"){
          shiny::textInput(
            inputId = "bin_dim",
            label = "Difference: Treatment - Control:",
            value = "",
            placeholder = "Enter values in (-1, 1) separated by commas."
          )
        }
      })
    
    
    output$ui_bin_p0 <-
      shiny::renderUI({
        if(input$outcome_type == "Binary"){
          shiny::textInput(
            inputId = "bin_p0",
            label = "Proportions: Control",
            value = "",
            placeholder = "Enter probabilities separated by commas."
          )
        }
      })
    
    
    
    
    #### Ordinal User Input ####################################################
    output$ui_ord_input_type <-
      shiny::renderUI({
        if(input$outcome_type %in% "Ordinal"){
          selectInput(
            inputId = "ord_input_type",
            label = "Input Type:",
            choices = c("PMF of Outcomes", "Mann-Whitney"),
            selected = "PMF of Outcomes",
            multiple = FALSE
          )
        }
      })
    
    
    output$ui_ord_input_1_parameters <-
      shiny::renderUI({
        if(input$outcome_type == "Ordinal" & length(input$ord_input_type) > 0){
          if(input$ord_input_type == "PMF of Outcomes"){
            shiny::textInput(
              inputId = "ord_p0",
              label = "PMF: Control",
              value = "",
              placeholder = "Enter probabilities separated by commas."
            )
          } else if(input$ord_input_type == "Mann-Whitney"){
            shiny::textInput(
              inputId = "ord_mw",
              label = "Mann-Whitney:",
              value = "",
              placeholder = "Enter probabilities separated by commas."
            )
          }
        }
      })
    
    output$ui_ord_input_2_parameters <-
      shiny::renderUI({
        if(input$outcome_type == "Ordinal" & length(input$ord_input_type) > 0){
          if(input$ord_input_type == "PMF of Outcomes"){
            shiny::textInput(
              inputId = "ord_p1",
              label = "PMF: Treatment",
              value = "",
              placeholder = "Enter probabilities separated by commas."
            )
          }
        }
      })
    
    
    
    
    #### Run Button ############################################################
    output$run_button <-
      shiny::renderUI({
        shiny::actionButton(
          inputId = "run_analysis",
          label = "Make It So"
        )
      })
    
    
    
    
    #### Covariate Adjustment User Input #######################################
    output$precision_gain <-
      shiny::renderUI({
        shiny::textInput(
          inputId = "precision_gain",
          label = "Precision Increase (%)",
          value = "",
          placeholder = "Enter percentages separated by commas."
        )
      })
    
    
    
    
    #### Sequential Design User Input ##########################################
    output$ui_gsd_alpha_spending <-
      shiny::renderUI({
        shiny::selectInput(
          inputId = "gsd_alpha_spending",
          label = "Efficacy Boundaries:",
          choices = c("Fixed Sample Size", "O'Brien-Fleming (OBF)",
                      "Pocock", "Kim-DeMets", "Hwang-Shi-DeCani"),
          multiple = FALSE
        )
      })
    
    output$ui_gsd_alpha_spending_params <-
      shiny::renderUI({
        
        shiny::req(
          input$gsd_alpha_spending %in% c("Kim-DeMets", "Hwang-Shi-DeCani")
        )
        
        shiny::numericInput(
          inputId = "design_gamma_a",
          label = "Alpha Spending Parameter:",
          value = 1
        )
      })
    
    
    
    
    output$ui_gsd_beta_spending <-
      shiny::renderUI({
        
        shiny::req(
          !(input$gsd_alpha_spending %in% c("Fixed Sample Size"))
        )
        
        shiny::selectInput(
          inputId = "gsd_beta_spending",
          label = "Futility Boundaries:",
          choices = c("None", "O'Brien-Fleming (OBF)",
                      "Pocock", "Kim-DeMets", "Hwang-Shi-DeCani"),
          multiple = FALSE
        )
      })
    
    
    
    
    output$ui_gsd_beta_spending_params <-
      shiny::renderUI({
        
        shiny::req(
          input$gsd_beta_spending %in% c("Kim-DeMets", "Hwang-Shi-DeCani")
        )
        
        shiny::numericInput(
          inputId = "design_gamma_b",
          label = "Beta Spending Parameter:",
          value = 1
        )
      })
    
    
    
    
    output$ui_information_fractions <-
      shiny::renderUI({
        
        shiny::req(
          input$gsd_alpha_spending != "Fixed Sample Size"
        )
        
        shiny::textInput(
          inputId = "design_info_fraction",
          label = "Information: Interim Analyses",
          value = "",
          placeholder = "Enter proportions, separated by commas"
        )
        
      })
    
    
    
    
    ### INPUT VALIDATION ########################################################
    
    #### Outcome Parameter Validation ##########################################
    cts_validation <-
      shinyvalidate::InputValidator$new()
    
    
    cts_validation$add_rule(
      inputId = "cts_dim",
      rule = 
        function(x) 
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                is_unique,
                (function(.x, values = 0)
                  exclude_values (x = .x, exclude = values))
              )
          )
    )
    
    
    
    
    cts_validation$add_rule(
      inputId = "cts_sd_0",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x)
                  in_range(x = .x, range_min = 0, range_max = Inf,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                is_unique
              )
          )
    )
    
    
    
    
    cts_validation$add_rule(
      inputId = "cts_sd_1",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x)
                  in_range(x = .x, range_min = 0, range_max = Inf,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                is_unique
              )
          )
    )
    
    
    
    
    bin_validation <-
      shinyvalidate::InputValidator$new()
    
    
    bin_validation$add_rule(
      inputId = "bin_dim",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x)
                  in_range(x = .x, range_min = -1, range_max = 1,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                is_unique,
                (function(.x, values = 0)
                  exclude_values (x = .x, exclude = values))
              )
          )
    )
    
    
    bin_validation$add_rule(
      inputId = "bin_p0",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x)
                  in_range(x = .x, range_min = 0, range_max = 1,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                is_unique
              )
          )
    )
    
    
    
    
    ord_mw_validation <-
      shinyvalidate::InputValidator$new()
    
    
    ord_mw_validation$add_rule(
      inputId = "ord_mw",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                (function(.x)
                  in_range(x = .x, range_min = 0, range_max = 1,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, values = 0.5)
                  exclude_values(
                    x = .x, exclude = values,
                    message = "For Mann-Whitney, 0.5 is the null value."
                  )
                )
              )
          )
    )
    
    
    
    
    ord_pmf_validation <-
      shinyvalidate::InputValidator$new()
    
    
    ord_pmf_validation$add_rule(
      inputId = "ord_p0",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x, length_max = 7)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                (function(.x)
                  in_range(x = .x, range_min = 0, range_max = 1,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, target_sum = 1)
                  sums_to(x = .x, target_sum = target_sum))
              )
          )
    )
    
    
    
    
    ord_pmf_validation$add_rule(
      inputId = "ord_p1",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x, length_max = 7)
                  length_range(x = .x, length_min = 0, length_max = length_max)),
                (function(.x)
                  in_range(x = .x, range_min = 0, range_max = 1,
                           include_lower = FALSE, include_upper = FALSE)),
                (function(.x, target_sum = 1)
                  sums_to(x = .x, target_sum = target_sum))
              )
          )
    )
    
    
    
    
    #### Covariate Adjustment Parameter Validation #############################
    precision_validation <-
      shinyvalidate::InputValidator$new()
    
    precision_validation$add_rule(
      inputId = "precision_gain",
      rule =
        function(x)
          check_delimited_text_input(
            x = x,
            delimiter = text_delimiter,
            check_functions =
              list(
                (function(.x, length_max = max_parameters)
                  length_range(x = .x, length_min = 1, length_max = length_max)),
                (function(.x)
                  in_range(x = .x, range_min = -10, range_max = 50,
                           include_lower = TRUE, include_upper = TRUE)),
                is_unique,
                (function(.x, values = 0)
                  exclude_values(
                    x = .x, exclude = values,
                    message = "The value 0 is included by default."
                  )
                )
              )
          )
    )
    
    
    
    
    #### Study Design Parameter Validation #####################################
    gsd_validation <-
      shinyvalidate::InputValidator$new()
    
    
    gsd_validation$add_rule(
      inputId = "design_info_fraction",
      rule =
        function(x) {
          if(is.null(input$gsd_alpha_spending)){
            NULL
          } else if(input$gsd_alpha_spending != "Fixed Sample Size"){
            check_delimited_text_input(
              x = x,
              delimiter = text_delimiter,
              check_functions =
                list(
                  (function(.x, length_max = max_interim_analyses)
                    length_range(x = .x, length_min = 1, length_max = length_max)),
                  (function(.x)
                    in_range(x = .x, 
                             range_min = info_level_range[1], 
                             range_max = info_level_range[2],
                             include_lower = FALSE, include_upper = FALSE)),
                  is_unique,
                  (function(.x)
                    limit_increment(
                      x = .x,
                      lower_bound = lowest_info_increment,
                      sort_first = TRUE,
                      take_abs = TRUE
                    )
                  )
                )
            )
          } else {
            NULL
          }
        }
    )
    
    
    gsd_validation$add_rule(
      inputId = "power",
      rule =
        function(x)
          in_range(x = x,
                   range_min = power_range[1], range_max = power_range[2],
                   include_lower = power_include_min,
                   include_upper = power_include_max)
    )
    
    
    gsd_validation$add_rule(
      inputId = "alpha",
      rule =
        function(x)
          in_range(x = x, range_min = alpha_range[1], range_max = alpha_range[2],
                   include_lower = alpha_include_min,
                   include_upper = alpha_include_max)
    )
    
    
    gsd_validation$add_rule(
      inputId = "design_gamma_a",
      rule =
        function(x){
          if(is.null(input$gsd_alpha_spending)){
            NULL
          } else if(input$gsd_alpha_spending %in% 
                    c("Kim-DeMets", "Hwang-Shi-DeCani")){
            in_range(
              x = x, 
              range_min = 
                ifelse(
                  test = input$gsd_alpha_spending == "Kim-DeMets",
                  yes = kd_range[1],
                  no = hsd_range[1]
                ),
              range_max = 
                ifelse(
                  test = input$gsd_alpha_spending == "Kim-DeMets",
                  yes = kd_range[2],
                  no = hsd_range[2]
                ),
              include_lower = TRUE,
              include_upper = TRUE
            )
          } else{
            NULL
          }
        }
    )
    
    
    gsd_validation$add_rule(
      inputId = "design_gamma_b",
      rule =
        function(x){
          if(is.null(input$gsd_beta_spending)){
            NULL
          } else if(input$gsd_beta_spending %in% 
                    c("Kim-DeMets", "Hwang-Shi-DeCani")){
            in_range(
              x = x, 
              range_min = 
                ifelse(
                  test = input$gsd_beta_spending == "Kim-DeMets",
                  yes = kd_range[1],
                  no = hsd_range[1]
                ),
              range_max = 
                ifelse(
                  test = input$gsd_beta_spending == "Kim-DeMets",
                  yes = kd_range[2],
                  no = hsd_range[2]
                ),
              include_lower = TRUE,
              include_upper = TRUE
            )
          } else{
            NULL
          }
        }
    )
    
    
    
    
    # Enable all rules
    cts_validation$enable()
    bin_validation$enable()
    ord_mw_validation$enable()
    ord_pmf_validation$enable()
    precision_validation$enable()
    gsd_validation$enable()
    
    
    
    
    ### REACTIVE VALUES ########################################################
    
    #### Reactive - Continuous #################################################
    cts_dim <-
      shiny::reactive({
        shiny::req(
          cts_validation$is_valid()
        )
        parse_delimited_text_to_numeric(
          x = input$cts_dim, delimiter = text_delimiter
        )
      })
    
    cts_sd_0 <-
      shiny::reactive({
        shiny::req(
          cts_validation$is_valid(),
        )
        parse_delimited_text_to_numeric(
          x = input$cts_sd_0, delimiter = text_delimiter
        )
      })
    
    cts_sd_1 <-
      shiny::reactive({
        shiny::req(
          cts_validation$is_valid(),
        )
        parse_delimited_text_to_numeric(
          x = input$cts_sd_1, delimiter = text_delimiter
        )
      })
    
    
    
    
    #### Reactive - Binary #####################################################
    bin_dim <-
      shiny::reactive({
        shiny::req(
          bin_validation$is_valid(),
        )
        parse_delimited_text_to_numeric(
          x = input$bin_dim, delimiter = text_delimiter
        )
      })
    
    bin_p0 <-
      shiny::reactive({
        shiny::req(
          bin_validation$is_valid(),
        )
        parse_delimited_text_to_numeric(
          x = input$bin_p0, delimiter = text_delimiter
        )
      })
    
    bin_p1 <-
      shiny::reactive({
        if(length(bin_dim() > 0) & length(bin_p0()) > 0){
          p1 <- bin_dim() + bin_p0()
          shiny::validate(
            shiny::need(
              expr = all(p1 > 0),
              message =
                paste0(
                  "Treatment group event probability must be greater than 1. ",
                  "Currently: ", p1
                )
            ),
            shiny::need(
              expr = all(p1 < 1), 
              message = 
                paste0(
                  "Treatment group event probability must be less than 1. ",
                  "Currently: ", p1
                )
            )
          )
          return(p1)
        } else {
          NULL
        }
      })
    
    
    
    
    #### Reactive - Ordinal ####################################################
    ord_p0 <- reactive({
      shiny::req(
        ord_pmf_validation$is_valid()
      )
      
      
      parse_delimited_text_to_numeric(
        x = input$ord_p0, delimiter = text_delimiter
      )
    })
    
    
    ord_p1 <- reactive({
      shiny::req(
        ord_pmf_validation$is_valid()
      )
      
      
      parse_delimited_text_to_numeric(
        x = input$ord_p1, delimiter = text_delimiter
      )
    })
    
    
    ord_mw <-
      shiny::reactive({
        shiny::req(
          ord_pmf_validation$is_valid(),
          ord_mw_validation$is_valid()
        )
        if(is.null(input$ord_input_type)){
          return(NULL)
        } else if(input$ord_input_type == "PMF of Outcomes"){
          if(length(ord_p0()) > 1 & length(ord_p1()) > 1){
            mw_from_pmfs(
              pmf_1 = ord_p1(),
              pmf_0 = ord_p0()
            )
          } else {
            return(NULL)
          }
        } else if (input$ord_input_type == "Mann-Whitney") {
          mw <-
            parse_delimited_text_to_numeric(
              x = input$ord_mw, delimiter = text_delimiter
            )
          if(length(mw) > 0){
            return(mw)
          } else {
            return(NULL)
          }
        }
      })
    
    
    
    
    #### Reactive - Precision Gain #############################################
    precision_gain <-
      shiny::reactive({
        shiny::req(
          precision_validation$is_valid(),
        )
        
        
        precision_gain_pct <- 
          parse_delimited_text_to_numeric(
            x = input$precision_gain, delimiter = text_delimiter
          )
        
        
        if(length(precision_gain_pct) == 0){
          return(0)
        } else {
          return(sort(c(0, precision_gain_pct))) 
        }
      })
    
    
    precision_gain_proportion <-
      shiny::reactive({
        1 + precision_gain()/100
      })
    
    
    
    
    #### Reactive - Study Design Parameters ####################################
    design_info_fraction <-
      shiny::reactive({
        shiny::req(
          gsd_validation$is_valid(),
        )
        info_fractions <- 
          parse_delimited_text_to_numeric(
            x = input$design_info_fraction, delimiter = text_delimiter
          ) |> 
          sort()
        
        if(length(info_fractions) == 0){
          return(1)
        } else {
          return(c(info_fractions, 1))
        }
      })
    
    
    design_stages <-
      shiny::reactive({
        if(input$gsd_alpha_spending == "Fixed Sample Size") {
          return(1)
        } else {
          total_analyses <- length(design_info_fraction())
          if(total_analyses > 0){
            return(total_analyses)
          } else {
            return(NULL)
          }
        }
      })
    
    
    design_label <-
      shiny::reactive({
        if(input$gsd_alpha_spending == "Fixed Sample Size") {
          return(NULL)
        } else {
          return(
            switch(
              EXPR = input$gsd_alpha_spending,
              "O'Brien-Fleming (OBF)" = "asOF",
              "Pocock" = "asP",
              "Kim-DeMets" = "asKD",
              "Hwang-Shi-DeCani" = "asHSD",
              NA
            )
          )
        }
      })
    
    
    
    type_beta_spending <-
      shiny::reactive({
        
        shiny::req(
          gsd_validation$is_valid(),
        )
        
        if(input$gsd_alpha_spending == "Fixed Sample Size") {
          return(NULL)
        } else {
          return(
            switch(
              EXPR = input$gsd_beta_spending,
              "O'Brien-Fleming (OBF)" = "bsOF",
              "Pocock" = "bsP",
              "Kim-DeMets" = "bsKD",
              "Hwang-Shi-DeCani" = "bsHSD",
              NA
            )
          )
        }
      })
    
    
    
    test_type <-
      shiny::reactive({
        if(input$test_type == "1-Sided"){
          return(1)
        } else if(input$test_type == "2-Sided"){
          return(2)
        } else{
          return(NULL)
        }
      })
    
    
    alpha_spending_parameter <-
      shiny::reactive({
        if(is.numeric(input$design_gamma_a)) {
          input$design_gamma_a
        } else {
          return(NA_real_)
        }
      })
    
    
    beta_spending_parameter <-
      shiny::reactive({
        if(is.numeric(input$design_gamma_b)) {
          input$design_gamma_b
        } else {
          return(NA_real_)
        }
      })
    
    
    study_design <-
      shiny::reactive({
        shiny::req(
          gsd_validation$is_valid()
        )
        
        if(input$gsd_alpha_spending %in%
           c("Fixed Sample Size", "O'Brien-Fleming (OBF)", "Pocock") |
           ifelse(
             test = is.null(alpha_spending_parameter()),
             yes = TRUE,
             no = !is.finite(alpha_spending_parameter())
           )
        ){
          gamma_a <- NA_real_
        } else {
          gamma_a <- alpha_spending_parameter()
        }
        
        if(is.null(input$gsd_beta_spending)){
          gamma_b <- NA_real_
        } else {
          if(input$gsd_beta_spending %in%
             c("None", "O'Brien-Fleming (OBF)", "Pocock") |
             ifelse(
               test = is.null(beta_spending_parameter()),
               yes = TRUE,
               no = !is.finite(beta_spending_parameter())
             )
          ){
            gamma_b <- NA_real_
          } else {
            gamma_b <- alpha_spending_parameter()
          }
        }
        
        rpact::getDesignGroupSequential(
          sided = test_type(),
          alpha = input$alpha,
          beta = 1 - input$power,
          informationRates = design_info_fraction(),
          typeOfDesign = design_label(),
          typeBetaSpending = type_beta_spending(),
          gammaA = gamma_a,
          gammaB = gamma_b
        )
      })
    
    
    
    #### Reactive - Sample Size by Scenario ####################################
    scenario_parameters <-
      shiny::reactive({
        shiny::req(
          if(input$outcome_type == "Continuous"){
            cts_dim() & cts_sd_0() & cts_sd_1()
          } else if(input$outcome_type == "Binary"){
            bin_dim() & bin_p0() & bin_p1()
          } else if(input$outcome_type == "Ordinal"){
            ord_mw()
          }
        )
        
        if(input$outcome_type == "Continuous"){
          params <-
            tidyr::expand_grid(
              difference_in_means = cts_dim(),
              sd_0 = cts_sd_0(),
              sd_1 = cts_sd_1()
            )
        } else if(input$outcome_type == "Binary"){
          params <-
            dplyr::tibble(
              difference_in_means = bin_dim(),
              pi_0 = bin_p0(),
              pi_1 = bin_p1()
            )
        } else if(input$outcome_type == "Ordinal"){
          if(!is.null(ord_mw())){
            params <-
              tidyr::expand_grid(
                mann_whitney = ord_mw()
              )
          }
        }
        
        params <-
          params |>
          tidyr::tibble() |>
          dplyr::mutate(
            scenario = dplyr::row_number()
          ) |> 
          tidyr::expand_grid(
            precision_gain = precision_gain_proportion()
          ) |> 
          dplyr::mutate(
            information_fixed =
              if(input$outcome_type %in% c("Continuous", "Binary")){
                required_information_uninflated(
                  delta = difference_in_means,
                  alpha = input$alpha,
                  sides = test_type(),
                  power = input$power
                )
              } else if(input$outcome_type %in% c("Ordinal")){
                required_information_uninflated_mann_whitney(
                  mw = mann_whitney,
                  alpha = input$alpha,
                  sides = test_type(),
                  power = input$power
                )
              },
            information_inflated =
              if(input$gsd_alpha_spending == "Fixed Sample Size"){
                information_fixed
              } else {
                required_information_inflated(
                  information_fixed = information_fixed,
                  gsd_design_specification = study_design()
                )
              },
            n_per_arm_fixed_unadjusted =
              if(input$outcome_type %in% c("Continuous")){
                information_to_n_continuous_1_to_1(
                  information = information_fixed,
                  sigma_0 = sd_0,
                  sigma_1 = sd_1,
                  round_up = FALSE
                )$n_per_arm
              } else if(input$outcome_type %in% c("Binary")){
                information_to_n_binary_1_to_1(
                  information = information_fixed,
                  pi_0 = pi_0,
                  pi_1 = pi_1,
                  round_up = FALSE
                )$n_per_arm
              } else if(input$outcome_type %in% c("Ordinal")){
                information_to_n_mann_whitney_1_to_1(
                  information = information_fixed,
                  mw = mann_whitney,
                  method = "fm",
                  round_up = FALSE
                )$n_per_arm
              },
            n_per_arm_fixed_adjusted =
              n_per_arm_fixed_unadjusted/precision_gain,
            n_per_arm_gsd_unadjusted =
              if(is.null(study_design())){
                NA
              } else {
                (
                  (study_design() |>
                     rpact::getDesignCharacteristics() |>
                     get(x = "inflationFactor"))*n_per_arm_fixed_unadjusted
                )
              },
            n_per_arm_gsd_adjusted =
              ceiling(n_per_arm_gsd_unadjusted/precision_gain),
            n_per_arm_gsd_unadjusted =
              ceiling(n_per_arm_gsd_unadjusted),
            n_per_arm_fixed_adjusted =
              ceiling(n_per_arm_fixed_adjusted),
            n_per_arm_fixed_unadjusted =
              ceiling(n_per_arm_fixed_unadjusted)
          )
      })
    
    
    interim_labels <-
      shiny::reactive({
        shiny::req(
          !is.null(scenario_parameters()),
          !is.null(design_info_fraction())
        )
        
        n_interim_analyses <- length(design_info_fraction()) - 1
        if(n_interim_analyses > 0){
          if(n_interim_analyses > 1){
            period_labels <-
              paste0(
                paste0("I", head(1:n_interim_analyses, -1)   ), "-",
                paste0("I", 2:n_interim_analyses)
              )
          } else {
            period_labels <- NULL
          }
          return(c(period_labels, paste0("I", n_interim_analyses, "-F")))
        } else {
          return(NULL)
        }
      })
    
    
    information_by_accrual <-
      shiny::reactive({
        shiny::req(
          if(input$outcome_type == "Continuous"){
            cts_dim() & cts_sd_0() & cts_sd_1()
          } else if(input$outcome_type == "Binary"){
            bin_dim() & bin_p0() & bin_p1()
          } else if(input$outcome_type == "Ordinal"){
            ord_mw()
          },
          !is.null(scenario_parameters())
        )
        
        n_interim_analyses <- length(design_info_fraction()) - 1
        
        max_sample_size <-
          max(scenario_parameters()$n_per_arm_gsd_unadjusted)
        
        tidyr::expand_grid(
          scenario_parameters() |>
            dplyr::select(
              -dplyr::all_of(
                x = c("information_fixed",
                      "n_per_arm_fixed_unadjusted",
                      "n_per_arm_gsd_unadjusted"))
            ),
          n_per_arm =
            seq(
              from = min_events_per_arm,
              to = max_sample_size,
              by = accrual_increment
            )
        ) |>
          dplyr::mutate(
            information_unadjusted_cumulative =
              if(input$outcome_type == "Continuous"){
                asymptotic_information_difference_means(
                  n_0 = n_per_arm,
                  sigma_0 = sd_0,
                  n_1 = n_per_arm,
                  sigma_1 = sd_1
                )
              } else if(input$outcome_type == "Binary"){
                asymptotic_information_difference_proportions(
                  n_0 = n_per_arm,
                  pi_0 = pi_0,
                  n_1 = n_per_arm,
                  pi_1 = pi_1
                )
              } else if(input$outcome_type == "Ordinal"){
                asymptotic_information_mann_whitney_fm(
                  n_0 = n_per_arm,
                  n_1 = n_per_arm,
                  mw = mann_whitney
                )
              } else if(input$outcome_type == "Time-to-Event"){
                NA
              },
            information_adjusted_cumulative =
              information_unadjusted_cumulative*precision_gain,
            
            information_fraction_adjusted_cumulative =
              information_adjusted_cumulative/information_inflated,
            
            accrual_phase =
              cut(
                x = information_fraction_adjusted_cumulative,
                breaks = c(-Inf, design_info_fraction(), Inf),
                labels = c("Pre-Analysis", interim_labels(), "Final Analysis")
              ),
            
            stage_number = as.numeric(accrual_phase) - 1,
            accrual_phase = as.character(accrual_phase)
          ) |>
          dplyr::group_by(
            precision_gain, accrual_phase, scenario
          ) |>
          dplyr::mutate(
            information_rank = rank(information_fraction_adjusted_cumulative)
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            analysis_trigger =
              case_when(
                (stage_number > 0) & (stage_number <= n_interim_analyses) &
                  information_rank == 1 ~
                  paste0("Interim Analysis ", stage_number),
                (stage_number == n_interim_analyses + 1 ) &
                  information_rank == 1 ~ "Final Analysis",
                TRUE ~ NA
              ),
            accrual_phase =
              case_when(
                accrual_phase == "Final Analysis" & information_rank > 1 ~ NA,
                TRUE ~ accrual_phase
              )
          ) |>
          dplyr::filter(
            !is.na(accrual_phase)
          ) |> 
          dplyr::mutate(
            precision_gain =
              factor(
                x = precision_gain
              ),
            scenario =
              factor(
                x = scenario
              )
          )
      })
    
    
    analysis_times <-
      shiny::reactive({
        shiny::req(
          information_by_accrual()
        )
        
        analysis_times <-
          information_by_accrual() |>
          dplyr::filter(
            !is.na(analysis_trigger)
          ) |>
          dplyr::select(
            scenario,
            analysis_trigger,
            n_per_arm,
            dplyr::any_of(
              x = c("difference_in_means",
                    "pi_0", "pi_1", "sd_0", "sd_1", "mann_whitney")
            ),
            stage_number,
            precision_gain,
            information_adjusted = information_adjusted_cumulative,
            information_fraction_adjusted = information_fraction_adjusted_cumulative,
            information_target = information_inflated
          ) |>
          dplyr::group_by(
            scenario,
            information_fraction_adjusted
          ) |>
          # Update to user-specified alpha spending function
          dplyr::mutate(
            n_analyses = length(analysis_trigger),
            info_fraction_recalc =
              case_when(
                analysis_trigger %in% "Final Analysis" ~ 1,
                TRUE ~ information_fraction_adjusted
              )
          ) |>
          dplyr::ungroup() |>
          dplyr::arrange(
            scenario, precision_gain, n_per_arm
          )
      })
    
    analysis_times_table <-
      reactive({
        shiny::req(
          analysis_times()
        )
        
        analysis_table <-
          analysis_times() |>
          dplyr::select(
            dplyr::any_of(
              x = c("scenario", "analysis_trigger",
                    "precision_gain",
                    "difference_in_means", "sd_0", "sd_1", "pi_0", "pi_1",
                    "mann_whitney",
                    "n_per_arm",
                    "information_adjusted", "info_fraction_recalc")
            )
          ) |>
          dplyr::mutate(
            information_adjusted =
              round(information_adjusted, digits = 2),
            info_fraction_recalc =
              round(info_fraction_recalc, digits = 3),
          ) |> 
          dplyr::rename(
            `Scenario` = scenario,
            `Analysis` = analysis_trigger,
            `Relative Precision` = precision_gain,
            `Sample Size Per Arm` = n_per_arm,
            `Information` = information_adjusted,
            `Information Fraction` = info_fraction_recalc
          )
        
        analysis_table <-
          if(input$outcome_type == "Continuous"){
            analysis_table |>
              dplyr::mutate(
                sd_1 = round(sd_1, digits = 3),
                sd_0 = round(sd_0, digits = 3),
                difference_in_means = round(difference_in_means, digits = 3)
              ) |>
              dplyr::rename(
                `Difference: T-C` = difference_in_means,
                `SD: Treatment` = sd_1,
                `SD: Control` = sd_0,
              )
          } else if(input$outcome_type == "Binary"){
            analysis_table |>
              dplyr::mutate(
                pi_1 = round(pi_1, digits = 3),
                pi_0 = round(pi_0, digits = 3),
                difference_in_means = round(difference_in_means, digits = 3)
              ) |>
              dplyr::rename(
                `Difference: T-C` = difference_in_means,
                `Pr{Event}: Treatment` = pi_1,
                `Pr{Event}: Control` = pi_0,
              )
          } else if(input$outcome_type == "Ordinal"){
            analysis_table |>
              dplyr::mutate(
                mann_whitney = round(mann_whitney, digits = 3),
              ) |>
              dplyr::rename(
                `Mann Whitney:` = mann_whitney
              )
          }
      })
    
    
    scenario_table <-
      shiny::reactive({
        shiny::req(
          scenario_parameters()
        )
        
        scenario_table <-
          scenario_parameters()
        
        if(input$outcome_type == "Continuous"){
          scenario_table <-
            scenario_table |> 
            dplyr::mutate(
              sd_1 = round(sd_1, digits = 3),
              sd_0 = round(sd_0, digits = 3),
              difference_in_means = round(difference_in_means, digits = 3)
            ) |>
            dplyr::select(
              dplyr::all_of(
                x = c("scenario", "difference_in_means",
                      "sd_0", "sd_1", "precision_gain")
              )
            ) |> 
            dplyr::rename(
              `Difference: T-C` = difference_in_means,
              `SD: Treatment` = sd_1,
              `SD: Control` = sd_0,
            )
        } else if(input$outcome_type == "Binary"){
          scenario_table <-
            scenario_table |>
            dplyr::mutate(
              pi_1 = round(pi_1, digits = 3),
              pi_0 = round(pi_0, digits = 3),
              difference_in_means = round(difference_in_means, digits = 3)
            ) |>
            dplyr::select(
              dplyr::all_of(
                x = c("scenario", "difference_in_means",
                      "pi_0", "pi_1", "precision_gain")
              )
            ) |> 
            dplyr::rename(
              `Difference: T-C` = difference_in_means,
              `Pr{Event}: Treatment` = pi_1,
              `Pr{Event}: Control` = pi_0,
            ) 
        } else if(input$outcome_type == "Ordinal"){
          scenario_table <-
            scenario_table |>
            dplyr::mutate(
              mann_whitney = round(mann_whitney, digits = 3),
            ) |>
            dplyr::select(
              dplyr::all_of(
                x = c("scenario", "mann_whitney", "precision_gain")
              )
            ) |> 
            dplyr::rename(
              `Mann Whitney:` = mann_whitney
            )
        }
        
        scenario_table |> 
          dplyr::rename(
            `Scenario` = scenario,
            `Relative Precision` = precision_gain
          )
      })
    
    
    ### SHINY OUTPUT ###########################################################
    output$table_scenarios_input <-
      output$table_scenarios_plot <-
      DT::renderDT(
        DT::datatable(
          data = scenario_table(),
          rownames = FALSE
        )
      )
    
    output$table_analyses <-
      
      DT::renderDT(
        DT::datatable(
          data = analysis_times_table(),
          options = 
            list(
              lengthMenu = 
                list(c(10, 25, 50, 100, -1),
                     c('10', '25', '50', '100', 'All')),
              pageLength = -1
          ),
          rownames = FALSE
        )
      )
    
    
    # Plot information accrued by sample size
    output$plot_information_by_n <-
      shiny::renderPlot(
        expr = {
          shiny::req(
            information_by_accrual()
          )
          
          info_by_n <-
            ggplot2::ggplot(
            ) +
            ggplot2::geom_hline(
              yintercept = 1,
              lty = 2,
              alpha = 0.8
            ) +
            ggplot2::geom_line(
              ggplot2::aes(
                y = information_fraction_adjusted_cumulative,
                x = n_per_arm,
                color = scenario,
                lty = precision_gain
              ),
              lwd = 1.2,
              data = information_by_accrual()
            ) +
            ggplot2::geom_segment(
              ggplot2::aes(
                x = n_per_arm,
                xend = n_per_arm,
                y = 0,
                yend = information_fraction_adjusted,
                color = scenario,
                lty = precision_gain
              ),
              lwd = 1,
              alpha = 0.6,
              data = analysis_times()
            ) +
            ggplot2::ggtitle(
              label = "Information Accrued by Sample Size"
            ) +
            ggplot2::ylab("Fraction of Required Information") +
            ggplot2::xlab("Sample Size per arm") +
            my_theme +
            color_palette +
            ggplot2::guides(
              color = ggplot2::guide_legend(title = "Scenario"),
              lty = ggplot2::guide_legend(title = "Relative Precision")
            )
          
          print(info_by_n)
        },
        
        # Dynamically Adjust Plot Size
        height = function() input$info_plot_height,
        width = function() input$info_plot_width
      )
    
    
    
    
    # Describe current design being considered
    output$ui_input_string <-
      shiny::renderText(
        expr = {
          if(input$outcome_type == "Continuous"){
            if(
              length(cts_dim()) == 0 |
              length(cts_sd_0()) == 0 |
              length(cts_sd_1()) == 0
            ){
              paste0(
                "For a continuous outcome, input the difference in mean ",
                "outcome between treatment and control arms and the standard ",
                "deviation of outcomes in each arm. Standard deviations must ",
                "be positive. See the `Instructions` tab for more information."
              )
            } else {
              paste0(
                "The goal is to design a ",
                ifelse(
                  test = input$gsd_alpha_spending == "Fixed Sample Size",
                  yes = "single",
                  no =
                    if(is.null(design_stages())){
                      "multi"
                    } else {
                      design_stages()
                    }
                ),
                "-stage randomized trial designed to detect a difference in ",
                " means of {",
                paste(cts_dim(), collapse = ", "), "} ",
                "between treatment and control when the standard deviation of ",
                "outcomes in the treatment arm is {",
                paste(cts_sd_0(), collapse = ", "), "} and the ",
                "standard deviation of outcomes in the control arm is {",
                paste(cts_sd_1(), collapse = ", "), "}. ",
                
                if(length(design_info_fraction()) > 1){
                  paste0(
                    "Interim analyses will be conducted when ",
                    paste_and(paste0(100*design_info_fraction(), "%")),
                    " of information is available. "
                  )
                },
                
                if(length(precision_gain()) >= 1){
                  paste0(
                    "If covariate adjustment is applied, the precision of ",
                    "estimation is changed by ",
                    paste_and(paste0(precision_gain(), "%")), "."
                  )
                },
                
                collapse = ""
              )
            }
          } else if(input$outcome_type == "Binary"){
            if(
              length(bin_dim()) == 0 |
              length(bin_p1()) == 0
            ){
              paste0(
                "For a binary outcome, input the risk difference of the event ",
                "between treatment and control arms and risk of the outcome ",
                "event in the control arm. The risk difference must be ",
                "between -1 and 1, and the risk of the event in the control ",
                "arm must be between 0 and 1. Additionally, the sum of the ",
                "risk difference and risk of the event in the control arm ",
                "must be between 0 and 1: this is the probability of the ",
                "event in the treatment arm. ",
                "See the `Instructions` tab for more information."
              )
            } else {
              paste0(
                "The goal is to design a ",
                ifelse(
                  test = input$gsd_alpha_spending == "Fixed Sample Size",
                  yes = "single",
                  no =
                    if(length(design_info_fraction()) < 2){
                      "multi"
                    } else {
                      design_stages()
                    }
                ),
                "-stage randomized trial designed to detect a difference in ",
                " means of {",
                paste(bin_dim(), collapse = ", "), "} ",
                "between treatment and control when the probability of an ",
                "event in the treatment arm is {",
                paste(bin_p1(), collapse = ", "), "} and the probability of ",
                "an event in the control arm is {",
                paste(bin_p0(), collapse = ", "), "}. ",
                
                if(length(design_info_fraction()) > 1){
                  paste0(
                    "Interim analyses will be conducted when ",
                    paste_and(paste0(100*design_info_fraction(), "%")),
                    " of information is available. "
                  )
                },
                
                if(length(precision_gain()) >= 1){
                  paste0(
                    "If covariate adjustment is applied, the precision of ",
                    "estimation is changed by ",
                    paste_and(paste0(precision_gain(), "%")), "."
                  )
                },
                
                collapse = ""
              )
            }
          } else if(input$outcome_type == "Ordinal"){
            if(is.null(ord_mw())){
              paste0(
                "For an ordinal outcome, you can enter either the ",
                "distribution of outcomes in each treatment arm (i.e. the ",
                "probability mass functions (PMFs)) or the ",
                "Mann-Whitney estimand. ",
                "See the `Instructions` tab for more information."
              )
            } else {
              paste0(
                "The goal is to design a ",
                ifelse(
                  test = input$gsd_alpha_spending == "Fixed Sample Size",
                  yes = "single",
                  no =
                    if(length(design_info_fraction()) < 2){
                      "multi"
                    } else {
                      design_stages()
                    }
                ),
                "-stage randomized trial designed to detect a tie-adjusted ",
                "absolute difference in the probability of a higher outcome ",
                "score (i.e. Mann-Whitney estimand) of {",
                paste(ord_mw(), collapse = ", "), "} ",
                "or higher. ",
                
                if(length(design_info_fraction()) > 1){
                  paste0(
                    "Interim analyses will be conducted when ",
                    paste_and(paste0(100*design_info_fraction(), "%")),
                    " of information is available. "
                  )
                },
                
                if(length(precision_gain()) >= 1){
                  paste0(
                    "If covariate adjustment is applied, the precision of ",
                    "estimation is changed by ",
                    paste_and(paste0(precision_gain(), "%")), "."
                  )
                },
                
                collapse = ""
              )
            }
          } else if(input$outcome_type == "Time-to-Event") {
            ""
          } else if(input$outcome_type == "Time-to-Event (Not Yet Implemented)"){
            paste0(
              "Time-to-event outcomes are not yet implemented. This will ",
              "be updated in a future release. Group sequential designs (GSD) ",
              "for time-to-event outcomes are available, including: ",
              "R (`rpact` package), ",
              "SAS (`PROC SEQDESIGN`), Stata (`gsdesign logrank`), and ",
              "other statistical packages."
            )
          }
        }
      )
  }




# Run the application 
shiny::shinyApp(ui = ui, server = server)
