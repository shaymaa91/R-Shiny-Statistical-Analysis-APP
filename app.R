# Load necessary libraries
library(shiny)
library(shinydashboard)
library(plotrix)
library(corrplot)
library(RColorBrewer)
library(moments)
library(DT)
library(BSDA)
library(EnvStats)

all_datasets <- data(package = .packages(all.available = TRUE))$results[, "Item"]

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Statistical Analysis App"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Descriptive Analysis", tabName = "descriptive", icon = icon("chart-bar")),
                menuItem("Discrete Distributions", tabName = "distributions", icon = icon("chart-line")),
                menuItem("Continuous Distributions", tabName = "continuous", icon = icon("chart-area")),
                menuItem("Sampling Distributions", tabName = "sampling", icon = icon("project-diagram")),
                menuItem("Inferential Statistics", tabName = "inferential", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # Home Tab Content
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       div(style = "text-align: center; margin-top: 20px;",
                           img(src = "KFUPM_logo.png", height = "250px")
                       ),
                       div(style = "max-width: 900px; margin: 0 auto; padding: 20px; background-color: #f5f5f5; border-radius: 10px;",
                           h1("Welcome to the Statistical Analysis App", style = "color: #00703c; text-align: center;"),
                           p("Explore a suite of statistical tools tailored for your analysis needs.",
                             style = "text-align: center; font-size: 16px;"),
                           
                           br(),
                           
                           fluidRow(
                             column(2, offset = 1,
                                    div(class = "icon-card",
                                        actionLink("link_descriptive", icon("chart-bar", class = "fa-2x")),
                                        div(class = "icon-card-title", "Descriptive", br(), "Stats"))
                             ),
                             column(2,
                                    div(class = "icon-card",
                                        actionLink("link_discrete", icon("chart-line", class = "fa-2x")),
                                        div(class = "icon-card-title", "Discrete", br(), "Dist."))
                             ),
                             column(2,
                                    div(class = "icon-card",
                                        actionLink("link_continuous", icon("chart-area", class = "fa-2x")),
                                        div(class = "icon-card-title", "Continuous", br(), "Dist."))
                             ),
                             column(2,
                                    div(class = "icon-card",
                                        actionLink("link_sampling", icon("project-diagram", class = "fa-2x")),
                                        div(class = "icon-card-title", "Sampling", br(), "Dist."))
                             ),
                             column(2,
                                    div(class = "icon-card",
                                        actionLink("link_inferance", icon("calculator", class = "fa-2x")),
                                        div(class = "icon-card-title", "Inferential", br(), "Stats"))
                             )
                           ),
                           
                           br(), br(),
                           
                           div(style = "margin-left: 80px; text-align: left; max-width: 800px;",
                               h3("Getting Started"),
                               p("1. Choose your desired analysis section"),
                               p("2. Follow the interface instructions for each tool"),
                               p("3. Upload your data or use built-in datasets for analysis"),
                               br(),
                               tags$blockquote(HTML("<b>“Without data, you're just another person with an opinion.” – W. Edwards Deming</b>")),
                              
                               h4("Developed by:", style = "text-align: center;"),
                               p("Fatimah Albahar, Kholoud Almutairi, Shaymaa Khalaf and Waiam Malibari", style = "color: #00703c; text-align: center;")
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "inferential",
              fluidRow(
                column(12, align = "right",
                       actionButton("go_home_from_inferential", "Back to Home", icon = icon("home"),
                                    style = "color: white; background-color: #00703c; border: none;")
                )
              ),
              h3("Inferential Statistics"),
              tabsetPanel(
                # One Population Sample
                tabPanel("One Population Sample",
                         h4("One Population Inference"),
                         fluidRow(
                           column(6,
                                  selectInput("test_type", "Select Test Type:",
                                              choices = c("Normality Check", "Location Parameter Test", "Variance Test", "Proportion Test")),
                                  
                                  # Normality Check Panel
                                  conditionalPanel(
                                    condition = "input.test_type == 'Normality Check'",
                                    radioButtons("nc_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file", "Built-in Dataset" = "builtin", "Manual Entry" = "manual"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.nc_data_source == 'file'",
                                      fileInput("nc_file", "Upload CSV File"),
                                      selectInput("nc_column", "Select Column", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.nc_data_source == 'builtin'",
                                      selectInput("nc_dataset", "Choose Dataset:", choices = all_datasets),
                                      selectInput("nc_dataset_col", "Select Column", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.nc_data_source == 'manual'",
                                      textAreaInput("nc_manual_data", "Enter Data (comma/space separated):", placeholder = "e.g., 98, 102, 101, 99")
                                    )
                                  ),
                                  
                                  # Mean Test Panel
                                  conditionalPanel(
                                    condition = "input.test_type == 'Location Parameter Test'",
                                    radioButtons("mean_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file", "Built-in Dataset" = "builtin", "Manual Entry" = "manual", "Summary Statistics" = "summary"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.mean_data_source == 'file'",
                                      fileInput("mean_file", "Upload CSV File"),
                                      selectInput("mean_column", "Select Column", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source == 'builtin'",
                                      selectInput("mean_dataset", "Choose Dataset:", choices = all_datasets),
                                      selectInput("mean_dataset_col", "Select Column", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source == 'manual'",
                                      textAreaInput("mean_manual_data", "Enter Data (comma/space separated):", placeholder = "e.g., 98, 102, 101, 99")
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source != 'summary'",
                                      selectInput("mean_test_type", "Select Test:", choices = c("Z-test", "t-test", "Wilcoxon signed-rank test"))
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source == 'summary'",
                                      selectInput("mean_test_type", "Select Test:", choices = c("Z-test", "t-test"))
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source != 'summary' && input.mean_test_type == 't-test'",
                                      numericInput("mu0_ttest", "Hypothesized mean (μ₀)", value = 0)
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.mean_data_source != 'summary' && input.mean_test_type == 'Wilcoxon signed-rank test'",
                                      numericInput("m0_wtest", "Hypothesized median (m₀)", value = 0)
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source != 'summary' && input.mean_test_type == 'Z-test'",
                                      numericInput("mu0_ztest", "Hypothesized mean (μ₀)", value = 0),
                                      numericInput("sigma_ztest", "Population SD (σ)", value = 1)
                                    ),
                                    conditionalPanel(
                                      condition = "input.mean_data_source == 'summary'",
                                      numericInput("n_mean", "Sample size (n)", value = 9),
                                      numericInput("xbar", "Sample mean", value = 102),
                                      numericInput("mu0", "Hypothesized mean (μ₀)", value = 100),
                                      conditionalPanel(condition = "input.mean_test_type == 'Z-test'", numericInput("sigma", "Population SD (σ)", value = 2)),
                                      conditionalPanel(condition = "input.mean_test_type == 't-test'", numericInput("s", "Sample SD (s)", value = 2))
                                    )
                                  ),
                                  
                                  
                                  # Variance Test Panel
                                  conditionalPanel(
                                    condition = "input.test_type == 'Variance Test'",
                                    radioButtons("var_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file",
                                                             "Built-in Dataset" = "builtin",
                                                             "Manual Entry" = "manual"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.var_data_source == 'file'",
                                      fileInput("var_file", "Upload CSV File"),
                                      selectInput("var_column", "Select Column", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.var_data_source == 'builtin'",
                                      selectInput("var_dataset", "Choose Dataset:",
                                                  choices = all_datasets),
                                      selectInput("var_dataset_col", "Select Column", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.var_data_source == 'manual'",
                                      textAreaInput("var_manual_data", "Enter Data (comma/space separated):",
                                                    placeholder = "e.g., 98, 102, 101, 99")
                                    ),
                                    numericInput("sigma0_sq", "Hypothesized variance", value = 1)
                                  ),
                                  
                                  # Proportion Test Panel
                                  conditionalPanel(
                                    condition = "input.test_type == 'Proportion Test'",
                                    radioButtons("prop_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file",
                                                             "Built-in Dataset" = "builtin",
                                                             "Manual Entry" = "manual",
                                                             "Summary Statistics" = "summary"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.prop_data_source == 'file'",
                                      fileInput("prop_file", "Upload CSV File"),
                                      selectInput("prop_column", "Select Column", choices = NULL),
                                      textInput("success_value", "Success Value", placeholder = "e.g., 1 or 'success'")
                                    ),
                                    conditionalPanel(
                                      condition = "input.prop_data_source == 'builtin'",
                                      selectInput("prop_dataset", "Choose Dataset:",
                                                  choices = all_datasets),
                                      selectInput("prop_dataset_col", "Select Column", choices = NULL),
                                      textInput("prop_success_value", "Success Value", placeholder = "e.g., 1 or 'success'")
                                    ),
                                    conditionalPanel(
                                      condition = "input.prop_data_source == 'manual'",
                                      textAreaInput("prop_manual_data", "Enter Successes (1) and Failures (0):",
                                                    placeholder = "e.g., 1, 0, 1, 1, 0")
                                    ),
                                    conditionalPanel(
                                      condition = "input.prop_data_source == 'summary'",
                                      numericInput("x_prop", "Number of successes (x)", value = 19),
                                      numericInput("n_prop", "Sample size (n)", value = 200)
                                    ),
                                    numericInput("p0", "Hypothesized proportion", value = 0.1),
                                    radioButtons("prop_test_type", "Test Type:",
                                                 choices = c("Exact binomial test", 
                                                             "Approximate normal test"))
                                  ),
                                  
                                  numericInput("alpha", "Significance level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
                                  conditionalPanel(
                                    condition = "input.test_type != 'Normality Check'",
                                    selectInput("alternative", "Alternative Hypothesis",
                                                choices = c("Two-sided" = "two.sided", "Less" = "less", "Greater" = "greater"))
                                  ),
                                  actionButton("run", "Calculate")
                           ),
                           mainPanel(verbatimTextOutput("result"), uiOutput("conclusion"))
                         )
                ),
                # Two Population Sample
                tabPanel("Two Population Sample",
                         h4("Two Population Inference"),
                         fluidRow(
                           column(6,
                                  selectInput("two_test_type", "Select Test Type:",
                                              choices = c(
                                                "Normality Check",          
                                                "Location Parameter Test",
                                                "Variance Test",
                                                "Proportion Test"
                                              )
                                  ),
                                  
                                  ## -------------------- Normality Check -------------------- ##
                                  conditionalPanel(
                                    condition = "input.two_test_type == 'Normality Check'",
                                    radioButtons("two_nc_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file",
                                                             "Built-in Datasets" = "builtin",
                                                             "Manual Entry"     = "manual"),
                                                 selected = "file"
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_nc_data_source == 'file'",
                                      fileInput("two_nc_file", "Upload CSV with both samples (two columns)"),
                                      selectInput("two_nc_column1", "Select Column for Sample X", choices = NULL),
                                      selectInput("two_nc_column2", "Select Column for Sample Y", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_nc_data_source == 'builtin'",
                                      selectInput("two_nc_dataset",    "Choose Dataset:",    choices = all_datasets),
                                      selectInput("two_nc_dataset_col1","Column for Sample X", choices = NULL),
                                      selectInput("two_nc_dataset_col2","Column for Sample Y", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_nc_data_source == 'manual'",
                                      textAreaInput("two_nc_manual1", "Enter Sample X", placeholder = "e.g. 2,3,5,7,..."),
                                      textAreaInput("two_nc_manual2", "Enter Sample Y", placeholder = "e.g. 5,7,8,9,...")
                                    )
                                  ),
                                  
                                  ## ---------------- Location Parameter Test ------------------ ##
                                  conditionalPanel(
                                    condition = "input.two_test_type == 'Location Parameter Test'",
                                    radioButtons("two_mean_data_source", "Data Source:",
                                                 choices = c("Upload Dataset"     = "file",
                                                             "Built-in Datasets"= "builtin",
                                                             "Manual Entry"     = "manual"),
                                                 selected = "file"
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_mean_data_source == 'file'",
                                      fileInput("two_mean_file", "Upload CSV with both samples (two columns)"),
                                      selectInput("two_mean_col1", "Column for Sample X", choices = NULL),
                                      selectInput("two_mean_col2", "Column for Sample Y", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_mean_data_source == 'builtin'",
                                      selectInput("two_mean_dataset", "Choose Dataset:", choices = all_datasets),
                                      selectInput("two_mean_dataset_col1", "Column for Sample X", choices = NULL),
                                      selectInput("two_mean_dataset_col2", "Column for Sample Y", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_mean_data_source == 'manual'",
                                      textAreaInput("two_mean_manual1", "Enter Data X", placeholder = "e.g. 2,3,5,7,..."),
                                      textAreaInput("two_mean_manual2", "Enter Data Y", placeholder = "e.g. 5,7,8,9,...")
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_mean_data_source == 'summary'",
                                      h4("Summary Sample X"),
                                      numericInput("two_n1", "n₁", value = 10, min = 1),
                                      numericInput("two_xbar1", "x̄₁", value = 5),
                                      radioButtons("two_sig1_known", "σ₁ known?", choices = c("Yes","No"), selected = "No"),
                                      conditionalPanel("input.two_sig1_known == 'Yes'", numericInput("sigma1", "σ₁", value = 1)),
                                      conditionalPanel("input.two_sig1_known == 'No'", numericInput("s1", "s₁", value = 1)),
                                      h4("Summary Sample Y"),
                                      numericInput("two_n2", "n₂", value = 10, min = 1),
                                      numericInput("two_xbar2", "x̄₂", value = 6),
                                      radioButtons("two_sig2_known", "σ₂ known?", choices = c("Yes","No"), selected = "No"),
                                      conditionalPanel("input.two_sig2_known == 'Yes'", numericInput("sigma2", "σ₂", value = 1)),
                                      conditionalPanel("input.two_sig2_known == 'No'", numericInput("s2", "s₂", value = 1))
                                    ),
                                    numericInput("two_mu0_diff", "H₀: μ₁ – μ₂ =", value = 0),
                                    selectInput("two_mean_test_type", "Select Test:",
                                                choices = c("Z-test  (Independent)", "t-test (Independent & equal variance)", "t-test (Independent & not equal variance)",
                                                            "t-test (Paired)", "Wilcoxon (Independent)", "Wilcoxon (Paired)")
                                    )
                                  ),
                                  
                                  ## -------------Variance Test ---------------- ##
                                  conditionalPanel(
                                    condition = "input.two_test_type == 'Variance Test'",
                                    radioButtons("two_var_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file",
                                                             "Built-in"     = "builtin",
                                                             "Manual Entry" = "manual"),
                                                 selected = "file"
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_var_data_source == 'file'",
                                      fileInput("two_var_file", "Upload CSV with both samples (two columns)"),
                                      selectInput("two_var_col1", "Column for Sample X", choices = NULL),
                                      selectInput("two_var_col2", "Column for Sample Y", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_var_data_source == 'builtin'",
                                      selectInput("two_var_dataset", "Choose Dataset:", choices = all_datasets),
                                      selectInput("two_var_dataset_col1", "Column for Sample X", choices = NULL),
                                      selectInput("two_var_dataset_col2", "Column for Sample Y", choices = NULL)
                                    ),
                                    conditionalPanel(
                                      condition = "input.two_var_data_source == 'manual'",
                                      textAreaInput("two_var_manual1", "Enter Data X"),
                                      textAreaInput("two_var_manual2", "Enter Data Y")
                                    ),
                                    
                                    numericInput("two_theta0", "H₀: σ₁²/σ₂² =", value = 1),
                                    selectInput("two_var_test_type", "Select Test:", choices = c("F-test","Mood's test"))
                                  ),
                                  
                                  # Proportion Test Panel
                                  conditionalPanel(
                                    condition = "input.two_test_type == 'Proportion Test'",
                                    textAreaInput("two_prop_manual_data", "Enter Successes (comma-separated groups):",
                                                  placeholder = "e.g., 5, 10, 30"),
                                    textAreaInput("two_prop_manual_n", "Enter Totals (comma-separated groups):",
                                                  placeholder = "e.g., 50, 80, 100")
                                  ),
                                  
                                  
                                  numericInput("two_alpha", "Significance level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
                                  actionButton("two_run", "Calculate")
                           ),
                           
                           mainPanel(
                             verbatimTextOutput("two_result"),
                             uiOutput("two_conclusion")
                           )
                         )
                ),
                #Three or More Population Sample
                tabPanel("Three or More Population Sample",
                         h4("Three or More Population Inference"),
                         fluidRow(
                           column(width = 6,
                                  
                                  selectInput("three_test_type", "Select Test Type",
                                              choices = c("Normality Check","Location Parameter Test","Variance Test","Proportion Test")
                                  ),
                                  # Normality Check Panel
                                  conditionalPanel(
                                    condition = "input.three_test_type == 'Normality Check'",
                                    radioButtons("three_nc_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file", "Built-in Dataset" = "builtin", "Manual Entry" = "manual"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.three_nc_data_source == 'file'",
                                      fileInput("three_nc_file", "Upload CSV File"),
                                      selectizeInput("three_nc_columns", "Select Columns", choices = NULL, multiple = TRUE)
                                    ),
                                    conditionalPanel(
                                      condition = "input.three_nc_data_source == 'builtin'",
                                      selectInput("three_nc_dataset", "Choose Dataset:", choices = all_datasets),
                                      selectizeInput("three_nc_dataset_cols", "Select Columns", choices = NULL, multiple = TRUE)
                                    ),
                                    conditionalPanel(
                                      condition = "input.three_nc_data_source == 'manual'",
                                      textAreaInput("three_nc_manual_data", "Enter Data (comma/space separated, groups separated by semicolons):", 
                                                    placeholder = "e.g., 98, 102, 101; 95, 99, 104; 100, 105, 110")
                                    )
                                  ),
                                  
                                  # Mean Test Panel
                                  conditionalPanel(
                                    condition = "input.three_test_type == 'Location Parameter Test'",
                                    radioButtons("three_mean_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file", "Built-in Dataset" = "builtin", "Manual Entry" = "manual"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.three_mean_data_source == 'file'",
                                      fileInput("three_mean_file", "Upload CSV File"),
                                      
                                      radioButtons("three_mean_input_mode_file", "Input Mode:",
                                                   choices = c("Response + Group" = "group", "Multiple Columns" = "cols"),
                                                   selected = "group"),
                                      
                                      conditionalPanel(
                                        condition = "input.three_mean_input_mode_file == 'group'",
                                        selectInput("three_mean_response_file", "Response Variable", choices = NULL),
                                        selectInput("three_mean_group_file", "Grouping Variable", choices = NULL)
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.three_mean_input_mode_file == 'cols'",
                                        selectizeInput("three_mean_cols_file", "Select Columns", choices = NULL, multiple = TRUE)
                                      )
                                    )
                                    ,
                                    conditionalPanel(
                                      condition = "input.three_mean_data_source == 'builtin'",
                                      selectInput("three_mean_dataset", "Choose Dataset:", choices = all_datasets),
                                      
                                      radioButtons("three_mean_input_mode_builtin", "Input Mode:",
                                                   choices = c("Response + Group" = "group", "Multiple Columns" = "cols"),
                                                   selected = "group"),
                                      
                                      conditionalPanel(
                                        condition = "input.three_mean_input_mode_builtin == 'group'",
                                        selectInput("three_mean_response_builtin", "Response Variable", choices = NULL),
                                        selectInput("three_mean_group_builtin", "Grouping Variable", choices = NULL)
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.three_mean_input_mode_builtin == 'cols'",
                                        selectizeInput("three_mean_cols_builtin", "Select Columns", choices = NULL, multiple = TRUE)
                                      )
                                    ),conditionalPanel(
                                      condition = "input.three_mean_data_source == 'manual'",
                                      textAreaInput("three_mean_manual_data", "Enter Data (comma/space separated, groups separated by semicolons):",
                                                    placeholder = "e.g., 98, 102, 101; 95, 99, 104; 100, 105, 110")
                                    ),
                                    radioButtons("three_mean_test_type", "Select Test:",
                                                 choices = c("ANOVA" = "anova", "Kruskal-Wallis" = "kruskal")))
                                  ,
                                  
                                  # Variance Test Panel
                                  conditionalPanel(
                                    condition = "input.three_test_type == 'Variance Test'",
                                    radioButtons("three_var_data_source", "Data Source:",
                                                 choices = c("Upload Dataset" = "file", "Built-in Dataset" = "builtin", "Manual Entry" = "manual"),
                                                 selected = "file"),
                                    conditionalPanel(
                                      condition = "input.three_var_data_source == 'file'",
                                      fileInput("three_var_file", "Upload CSV File"),
                                      
                                      radioButtons("three_var_input_mode_file", "Input Mode:",
                                                   choices = c("Response + Group" = "group", "Multiple Columns" = "cols"),
                                                   selected = "group"),
                                      
                                      conditionalPanel(
                                        condition = "input.three_var_input_mode_file == 'group'",
                                        selectInput("three_var_response_file", "Response Variable", choices = NULL),
                                        selectInput("three_var_group_file", "Grouping Variable", choices = NULL)
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.three_var_input_mode_file == 'cols'",
                                        selectizeInput("three_var_cols_file", "Select Columns", choices = NULL, multiple = TRUE)
                                      )
                                    ),conditionalPanel(
                                      condition = "input.three_var_data_source == 'builtin'",
                                      selectInput("three_var_dataset", "Choose Dataset:", choices = all_datasets),
                                      
                                      radioButtons("three_var_input_mode_builtin", "Input Mode:",
                                                   choices = c("Response + Group" = "group", "Multiple Columns" = "cols"),
                                                   selected = "group"),
                                      
                                      conditionalPanel(
                                        condition = "input.three_var_input_mode_builtin == 'group'",
                                        selectInput("three_var_response_builtin", "Response Variable", choices = NULL),
                                        selectInput("three_var_group_builtin", "Grouping Variable", choices = NULL)
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.three_var_input_mode_builtin == 'cols'",
                                        selectizeInput("three_var_cols_builtin", "Select Columns", choices = NULL, multiple = TRUE)
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.three_var_data_source == 'manual'",
                                      textAreaInput("three_var_manual_data", "Enter Data (comma/space separated, groups separated by semicolons):",
                                                    placeholder = "e.g., 98, 102, 101; 95, 99, 104; 100, 105, 110")
                                    )
                                    ,radioButtons("three_var_test_type", "Select Test:",
                                                  choices = c("Bartlett" = "bartlett", "Fligner-Killeen" = "fligner"))
                                    
                                  ),
                                  
                                  
                                  # Proportion Test Panel
                                  conditionalPanel(
                                    condition = "input.three_test_type == 'Proportion Test'",
                                    textAreaInput("three_prop_manual_data", "Enter Successes (comma-separated groups):",
                                                  placeholder = "e.g., 5, 10, 30"),
                                    textAreaInput("three_prop_manual_n", "Enter Totals (comma-separated groups):",
                                                  placeholder = "e.g., 50, 80, 100")
                                  ),
                                  
                                  
                                  numericInput("three_alpha", "Significance level (α)", value = 0.05, min = 0, max = 1, step = 0.01),
                                  actionButton("three_run", "Calculate")
                           ),
                           mainPanel(
                             verbatimTextOutput("three_result"),
                             uiOutput("three_conclusion")
                           )
                         )))
      ),
      
      # Descriptive Analysis Tab
      tabItem(tabName = "descriptive",
              fluidRow(
                column(12, align = "right",
                       actionButton("go_home_from_descriptive", "Back to Home", icon = icon("home"),
                                    style = "color: white; background-color: #00703c; border: none;")
                )
              ),
              h3("Descriptive Analysis App"),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("data_source", "Data Source:",
                               choices = c("Upload CSV" = "upload", 
                                           "Use Built-in Dataset" = "builtin"),
                               selected = "upload"),
                  conditionalPanel(
                    condition = "input.data_source == 'upload'",
                    fileInput("file", "Upload CSV File", accept = ".csv")
                  ),
                  conditionalPanel(
                    condition = "input.data_source == 'builtin'",
                    selectInput("dataset", "Choose Dataset:",
                                choices = all_datasets)
                  ),
                  selectInput("analysis_type", "Select Analysis Type:", 
                              choices = c("General Summary", "Single Variable Analysis", 
                                          "Two Variables Analysis", "Three Variables Analysis")),
                  
                  uiOutput("var_select"),
                  uiOutput("var_select_2"),
                  uiOutput("var_select_3"),
                  conditionalPanel(condition="input.analysis_type == 'Single Variable Analysis'",
                                   selectInput("calc_quantile", "Calculate Quantile:", 
                                               choices = c("Yes", "No"), selected ="No"),
                                   conditionalPanel(condition="input.calc_quantile =='Yes'",
                                                    numericInput("quantile_prob", "Quantile (0-1):", 
                                                                 value = 0.5, min = 0, max = 1, step = 0.01),
                                                    selectInput("quantile_type", "Quantile Type:",
                                                                choices = 1:9, selected = 7)
                                                    ,
                                                    checkboxInput("show_iqr", "Calculate IQR", value = FALSE)
                                   )
                                   ),
                  actionButton("analyze", "Analyze")
                ),
                mainPanel(
                  conditionalPanel(
                    condition = "input.analysis_type == 'General Summary'",
                    h4("General Summary of the Dataset"),
                    verbatimTextOutput("general_summary"),
                    h4("Data Preview"),
                    DTOutput("data_preview")
                  ),
                  conditionalPanel(
                    condition = "input.analysis_type == 'Single Variable Analysis'",
                    h4("Summary Statistics"),
                    verbatimTextOutput("summary"),
                    h4("Visualizations"),
                    plotOutput("plot_single")
                  ),
                  conditionalPanel(
                    condition = "input.analysis_type == 'Two Variables Analysis'",
                    h4("Summary Statistics"),
                    verbatimTextOutput("summary2"),
                    h4("Visualizations"),
                    plotOutput("plot_two")
                  ),
                  conditionalPanel(
                    condition = "input.analysis_type == 'Three Variables Analysis'",
                    h4("Summary Statistics"),
                    verbatimTextOutput("summary3"),
                    h4("Visualizations"),
                    plotOutput("plot_three"),
                    plotOutput("plot_three_heatmap")
                  )
                )
              )
      ),
      
      # Discrete Distributions Tab
      tabItem(tabName = "distributions",
              fluidRow(
                column(12, align = "right",
                       actionButton("go_home_from_descriptive", "Back to Home", icon = icon("home"),
                                    style = "color: white; background-color: #00703c; border: none;")
                )
              ),
              h3("Discrete Distribution Calculator"),
              tabsetPanel(
                tabPanel("Poisson Distribution",
                         h4("Poisson Distribution Calculator"),
                         numericInput("lambda", "Lambda (λ):", value = 1, min = 0),
                         selectInput("calc_type_pois", "Select Calculation:",
                                     choices = c("Probability Mass Function PMF",
                                                 "Cumulative Probability P(X <= q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Cumulative Probability P(X >= q)",
                                                 "Cumulative Probability P(X < q)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("pois_input_ui"),
                         actionButton("calc_pois", "Calculate"),
                         verbatimTextOutput("pois_result")
                ),
                tabPanel("Geometric Distribution",
                         h4("Geometric Distribution Calculator"),
                         numericInput("prob", "Probability of Success (p):", value = 0.5, min = 0, max = 1),
                         selectInput("calc_type_geom", "Select Calculation:",
                                     choices = c("Probability Mass Function PMF",
                                                 "Cumulative Probability P(X <= q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Cumulative Probability P(X >= q)",
                                                 "Cumulative Probability P(X < q)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("geom_input_ui"),
                         actionButton("calc_geom", "Calculate"),
                         verbatimTextOutput("geom_result")
                ),
                tabPanel("Binomial Distribution",
                         h4("Binomial Distribution Calculator"),
                         numericInput("n_binom", "Number of Trials (n):", value = 10, min = 0),
                         numericInput("p_binom", "Probability of Success (p):", value = 0.5, min = 0, max = 1),
                         selectInput("calc_type_binom", "Select Calculation:",
                                     choices = c("Probability Mass Function PMF",
                                                 "Cumulative Probability P(X <= q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Cumulative Probability P(X >= q)",
                                                 "Cumulative Probability P(X < q)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("binom_input_ui"),
                         actionButton("calc_binom", "Calculate"),
                         verbatimTextOutput("binom_result")
                ),
                tabPanel("Hypergeometric Distribution",
                         h4("Hypergeometric Distribution Calculator"),
                         numericInput("N_hyper", "Population Size (N):", value = 50, min = 0),
                         numericInput("K_hyper", "Successes in Population (K):", value = 20, min = 0),
                         numericInput("n_hyper", "Sample Size (n):", value = 10, min = 0),
                         selectInput("calc_type_hyper", "Select Calculation:",
                                     choices = c("Probability Mass Function PMF",
                                                 "Cumulative Probability P(X <= q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Cumulative Probability P(X >= q)",
                                                 "Cumulative Probability P(X < q)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("hyper_input_ui"),
                         actionButton("calc_hyper", "Calculate"),
                         verbatimTextOutput("hyper_result")
                )
              )
      ),
      
      # Continuous Distributions Tab
      tabItem(tabName = "continuous",
              fluidRow(
                column(12, align = "right",
                       actionButton("go_home_from_descriptive", "Back to Home", icon = icon("home"),
                                    style = "color: white; background-color: #00703c; border: none;")
                )
              ),
              h3("Continuous Distribution Calculator"),
              tabsetPanel(
                tabPanel("Normal Distribution",
                         h4("Normal Distribution Calculator"),
                         numericInput("mean_norm", "Mean (μ):", value = 0),
                         numericInput("sd_norm", "Standard Deviation (σ):", value = 1, min = 0),
                         selectInput("calc_type_norm", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Conditional Probability P(X < b | X > a)",
                                                 "Conditional Probability P(X < b | X < a)",
                                                 "Conditional Probability P(X > b | X < a)",
                                                 "Conditional Probability P(X > b | X > a)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("norm_input_ui"),
                         actionButton("calc_norm", "Calculate"),
                         verbatimTextOutput("norm_result")
                ),
                tabPanel("Weibull Distribution",
                         h4("Weibull Distribution Calculator"),
                         numericInput("shape_weibull", "Shape (β):", value = 1, min = 0),
                         numericInput("scale_weibull", "Scale (δ):", value = 1, min = 0),
                         selectInput("calc_type_weibull", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Conditional Probability P(X < b | X > a)",
                                                 "Conditional Probability P(X < b | X < a)",
                                                 "Conditional Probability P(X > b | X < a)",
                                                 "Conditional Probability P(X > b | X > a)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("weibull_input_ui"),
                         actionButton("calc_weibull", "Calculate"),
                         verbatimTextOutput("weibull_result")
                ),
                tabPanel("Lognormal Distribution",
                         h4("Lognormal Distribution Calculator"),
                         numericInput("meanlog_lognorm", "Meanlog (θ):", value = 0),
                         numericInput("sdlog_lognorm", "Sdlog (ω):", value = 1, min = 0),
                         selectInput("calc_type_lognorm", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Conditional Probability P(X < b | X > a)",
                                                 "Conditional Probability P(X < b | X < a)",
                                                 "Conditional Probability P(X > b | X < a)",
                                                 "Conditional Probability P(X > b | X > a)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("lognorm_input_ui"),
                         actionButton("calc_lognorm", "Calculate"),
                         verbatimTextOutput("lognorm_result")
                ),
                tabPanel("Exponential Distribution",
                         h4("Exponential Distribution Calculator"),
                         numericInput("rate_exp", "Rate (λ):", value = 1, min = 0),
                         selectInput("calc_type_exp", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Conditional Probability P(X < b | X > a)",
                                                 "Conditional Probability P(X < b | X < a)",
                                                 "Conditional Probability P(X > b | X < a)",
                                                 "Conditional Probability P(X > b | X > a)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("exp_input_ui"),
                         actionButton("calc_exp", "Calculate"),
                         verbatimTextOutput("exp_result")
                )
              )
      ),
      
      # Sampling Distributions Tab
      tabItem(tabName = "sampling",
              fluidRow(
                column(12, align = "right",
                       actionButton("go_home_from_descriptive", "Back to Home", icon = icon("home"),
                                    style = "color: white; background-color: #00703c; border: none;")
                )
              ),
              h3("Sampling Distribution Calculator"),
              tabsetPanel(
                tabPanel("Normal Distribution",
                         h4("Normal Sampling Distribution"),
                         radioButtons("sd_choice", "Standard Deviation Input:",
                                      choices = c("Enter Sample Standard Error directly" = "direct",
                                                  "Calculate Sample Standard Error from σ/√n" = "calc"),
                                      selected = "direct"),
                         conditionalPanel(
                           condition = "input.sd_choice == 'direct'",
                           numericInput("sampling_sd", "Sample Standard Error:", value = 1, min = 0)
                         ),
                         conditionalPanel(
                           condition = "input.sd_choice == 'calc'",
                           numericInput("population_sd", "Population Standard Deviation (σ):", value = 1, min = 0),
                           numericInput("sample_size", "Sample Size (n):", value = 30, min = 1)
                         ),
                         numericInput("sampling_mean", "Mean (μ):", value = 0),
                         selectInput("calc_type_snorm", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("snorm_input_ui"),
                         actionButton("calc_snorm", "Calculate"),
                         verbatimTextOutput("snorm_result")
                ),
                tabPanel("t-Distribution",
                         h4("Student's t-Distribution"),
                         numericInput("t_df", "Degrees of Freedom (df):", value = 30, min = 1),
                         selectInput("calc_type_t", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("t_input_ui"),
                         actionButton("calc_t", "Calculate"),
                         verbatimTextOutput("t_result")
                ),
                tabPanel("F-Distribution",
                         h4("F-Distribution"),
                         numericInput("df1", "Numerator DF (d1):", value = 5, min = 1),
                         numericInput("df2", "Denominator DF (d2):", value = 10, min = 1),
                         selectInput("calc_type_f", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("f_input_ui"),
                         actionButton("calc_f", "Calculate"),
                         verbatimTextOutput("f_result")
                ),
                tabPanel("Chi-squared Distribution",
                         h4("Chi-squared Distribution"),
                         numericInput("chisq_df", "Degrees of Freedom (df):", value = 5, min = 1),
                         selectInput("calc_type_chisq", "Select Calculation:",
                                     choices = c(
                                                 "Cumulative Probability P(X < q)",
                                                 "Cumulative Probability P(X > q)",
                                                 "Interval Probability P(a < X < b)",
                                                 "Quantile Inverse CDF")),
                         uiOutput("chisq_input_ui"),
                         actionButton("calc_chisq", "Calculate"),
                         verbatimTextOutput("chisq_result")
                )
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  observeEvent(input$go_home_from_descriptive, {
    updateTabItems(session, "tabs", "home")
  })
  observeEvent(input$go_home_from_distributions, {
    updateTabItems(session, "tabs", "home")
  })
  observeEvent(input$go_home_from_continuous, {
    updateTabItems(session, "tabs", "home")
  })
  observeEvent(input$go_home_from_sampling, {
    updateTabItems(session, "tabs", "home")
  })
  observeEvent(input$go_home_from_inferential, {
    updateTabItems(session, "tabs", "home")
  })
  observeEvent(input$link_descriptive, {
    updateTabItems(session, "tabs", "descriptive")
  })
  observeEvent(input$link_discrete, {
    updateTabItems(session, "tabs", "distributions")
  })
  observeEvent(input$link_continuous, {
    updateTabItems(session, "tabs", "continuous")
  })
  observeEvent(input$link_sampling, {
    updateTabItems(session, "tabs", "sampling")
  })
  observeEvent(input$link_inferance, {
    updateTabItems(session, "tabs", "inferential")
  })
  
  # Descriptive Analysis Logic
  data <- reactive({
    if (input$data_source == "upload") {
      req(input$file)
      read.csv(input$file$datapath)
    } else {
      req(input$dataset)
      if(input$dataset == "diamonds") {
        ggplot2::diamonds  # Special case for diamonds dataset
      } else {
        get(input$dataset)
      }
    }
  })
  
  output$general_summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  # Output for data preview
  output$data_preview <- renderDT({
    req(data())
    DT::datatable(
      data(),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        serverSide = TRUE,  # Enable server-side processing
        processing = TRUE,  # Show processing indicator
        deferRender = TRUE, # Only render visible items
        scrollCollapse = TRUE,
        dom = 'tip'         # Only show table and processing info
      ),
      rownames = FALSE,
      filter = 'none'       # Disable client-side filtering
    )
  })
  
  
  # Variable selection UIs
  output$var_select <- renderUI({
    req(data())
    if (input$analysis_type == "Single Variable Analysis") {
      selectInput("variable", "Select a Variable:", choices = names(data()))
    }
  })
  
  output$var_select_2 <- renderUI({
    req(data())
    if (input$analysis_type == "Two Variables Analysis") {
      tagList(
        selectInput("variable", "Select First Variable:", choices = names(data())),
        selectInput("variable2", "Select Second Variable:", choices = names(data()))
      )
    }
  })
  
  output$var_select_3 <- renderUI({
    req(data())
    if (input$analysis_type == "Three Variables Analysis") {
      tagList(
        selectInput("variable", "Select First Variable:", choices = names(data())),
        selectInput("variable2", "Select Second Variable:", choices = names(data())),
        selectInput("variable3", "Select Third Variable:", choices = names(data()))
      )
    }
  })
  
  # Analysis logic
  observeEvent(input$analyze, {
    req(data())
    
    if (input$analysis_type == "Single Variable Analysis") {
      req(input$variable)
      var_data <- data()[[input$variable]]
      
      output$summary <- renderPrint({
        if (is.numeric(var_data)) {
          stats <- list(
            Mean = mean(var_data, na.rm = TRUE),
            Median = median(var_data, na.rm = TRUE),
            Mode = as.numeric(names(sort(table(var_data), decreasing = TRUE)[1])),
            Variance = var(var_data, na.rm = TRUE),
            SD = sd(var_data, na.rm = TRUE),
            Skewness = skewness(var_data, na.rm = TRUE),
            Kurtosis = kurtosis(var_data, na.rm = TRUE)
          )
          
          # Quantile calculation
          if(!is.null(input$quantile_prob)) {
            tryCatch({
              validate(
                need(input$quantile_prob >= 0 && input$quantile_prob <= 1,
                     "Quantile must be between 0 and 1")
              )
              
              q_type <- as.integer(input$quantile_type)
              stats$Selected_Quantile <- quantile(var_data, 
                                                  probs = input$quantile_prob,
                                                  na.rm = TRUE, 
                                                  type = q_type)
              
              # IQR calculation - FIXED EXTRA PARENTHESIS HERE
              if(input$show_iqr) {
                iqr <- quantile(var_data, probs = c(0.25, 0.75),
                                na.rm = TRUE, type = q_type)
                stats$IQR_25th <- iqr[1]
                stats$IQR_75th <- iqr[2]
                stats$IQR <- iqr[2] - iqr[1]
              }
              
            }, error = function(e) {
              stats$Error <<- "Invalid quantile input"
            })
          }
          stats
          
        } else {
          list(
            Frequency = table(var_data),
            Proportion = prop.table(table(var_data))
          )  # ADDED MISSING PARENTHESIS HERE
        }
      })
      
      output$plot_single <- renderPlot({
        if (is.numeric(var_data)) {
          par(mfrow = c(1, 2))
          hist(var_data, main = paste("Histogram of", input$variable), 
               col = "lightblue", border = "black", breaks = 10, xlab = input$variable)
          boxplot(var_data, main = paste("Boxplot of", input$variable), 
                  col = "lightblue", horizontal = TRUE)
        } else {
          par(mfrow = c(1, 2))
          barplot(table(var_data), main = paste("Bar Plot of", input$variable), 
                  col = brewer.pal(length(unique(var_data)), "Blues"))
          pie(table(var_data), main = paste("Pie Chart of", input$variable), 
              col = brewer.pal(length(unique(var_data)), "Blues"))
        }
      })
    }
    
    else if (input$analysis_type == "Two Variables Analysis") {
      req(input$variable, input$variable2)
      var1 <- data()[[input$variable]]
      var2 <- data()[[input$variable2]]
      
      output$summary2 <- renderPrint({
        if (is.numeric(var1) & is.numeric(var2)) {
          list(
            Pearson_Correlation = cor(var1, var2, method = "pearson", use = "complete.obs"),
            Spearman_Correlation = cor(var1, var2, method = "spearman", use = "complete.obs")
          )
        } else if (!is.numeric(var1) & !is.numeric(var2)) {
          list(
            Frequency_Distribution = table(var1, var2),
            Proportion_Distribution = prop.table(table(var1, var2))
          )
        }
      })
      
      output$plot_two <- renderPlot({
        if (is.numeric(var1) & is.numeric(var2)) {
          plot(var1, var2, main = paste("Scatter Plot of", input$variable, "vs", input$variable2), 
               pch = 19, col = "lightblue", xlab = input$variable, ylab = input$variable2)
        } else if (!is.numeric(var1) & !is.numeric(var2)) {
          par(mfrow = c(1, 2))
          barplot(table(var1, var2), main = paste("Stacked Bar Plot of", input$variable, "and", input$variable2), 
                  xlab = input$variable2, ylab = "Frequency", 
                  col = brewer.pal(length(table(var1, var2)[,1]), "Blues"))
          mosaicplot(table(var1, var2), main = paste("Mosaic Plot of", input$variable, "and", input$variable2), 
                     color = brewer.pal(length(table(var1, var2)[,1]), "Blues"), 
                     xlab = input$variable, ylab = input$variable2, cex.axis = 0.8)
        }
      })
    }
    
    else if (input$analysis_type == "Three Variables Analysis") {
      req(input$variable, input$variable2, input$variable3)
      var1 <- data()[[input$variable]]
      var2 <- data()[[input$variable2]]
      var3 <- data()[[input$variable3]]
      
      output$summary3 <- renderPrint({
        if (is.numeric(var1) & is.numeric(var2) & is.numeric(var3)) {
          numerical_vars <- data.frame(var1, var2, var3)
          colnames(numerical_vars) <- c(input$variable, input$variable2, input$variable3)
          correlation_matrix <- cor(numerical_vars, use = "complete.obs")
          return(correlation_matrix)
        } 
        else if (!is.numeric(var1) & !is.numeric(var2) & !is.numeric(var3)) {
          multi_contingency_table <- table(var1, var2, var3)
          return(list(
            multi_contingency_table = multi_contingency_table,
            multi_joint_proportion = prop.table(multi_contingency_table),
            flattened_table = ftable(var1 ~ var2 + var3)
          ))
        }
      })
      
      output$plot_three <- renderPlot({
        if (is.numeric(var1) & is.numeric(var2) & is.numeric(var3)) {
          numerical_vars <- data.frame(var1, var2, var3)
          colnames(numerical_vars) <- c(input$variable, input$variable2, input$variable3)
          pairs(numerical_vars, main = paste("Scatter Plot Matrix"), 
                pch = 19, col = "skyblue", cex = 0.5)
        } 
        else if (!is.numeric(var1) & !is.numeric(var2) & !is.numeric(var3)) {
          par(mfrow = c(1, 2))
          interaction_var <- interaction(var1, var3)
          barplot(table(interaction_var, var2), main = "Combined Bar Plot", 
                  xlab = input$variable2, ylab = "Frequency", 
                  col = brewer.pal(length(table(interaction_var)), "Blues"), beside = FALSE)
        }
      })
      
      output$plot_three_heatmap <- renderPlot({
        if (is.numeric(var1) & is.numeric(var2) & is.numeric(var3)) {
          numerical_vars <- data.frame(var1, var2, var3)
          colnames(numerical_vars) <- c(input$variable, input$variable2, input$variable3)
          corrplot(cor(numerical_vars, use = "complete.obs"), 
                   method = "circle", outline = TRUE, type = "full", diag = FALSE, 
                   col = colorRampPalette(brewer.pal(10, "RdYlBu"))(20), 
                   mar = c(1,1,0,0), tl.cex = 0.5, tl.col = "black", 
                   title = "Correlation Heatmap")
        } 
      })
    }
  })
  
  # Discrete Distributions Logic
  output$pois_input_ui <- renderUI({
    switch(input$calc_type_pois,
           "Probability Mass Function PMF" = numericInput("x_pois", "Value (x):", value = 0, min = 0),
           "Cumulative Probability P(X <= q)" = numericInput("q_pois_lower", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_pois_upper", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X >= q)" = numericInput("q_pois_geq", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_pois_less", "Quantile (q):", value = 0, min = 0),
           "Quantile Inverse CDF" = numericInput("p_pois", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_pois, {
    lambda <- input$lambda
    result <- switch(input$calc_type_pois,
                     "Probability Mass Function PMF" = dpois(input$x_pois, lambda),
                     "Cumulative Probability P(X <= q)" = ppois(input$q_pois_lower, lambda),
                     "Cumulative Probability P(X > q)" = ppois(input$q_pois_upper, lambda, lower.tail = FALSE),
                     "Cumulative Probability P(X >= q)" = 1 - ppois(input$q_pois_geq - 1, lambda),
                     "Cumulative Probability P(X < q)" = ppois(input$q_pois_less - 1, lambda),
                     "Quantile Inverse CDF" = qpois(input$p_pois, lambda)
    )
    output$pois_result <- renderPrint(cat("Result:", result, "\n"))
  })
  
  # Geometric Distribution Server Logic
  output$geom_input_ui <- renderUI({
    switch(input$calc_type_geom,
           "Probability Mass Function PMF" = numericInput("x_geom", "Value (x):", value = 1, min = 1),
           "Cumulative Probability P(X <= q)" = numericInput("q_geom_lower", "Quantile (q):", value = 1, min = 1),
           "Cumulative Probability P(X > q)" = numericInput("q_geom_upper", "Quantile (q):", value = 1, min = 1),
           "Cumulative Probability P(X >= q)" = numericInput("q_geom_geq", "Quantile (q):", value = 1, min = 1),
           "Cumulative Probability P(X < q)" = numericInput("q_geom_less", "Quantile (q):", value = 1, min = 1),
           "Quantile Inverse CDF" = numericInput("p_geom", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_geom, {
    prob <- input$prob
    result <- switch(input$calc_type_geom,
                     "Probability Mass Function PMF" = dgeom(input$x_geom - 1, prob),  # R uses 0-based index
                     "Cumulative Probability P(X <= q)" = pgeom(input$q_geom_lower - 1, prob),
                     "Cumulative Probability P(X > q)" = pgeom(input$q_geom_upper - 1, prob, lower.tail = FALSE),
                     "Cumulative Probability P(X >= q)" = 1 - pgeom(input$q_geom_geq - 2, prob),
                     "Cumulative Probability P(X < q)" = pgeom(input$q_geom_less - 2, prob),
                     "Quantile Inverse CDF" = qgeom(input$p_geom, prob) + 1  # Adjust to 1-based
    )
    output$geom_result <- renderPrint(cat("Result:", result, "\n"))
  })
  
  # Binomial Distribution Server Logic
  output$binom_input_ui <- renderUI({
    switch(input$calc_type_binom,
           "Probability Mass Function PMF" = numericInput("x_binom", "Value (x):", value = 0, min = 0),
           "Cumulative Probability P(X <= q)" = numericInput("q_binom_lower", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_binom_upper", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X >= q)" = numericInput("q_binom_geq", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_binom_less", "Quantile (q):", value = 0, min = 0),
           "Quantile Inverse CDF" = numericInput("p_binom", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_binom, {
    n <- input$n_binom
    p <- input$p_binom
    result <- switch(input$calc_type_binom,
                     "Probability Mass Function PMF" = dbinom(input$x_binom, n, p),
                     "Cumulative Probability P(X <= q)" = pbinom(input$q_binom_lower, n, p),
                     "Cumulative Probability P(X > q)" = pbinom(input$q_binom_upper, n, p, lower.tail = FALSE),
                     "Cumulative Probability P(X >= q)" = 1 - pbinom(input$q_binom_geq - 1, n, p),
                     "Cumulative Probability P(X < q)" = pbinom(input$q_binom_less - 1, n, p),
                     "Quantile Inverse CDF" = qbinom(input$p_binom, n, p)
    )
    output$binom_result <- renderPrint(cat("Result:", result, "\n"))
  })
  
  # Hypergeometric Distribution Server Logic
  output$hyper_input_ui <- renderUI({
    switch(input$calc_type_hyper,
           "Probability Mass Function PMF" = numericInput("x_hyper", "Value (x):", value = 0, min = 0),
           "Cumulative Probability P(X <= q)" = numericInput("q_hyper_lower", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_hyper_upper", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X >= q)" = numericInput("q_hyper_geq", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_hyper_less", "Quantile (q):", value = 0, min = 0),
           "Quantile Inverse CDF" = numericInput("p_hyper", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_hyper, {
    N <- input$N_hyper
    K <- input$K_hyper
    n <- input$n_hyper
    result <- switch(input$calc_type_hyper,
                     "Probability Mass Function PMF" = dhyper(input$x_hyper, K, N - K, n),
                     "Cumulative Probability P(X <= q)" = phyper(input$q_hyper_lower, K, N - K, n),
                     "Cumulative Probability P(X > q)" = phyper(input$q_hyper_upper, K, N - K, n, lower.tail = FALSE),
                     "Cumulative Probability P(X >= q)" = 1 - phyper(input$q_hyper_geq - 1, K, N - K, n),
                     "Cumulative Probability P(X < q)" = phyper(input$q_hyper_less - 1, K, N - K, n),
                     "Quantile Inverse CDF" = qhyper(input$p_hyper, K, N - K, n)
    )
    output$hyper_result <- renderPrint(cat("Result:", result, "\n"))
  })
  
  # Normal Distribution Server Logic
  output$norm_input_ui <- renderUI({
    switch(input$calc_type_norm,
           "Probability Density Function PDF" = numericInput("x_norm", "Value (x):", value = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_norm_lower", "Quantile (q):", value = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_norm_upper", "Quantile (q):", value = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_norm", "Lower Bound (a):", value = 0),
             numericInput("b_norm", "Upper Bound (b):", value = 1)
           ),
           "Conditional Probability P(X < b | X > a)" = tagList(
             numericInput("a_norm", "Condition Lower Bound (a):", value = 0),
             numericInput("b_norm", "Target Upper Bound (b):", value = 1)
           ),
           "Conditional Probability P(X < b | X < a)" = tagList(
             numericInput("a_norm", "Upper Condition (a):", value = 1),
             numericInput("b_norm", "Target (b):", value = 0.5)
           ),
           "Conditional Probability P(X > b | X < a)" = tagList(
             numericInput("a_norm", "Upper Condition (a):", value = 1),
             numericInput("b_norm", "Target (b):", value = 0.5)
           ),
           "Conditional Probability P(X > b | X > a)" = tagList(
             numericInput("a_norm", "Lower Condition (a):", value = 0),
             numericInput("b_norm", "Target (b):", value = 1)
           ),
           "Quantile Inverse CDF" = numericInput("p_norm", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_norm, {
    result <- switch(input$calc_type_norm,
                     "Probability Density Function PDF" = dnorm(input$x_norm, input$mean_norm, input$sd_norm),
                     "Cumulative Probability P(X < q)" = pnorm(input$q_norm_lower, input$mean_norm, input$sd_norm),
                     "Cumulative Probability P(X > q)" = pnorm(input$q_norm_upper, input$mean_norm, input$sd_norm, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_norm
                       b <- input$b_norm
                       if(b <= a) return("Error: b must be greater than a")
                       pnorm(b, input$mean_norm, input$sd_norm) - pnorm(a, input$mean_norm, input$sd_norm)
                     },
                     "Conditional Probability P(X <= b | X > a)" = {
                       a <- input$a_norm
                       b <- input$b_norm
                       if(b <= a) return("Error: b must be greater than a")
                       prob_a <- pnorm(a, input$mean_norm, input$sd_norm)
                       prob_b <- pnorm(b, input$mean_norm, input$sd_norm)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       (prob_b - prob_a)/(1 - prob_a)
                     },
                     "Conditional Probability P(X <= b | X < a)" = {
                       a <- input$a_norm
                       b <- input$b_norm
                       prob_a <- pnorm(a, input$mean_norm, input$sd_norm)
                       prob_b <- pnorm(b, input$mean_norm, input$sd_norm)
                       if(b < a) (prob_b/prob_a) else 1
                     },
                     "Conditional Probability P(X > b | X < a)" = {
                       a <- input$a_norm
                       b <- input$b_norm
                       prob_a <- pnorm(a, input$mean_norm, input$sd_norm)
                       prob_b <- pnorm(b, input$mean_norm, input$sd_norm)
                       if(b < a) (prob_a - prob_b)/prob_a else 0
                     },
                     "Conditional Probability P(X > b | X > a)" = {
                       a <- input$a_norm
                       b <- input$b_norm
                       prob_a <- pnorm(a, input$mean_norm, input$sd_norm)
                       prob_b <- pnorm(b, input$mean_norm, input$sd_norm)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       if(b >= a) (1 - prob_b)/(1 - prob_a) else 1
                     },
                     "Quantile Inverse CDF" = qnorm(input$p_norm, input$mean_norm, input$sd_norm)
    )
    
    output$norm_result <- renderPrint({
      if(is.character(result)) cat("Error:", result, "\n")
      else cat("Result:", round(result, 6), "\n")
    })
  })
  
  # Weibull Distribution Server Logic
  output$weibull_input_ui <- renderUI({
    switch(input$calc_type_weibull,
           "Probability Density Function PDF" = numericInput("x_weibull", "Value (x):", value = 0, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_weibull_lower", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_weibull_upper", "Quantile (q):", value = 0, min = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_weibull", "Lower Bound (a):", value = 0, min = 0),
             numericInput("b_weibull", "Upper Bound (b):", value = 1, min = 0)
           ),
           "Conditional Probability P(X < b | X > a)" = tagList(
             numericInput("a_weibull", "Condition Lower Bound (a):", value = 0, min = 0),
             numericInput("b_weibull", "Target Upper Bound (b):", value = 1, min = 0)
           ),
           "Conditional Probability P(X < b | X < a)" = tagList(
             numericInput("a_weibull", "Upper Condition (a):", value = 1, min = 0),
             numericInput("b_weibull", "Target (b):", value = 0.5, min = 0)
           ),
           "Conditional Probability P(X > b | X < a)" = tagList(
             numericInput("a_weibull", "Upper Condition (a):", value = 1, min = 0),
             numericInput("b_weibull", "Target (b):", value = 0.5, min = 0)
           ),
           "Conditional Probability P(X > b | X > a)" = tagList(
             numericInput("a_weibull", "Lower Condition (a):", value = 0, min = 0),
             numericInput("b_weibull", "Target (b):", value = 1, min = 0)
           ),
           "Quantile Inverse CDF" = numericInput("p_weibull", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_weibull, {
    result <- switch(input$calc_type_weibull,
                     "Probability Density Function PDF" = dweibull(input$x_weibull, input$shape_weibull, input$scale_weibull),
                     "Cumulative Probability P(X < q)" = pweibull(input$q_weibull_lower, input$shape_weibull, input$scale_weibull),
                     "Cumulative Probability P(X > q)" = pweibull(input$q_weibull_upper, input$shape_weibull, input$scale_weibull, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_weibull
                       b <- input$b_weibull
                       if(b <= a) return("Error: b must be greater than a")
                       pweibull(b, input$shape_weibull, input$scale_weibull) - pweibull(a, input$shape_weibull, input$scale_weibull)
                     },
                     "Conditional Probability P(X < b | X > a)" = {
                       a <- input$a_weibull
                       b <- input$b_weibull
                       if(b <= a) return("Error: b must be greater than a")
                       prob_a <- pweibull(a, input$shape_weibull, input$scale_weibull)
                       prob_b <- pweibull(b, input$shape_weibull, input$scale_weibull)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       (prob_b - prob_a)/(1 - prob_a)
                     },
                     "Conditional Probability P(X < b | X < a)" = {
                       a <- input$a_weibull
                       b <- input$b_weibull
                       if(a <= 0) return("Error: a must be positive")
                       prob_a <- pweibull(a, input$shape_weibull, input$scale_weibull)
                       prob_b <- pweibull(b, input$shape_weibull, input$scale_weibull)
                       if(b < a) (prob_b/prob_a) else 1
                     },
                     "Conditional Probability P(X > b | X < a)" = {
                       a <- input$a_weibull
                       b <- input$b_weibull
                       if(a <= 0) return("Error: a must be positive")
                       prob_a <- pweibull(a, input$shape_weibull, input$scale_weibull)
                       prob_b <- pweibull(b, input$shape_weibull, input$scale_weibull)
                       if(b < a) (prob_a - prob_b)/prob_a else 0
                     },
                     "Conditional Probability P(X > b | X > a)" = {
                       a <- input$a_weibull
                       b <- input$b_weibull
                       prob_a <- pweibull(a, input$shape_weibull, input$scale_weibull)
                       prob_b <- pweibull(b, input$shape_weibull, input$scale_weibull)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       if(b >= a) (1 - prob_b)/(1 - prob_a) else 1
                     },
                     "Quantile Inverse CDF" = qweibull(input$p_weibull, input$shape_weibull, input$scale_weibull)
    )
    
    output$weibull_result <- renderPrint({
      if(is.character(result)) cat("Error:", result, "\n")
      else cat("Result:", round(result, 6), "\n")
    })
  })
  
  # Lognormal Distribution Server Logic
  output$lognorm_input_ui <- renderUI({
    switch(input$calc_type_lognorm,
           "Probability Density Function PDF" = numericInput("x_lognorm", "Value (x):", value = 0, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_lognorm_lower", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_lognorm_upper", "Quantile (q):", value = 0, min = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_lognorm", "Lower Bound (a):", value = 0, min = 0),
             numericInput("b_lognorm", "Upper Bound (b):", value = 1, min = 0)
           ),
           "Conditional Probability P(X < b | X > a)" = tagList(
             numericInput("a_lognorm", "Condition Lower Bound (a):", value = 0, min = 0),
             numericInput("b_lognorm", "Target Upper Bound (b):", value = 1, min = 0)
           ),
           "Conditional Probability P(X < b | X < a)" = tagList(
             numericInput("a_lognorm", "Upper Condition (a):", value = 1, min = 0),
             numericInput("b_lognorm", "Target (b):", value = 0.5, min = 0)
           ),
           "Conditional Probability P(X > b | X < a)" = tagList(
             numericInput("a_lognorm", "Upper Condition (a):", value = 1, min = 0),
             numericInput("b_lognorm", "Target (b):", value = 0.5, min = 0)
           ),
           "Conditional Probability P(X > b | X > a)" = tagList(
             numericInput("a_lognorm", "Lower Condition (a):", value = 0, min = 0),
             numericInput("b_lognorm", "Target (b):", value = 1, min = 0)
           ),
           "Quantile Inverse CDF" = numericInput("p_lognorm", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_lognorm, {
    result <- switch(input$calc_type_lognorm,
                     "Probability Density Function PDF" = dlnorm(input$x_lognorm, input$meanlog_lognorm, input$sdlog_lognorm),
                     "Cumulative Probability P(X < q)" = plnorm(input$q_lognorm_lower, input$meanlog_lognorm, input$sdlog_lognorm),
                     "Cumulative Probability P(X > q)" = plnorm(input$q_lognorm_upper, input$meanlog_lognorm, input$sdlog_lognorm, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_lognorm
                       b <- input$b_lognorm
                       if(b <= a) return("Error: b must be greater than a")
                       plnorm(b, input$meanlog_lognorm, input$sdlog_lognorm) - plnorm(a, input$meanlog_lognorm, input$sdlog_lognorm)
                     },
                     "Conditional Probability P(X < b | X > a)" = {
                       a <- input$a_lognorm
                       b <- input$b_lognorm
                       if(b <= a) return("Error: b must be greater than a")
                       prob_a <- plnorm(a, input$meanlog_lognorm, input$sdlog_lognorm)
                       prob_b <- plnorm(b, input$meanlog_lognorm, input$sdlog_lognorm)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       (prob_b - prob_a)/(1 - prob_a)
                     },
                     "Conditional Probability P(X < b | X < a)" = {
                       a <- input$a_lognorm
                       b <- input$b_lognorm
                       if(a <= 0) return("Error: a must be positive")
                       prob_a <- plnorm(a, input$meanlog_lognorm, input$sdlog_lognorm)
                       prob_b <- plnorm(b, input$meanlog_lognorm, input$sdlog_lognorm)
                       if(b < a) (prob_b/prob_a) else 1
                     },
                     "Conditional Probability P(X > b | X < a)" = {
                       a <- input$a_lognorm
                       b <- input$b_lognorm
                       if(a <= 0) return("Error: a must be positive")
                       prob_a <- plnorm(a, input$meanlog_lognorm, input$sdlog_lognorm)
                       prob_b <- plnorm(b, input$meanlog_lognorm, input$sdlog_lognorm)
                       if(b < a) (prob_a - prob_b)/prob_a else 0
                     },
                     "Conditional Probability P(X > b | X > a)" = {
                       a <- input$a_lognorm
                       b <- input$b_lognorm
                       prob_a <- plnorm(a, input$meanlog_lognorm, input$sdlog_lognorm)
                       prob_b <- plnorm(b, input$meanlog_lognorm, input$sdlog_lognorm)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       if(b >= a) (1 - prob_b)/(1 - prob_a) else 1
                     },
                     "Quantile Inverse CDF" = qlnorm(input$p_lognorm, input$meanlog_lognorm, input$sdlog_lognorm)
    )
    
    output$lognorm_result <- renderPrint({
      if(is.character(result)) cat("Error:", result, "\n")
      else cat("Result:", round(result, 6), "\n")
    })
  })
  
  # Exponential Distribution Logic
  output$exp_input_ui <- renderUI({
    switch(input$calc_type_exp,
           "Probability Density Function PDF" = numericInput("x_exp", "Value (x):", value = 0, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_exp_lower", "Quantile (q):", value = 0, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_exp_upper", "Quantile (q):", value = 0, min = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_exp", "Lower Bound (a):", value = 0, min = 0),
             numericInput("b_exp", "Upper Bound (b):", value = 1, min = 0)
           ),
           "Conditional Probability P(X < b | X > a)" = tagList(
             numericInput("a_exp", "Condition Lower Bound (a):", value = 0, min = 0),
             numericInput("b_exp", "Target Upper Bound (b):", value = 1, min = 0)
           ),
           "Conditional Probability P(X < b | X < a)" = tagList(
             numericInput("a_exp", "Upper Condition (a):", value = 1, min = 0),
             numericInput("b_exp", "Target (b):", value = 0.5, min = 0)
           ),
           "Conditional Probability P(X > b | X < a)" = tagList(
             numericInput("a_exp", "Upper Condition (a):", value = 1, min = 0),
             numericInput("b_exp", "Target (b):", value = 0.5, min = 0)
           ),
           "Conditional Probability P(X > b | X > a)" = tagList(
             numericInput("a_exp", "Lower Condition (a):", value = 0, min = 0),
             numericInput("b_exp", "Target (b):", value = 1, min = 0)
           ),
           "Quantile Inverse CDF" = numericInput("p_exp", "Probability (p):", value = 0.5, min = 0, max = 1)
    )
  })
  observeEvent(input$calc_exp, {
    result <- switch(input$calc_type_exp,
                     "Probability Density Function PDF" = dexp(input$x_exp, input$rate_exp),
                     "Cumulative Probability P(X < q)" = pexp(input$q_exp_lower, input$rate_exp),
                     "Cumulative Probability P(X > q)" = pexp(input$q_exp_upper, input$rate_exp, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_exp
                       b <- input$b_exp
                       if(b <= a) return("Error: b must be greater than a")
                       pexp(b, input$rate_exp) - pexp(a, input$rate_exp)
                     },
                     "Conditional Probability P(X < b | X > a)" = {
                       a <- input$a_exp
                       b <- input$b_exp
                       if(b <= a) return("Error: b must be greater than a")
                       prob_a <- pexp(a, input$rate_exp)
                       prob_b <- pexp(b, input$rate_exp)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       (prob_b - prob_a)/(1 - prob_a)
                     },
                     "Conditional Probability P(X < b | X < a)" = {
                       a <- input$a_exp
                       b <- input$b_exp
                       prob_a <- pexp(a, input$rate_exp)
                       prob_b <- pexp(b, input$rate_exp)
                       if(a <= 0) return("Error: a must be positive")
                       if(b < a) (prob_b/prob_a) else 1
                     },
                     "Conditional Probability P(X > b | X < a)" = {
                       a <- input$a_exp
                       b <- input$b_exp
                       prob_a <- pexp(a, input$rate_exp)
                       prob_b <- pexp(b, input$rate_exp)
                       if(a <= 0) return("Error: a must be positive")
                       if(b < a) (prob_a - prob_b)/prob_a else 0
                     },
                     "Conditional Probability P(X > b | X > a)" = {
                       a <- input$a_exp
                       b <- input$b_exp
                       prob_a <- pexp(a, input$rate_exp)
                       prob_b <- pexp(b, input$rate_exp)
                       if(prob_a >= 1) return("Error: Invalid condition")
                       if(b >= a) (1 - prob_b)/(1 - prob_a) else 1
                     },
                     "Quantile Inverse CDF" = qexp(input$p_exp, input$rate_exp)
    )
    output$exp_result <- renderPrint({
      if(is.character(result)) {
        cat(result, "\n")
      } else {
        cat("Result:", round(result, 6), "\n")
      }
    })
  })
  
  
  # Normal Sampling Distribution
  output$snorm_input_ui <- renderUI({
    switch(input$calc_type_snorm,
           "Probability Density Function PDF" = numericInput("x_snorm", "Value (x):", value = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_snorm_lower", "Quantile (q):", value = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_snorm_upper", "Quantile (q):", value = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_snorm", "Lower Bound (a):", value = -1),
             numericInput("b_snorm", "Upper Bound (b):", value = 1)
           ),
           "Quantile Inverse CDF" = numericInput("p_snorm", "Probability (p):", value = 0.95, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_snorm, {
    # Calculate SD based on user choice
    if(input$sd_choice == "calc") {
      req(input$population_sd, input$sample_size)
      if(input$sample_size < 1) {
        output$snorm_result <- renderPrint("Error: Sample size must be ≥ 1")
        return()
      }
      sd <- input$population_sd / sqrt(input$sample_size)
    } else {
      req(input$sampling_sd)
      sd <- input$sampling_sd
    }
    
    result <- switch(input$calc_type_snorm,
                     "Probability Density Function PDF" = {
                       req(input$x_snorm)
                       dnorm(input$x_snorm, input$sampling_mean, sd)
                     },
                     "Cumulative Probability P(X < q)" = {
                       req(input$q_snorm_lower)
                       pnorm(input$q_snorm_lower, input$sampling_mean, sd)
                     },
                     "Cumulative Probability P(X > q)" = {
                       req(input$q_snorm_upper)
                       pnorm(input$q_snorm_upper, input$sampling_mean, sd, lower.tail = FALSE)
                     },
                     "Interval Probability P(a < X < b)" = {
                       req(input$a_snorm, input$b_snorm)
                       if(input$b_snorm <= input$a_snorm) return("Error: b must be greater than a")
                       pnorm(input$b_snorm, input$sampling_mean, sd) - 
                         pnorm(input$a_snorm, input$sampling_mean, sd)
                     },
                     "Quantile Inverse CDF" = {
                       req(input$p_snorm)
                       qnorm(input$p_snorm, input$sampling_mean, sd)
                     })
    
    output$snorm_result <- renderPrint({
      if(is.character(result)) {
        cat("Error:", result)
      } else if(is.numeric(result)) {
        cat("Result: ", round(result, 6),"\n", 
            if(input$sd_choice == "calc") cat("Sample Standard Error:", round(sd, 4),"\n"))
      } else {
        "Invalid calculation"
      }
    })
  })
  
  # Dynamic UI Inputs
  output$snorm_input_ui <- renderUI({
    switch(input$calc_type_snorm,
           "Probability Density Function PDF" = numericInput("x_snorm", "Value (x):", value = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_snorm_lower", "Quantile (q):", value = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_snorm_upper", "Quantile (q):", value = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_snorm", "Lower Bound (a):", value = -1),
             numericInput("b_snorm", "Upper Bound (b):", value = 1)
           ),
           "Quantile Inverse CDF" = numericInput("p_snorm", "Probability (p):", value = 0.95, min = 0, max = 1)
    )
  })
  
  # t-Distribution
  output$t_input_ui <- renderUI({
    switch(input$calc_type_t,
           "Probability Density Function PDF" = numericInput("x_t", "Value (x):", value = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_t_lower", "Quantile (q):", value = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_t_upper", "Quantile (q):", value = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_t", "Lower Bound (a):", value = -1),
             numericInput("b_t", "Upper Bound (b):", value = 1)
           ),
           "Quantile Inverse CDF" = numericInput("p_t", "Probability (p):", value = 0.95, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_t, {
    result <- switch(input$calc_type_t,
                     "Probability Density Function PDF" = dt(input$x_t, input$t_df),
                     "Cumulative Probability P(X < q)" = pt(input$q_t_lower, input$t_df),
                     "Cumulative Probability P(X > q)" = pt(input$q_t_upper, input$t_df, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_t
                       b <- input$b_t
                       if(b <= a) return("Error: b must be greater than a")
                       pt(b, input$t_df) - pt(a, input$t_df)
                     },
                     "Quantile Inverse CDF" = qt(input$p_t, input$t_df)
    )
    output$t_result <- renderPrint(format_result(result))
  })
  
  # F-Distribution
  output$f_input_ui <- renderUI({
    switch(input$calc_type_f,
           "Probability Density Function PDF" = numericInput("x_f", "Value (x):", value = 1, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_f_lower", "Quantile (q):", value = 1, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_f_upper", "Quantile (q):", value = 1, min = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_f", "Lower Bound (a):", value = 0.5),
             numericInput("b_f", "Upper Bound (b):", value = 1.5)
           ),
           "Quantile Inverse CDF" = numericInput("p_f", "Probability (p):", value = 0.95, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_f, {
    result <- switch(input$calc_type_f,
                     "Probability Density Function PDF" = df(input$x_f, input$df1, input$df2),
                     "Cumulative Probability P(X < q)" = pf(input$q_f_lower, input$df1, input$df2),
                     "Cumulative Probability P(X > q)" = pf(input$q_f_upper, input$df1, input$df2, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_f
                       b <- input$b_f
                       if(b <= a) return("Error: b must be greater than a")
                       pf(b, input$df1, input$df2) - pf(a, input$df1, input$df2)
                     },
                     "Quantile Inverse CDF" = qf(input$p_f, input$df1, input$df2)
    )
    output$f_result <- renderPrint(format_result(result))
  })
  
  # Chi-squared Distribution
  output$chisq_input_ui <- renderUI({
    switch(input$calc_type_chisq,
           "Probability Density Function PDF" = numericInput("x_chisq", "Value (x):", value = 1, min = 0),
           "Cumulative Probability P(X < q)" = numericInput("q_chisq_lower", "Quantile (q):", value = 1, min = 0),
           "Cumulative Probability P(X > q)" = numericInput("q_chisq_upper", "Quantile (q):", value = 1, min = 0),
           "Interval Probability P(a < X < b)" = tagList(
             numericInput("a_chisq", "Lower Bound (a):", value = 0.5),
             numericInput("b_chisq", "Upper Bound (b):", value = 1.5)
           ),
           "Quantile Inverse CDF" = numericInput("p_chisq", "Probability (p):", value = 0.95, min = 0, max = 1)
    )
  })
  
  observeEvent(input$calc_chisq, {
    result <- switch(input$calc_type_chisq,
                     "Probability Density Function PDF" = dchisq(input$x_chisq, input$chisq_df),
                     "Cumulative Probability P(X < q)" = pchisq(input$q_chisq_lower, input$chisq_df),
                     "Cumulative Probability P(X > q)" = pchisq(input$q_chisq_upper, input$chisq_df, lower.tail = FALSE),
                     "Interval Probability P(a < X < b)" = {
                       a <- input$a_chisq
                       b <- input$b_chisq
                       if(b <= a) return("Error: b must be greater than a")
                       pchisq(b, input$chisq_df) - pchisq(a, input$chisq_df)
                     },
                     "Quantile Inverse CDF" = qchisq(input$p_chisq, input$chisq_df)
    )
    output$chisq_result <- renderPrint(format_result(result))
  })
  #One Population Sample
  # Helper function for formatting results
  format_result <- function(result) {
    if(is.character(result)) {
      cat("Error:", result)
    } else if(is.numeric(result)) {
      if(abs(result) < 1e-4) result <- signif(result, 3)
      cat("Result:", round(result, 6))
    } else {
      "Invalid calculation"
    }
  }
  # --- dynamically update column selectors for uploaded files ---
  observeEvent(input$nc_file, {
    req(input$nc_file)
    df <- read.csv(input$nc_file$datapath)
    updateSelectInput(session, "nc_column", choices = names(df))
  })
  
  observeEvent(input$mean_file, { req(input$mean_file); df <- read.csv(input$mean_file$datapath); updateSelectInput(session, "mean_column", choices = names(df)) })
  
  
  observeEvent(input$var_file, {
    req(input$var_file)
    df <- read.csv(input$var_file$datapath)
    updateSelectInput(session, "var_column", choices = names(df))
  })
  
  observeEvent(input$prop_file, {
    req(input$prop_file)
    df <- read.csv(input$prop_file$datapath)
    updateSelectInput(session, "prop_column", choices = names(df))
  })
  
  # --- dynamically update column selectors for built-in datasets ---
  observeEvent(input$nc_dataset, {
    req(input$nc_dataset)
    df <- get(input$nc_dataset)
    updateSelectInput(session, "nc_dataset_col", choices = names(df))
  })
  
  observeEvent(input$mean_dataset, { req(input$mean_dataset); df <- get(input$mean_dataset); updateSelectInput(session, "mean_dataset_col", choices = names(df)) })
  
  
  observeEvent(input$var_dataset, {
    req(input$var_dataset)
    df <- get(input$var_dataset)
    updateSelectInput(session, "var_dataset_col", choices = names(df))
  })
  
  observeEvent(input$prop_dataset, {
    req(input$prop_dataset)
    df <- get(input$prop_dataset)
    updateSelectInput(session, "prop_dataset_col", choices = names(df))
  })
  
  
  # Data handling for Normality Check
  nc_data <- reactive({
    req(input$test_type == "Normality Check")
    if(input$nc_data_source == "file") {
      req(input$nc_file, input$nc_column)
      data <- read.csv(input$nc_file$datapath)
      data[[input$nc_column]]
    } else if(input$nc_data_source == "builtin") {
      req(input$nc_dataset, input$nc_dataset_col)
      data <- get(input$nc_dataset)
      data[[input$nc_dataset_col]]
    } else if(input$nc_data_source == "manual") {
      req(input$nc_manual_data)
      values <- as.numeric(unlist(strsplit(input$nc_manual_data, "[, \n]+")))
      validate(need(!any(is.na(values)), "Invalid data input"))
      values
    }
  })
  
  # Data handling for Mean Test
  mean_data <- reactive({
    req(input$test_type == "Location Parameter Test")
    if (input$mean_data_source == "file") {
      req(input$mean_file, input$mean_column)
      read.csv(input$mean_file$datapath)[[input$mean_column]]
    } else if (input$mean_data_source == "builtin") {
      req(input$mean_dataset, input$mean_dataset_col)
      get(input$mean_dataset)[[input$mean_dataset_col]]
    } else if (input$mean_data_source == "manual") {
      req(input$mean_manual_data)
      vals <- as.numeric(unlist(strsplit(input$mean_manual_data, "[, \n]+")))
      validate(need(!any(is.na(vals)), "Invalid manual input"))
      vals
    } else NULL
  })
  
  test_result <- reactiveVal(NULL)
  
  # Data handling for Variance Test
  var_data <- reactive({
    req(input$test_type == "Variance Test")
    if(input$var_data_source == "file") {
      req(input$var_file, input$var_column)
      data <- read.csv(input$var_file$datapath)
      data[[input$var_column]]
    } else if(input$var_data_source == "builtin") {
      req(input$var_dataset, input$var_dataset_col)
      data <- get(input$var_dataset)
      data[[input$var_dataset_col]]
    } else if(input$var_data_source == "manual") {
      req(input$var_manual_data)
      values <- as.numeric(unlist(strsplit(input$var_manual_data, "[, \n]+")))
      validate(need(!any(is.na(values)), "Invalid data input"))
      values
    }
  })
  
  # Data handling for Proportion Test
  prop_data <- reactive({
    req(input$test_type == "Proportion Test")
    if(input$prop_data_source == "file") {
      req(input$prop_file, input$prop_column, input$success_value)
      data <- read.csv(input$prop_file$datapath)
      values <- data[[input$prop_column]]
      if(is.numeric(values)) {
        successes <- sum(values == as.numeric(input$success_value))
      } else {
        successes <- sum(values == input$success_value)
      }
      return(list(x = successes, n = length(values)))
    } else if(input$prop_data_source == "builtin") {
      req(input$prop_dataset, input$prop_dataset_col, input$prop_success_value)
      data <- get(input$prop_dataset)
      values <- data[[input$prop_dataset_col]]
      if(is.numeric(values)) {
        successes <- sum(values == as.numeric(input$prop_success_value))
      } else {
        successes <- sum(values == input$prop_success_value)
      }
      return(list(x = successes, n = length(values)))
    } else if(input$prop_data_source == "manual") {
      req(input$prop_manual_data)
      values <- as.numeric(unlist(strsplit(input$prop_manual_data, "[, \n]+")))
      validate(need(all(values %in% c(0,1)), "Data must be 0s and 1s"))
      return(list(x = sum(values), n = length(values)))
    } else if(input$prop_data_source == "summary") {
      return(list(x = input$x_prop, n = input$n_prop))
    }
  })
  
  observeEvent(input$run, {
    tryCatch({
      if(input$test_type == "Normality Check") {
        data <- nc_data()
        test <- shapiro.test(data)
        test_result(test)
        output$result <- renderPrint({
          list(
            Test_Results = test,
            Data_Summary = summary(data),
            Data_Preview = head(data) 
          )
        })
      }
      
      else if (input$test_type == "Location Parameter Test") {
        mu_val <- if (input$mean_data_source == "summary") input$mu0 else if (input$mean_test_type == "Z-test") input$mu0_ztest else if (input$mean_test_type == "t-test") input$mu0_ttest else input$m0_wtest
        if (input$mean_data_source == "summary") {
          if (input$mean_test_type == "Z-test") {
            test <- zsum.test(mean.x = input$xbar, sigma.x = input$sigma, n.x = input$n_mean, mu = input$mu0, alternative = input$alternative, conf.level = 1 - input$alpha)
          } else {
            test <- tsum.test(mean.x = input$xbar, s.x = input$s, n.x = input$n_mean, mu = input$mu0, alternative = input$alternative, conf.level = 1 - input$alpha)
          }
        } else {
          data <- mean_data()
          if (input$mean_test_type == "Z-test") {
            test <- z.test(data, mu = mu_val, sigma.x = input$sigma_ztest, alternative = input$alternative, conf.level = 1 - input$alpha)
          } else if (input$mean_test_type == "t-test") {
            test <- t.test(data, mu = mu_val, alternative = input$alternative, conf.level = 1 - input$alpha)
          } else {
            test <- wilcox.test(data, mu= mu_val, alternative = input$alternative, conf.int = TRUE, conf.level = 1 - input$alpha)
          }
        }
        test_result(test)
        output$result <- renderPrint(test)
      }
      else if(input$test_type == "Variance Test") {
        data <- var_data()
        validate(need(length(data) >= 2, "Need at least 2 observations for variance test"))
        test <- varTest(data, sigma.squared = input$sigma0_sq, 
                        alternative = input$alternative, 
                        conf.level = 1 - input$alpha)
        test_result(test)
        output$result <- renderPrint(test)
      }
      else if(input$test_type == "Proportion Test") {
        if (input$prop_data_source == "summary"){
          if(input$prop_test_type == "Exact binomial test") {
            test <- binom.test(x = input$x_prop, n = input$n_prop, p = input$p0,
                               alternative = input$alternative,
                               conf.level = 1 - input$alpha)
          } else {
            test <- prop.test(x = input$x_prop, n = input$n_prop, p = input$p0,
                              alternative = input$alternative,
                              conf.level = 1 - input$alpha)
          }
        }
        else {
          data <- prop_data()
          if(input$prop_test_type == "Exact binomial test") {
            test <- binom.test(x = data$x, n = data$n, p = input$p0,
                               alternative = input$alternative,
                               conf.level = 1 - input$alpha)
          } else {
            test <- prop.test(x = data$x, n = data$n, p = input$p0,
                              alternative = input$alternative,
                              conf.level = 1 - input$alpha)
          }
        }
        
        test_result(test)
        output$result <- renderPrint(test)
      }
      
      # Generate conclusion
      output$conclusion <- renderUI({
        req(test_result())
        test <- test_result()
        pval <- test$p.value
        ci <- if (!is.null(test$conf.int)) test$conf.int else c(NA, NA)
        if (input$test_type == "Location Parameter Test"){
          mu_val <- if (input$mean_data_source == "summary") input$mu0 else if (input$mean_test_type == "Z-test") input$mu0_ztest else input$mu0_ttest
          conclusion <- if (pval < input$alpha) {
            paste0("Reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          }
          
          if (!is.null(ci) && all(!is.na(ci))) {
            ci_text <- paste0("[", round(ci[1], 4), ", ", round(ci[2], 4), "]")
            inside <- if (mu_val >= ci[1] && mu_val <= ci[2]) "INSIDE" else "OUTSIDE"
            conclusion <- paste(conclusion, paste0("\nH₀ value ", mu_val, " is ", inside, " the ", (1 - input$alpha)*100, "% CI ", ci_text))
          }
        } else if(input$test_type == "Variance Test"){
          conclusion <- if (pval < input$alpha) {
            paste0("Reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          }
          
          if (!is.null(ci) && all(!is.na(ci))) {
            ci_text <- paste0("[", round(ci[1], 4), ", ", round(ci[2], 4), "]")
            inside <- if (input$sigma0_sq >= ci[1] && input$sigma0_sq <= ci[2]) "INSIDE" else "OUTSIDE"
            conclusion <- paste(conclusion, paste0("\nH₀ value ", input$sigma0_sq, " is ", inside, " the ", (1 - input$alpha)*100, "% CI ", ci_text))
          }
        } else if(input$test_type == "Proportion Test"){
          conclusion <- if (pval < input$alpha) {
            paste0("Reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          }
          
          if (!is.null(ci) && all(!is.na(ci))) {
            ci_text <- paste0("[", round(ci[1], 4), ", ", round(ci[2], 4), "]")
            inside <- if (input$p0 >= ci[1] && input$p0 <= ci[2]) "INSIDE" else "OUTSIDE"
            conclusion <- paste(conclusion, paste0("\nH₀ value ", input$p0, " is ", inside, " the ", (1 - input$alpha)*100, "% CI ", ci_text))
          }
        } else if (input$test_type == "Normality Check"){
          conclusion <- if (pval < input$alpha) {
            paste0("Reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$alpha, " (p = ", round(pval, 4), ")")
          }
          
          
        }
        
        
        HTML(gsub("\n", "<br>", conclusion))
      })
    })
  })
  #Two Population Samples
  # --- dynamically populate column selects on upload or builtin ---
  observeEvent(input$two_nc_file, {
    req(input$two_nc_file)
    two_cols <- names(read.csv(input$two_nc_file$datapath))
    updateSelectInput(session, "two_nc_column1", choices = two_cols)
    updateSelectInput(session, "two_nc_column2", choices = two_cols)
  })
  observeEvent(input$two_nc_dataset, {
    two_df <- get(input$two_nc_dataset)
    updateSelectInput(session, "two_nc_dataset_col1", choices = names(two_df))
    updateSelectInput(session, "two_nc_dataset_col2", choices = names(two_df))
  })
  
  observeEvent(input$two_mean_file, {
    req(input$two_mean_file)
    two_cols <- names(read.csv(input$two_mean_file$datapath))
    updateSelectInput(session, "two_mean_col1", choices = two_cols)
    updateSelectInput(session, "two_mean_col2", choices = two_cols)
  })
  observeEvent(input$two_mean_dataset, {
    two_df <- get(input$two_mean_dataset)
    updateSelectInput(session, "two_mean_dataset_col1", choices = names(two_df))
    updateSelectInput(session, "two_mean_dataset_col2", choices = names(two_df))
  })
  
  observeEvent(input$two_var_file, {
    req(input$two_var_file)
    two_cols <- names(read.csv(input$two_var_file$datapath))
    updateSelectInput(session, "two_var_col1", choices = two_cols)
    updateSelectInput(session, "two_var_col2", choices = two_cols)
  })
  observeEvent(input$two_var_dataset, {
    two_df <- get(input$two_var_dataset)
    updateSelectInput(session, "two_var_dataset_col1", choices = names(two_df))
    updateSelectInput(session, "two_var_dataset_col2", choices = names(two_df))
  })
  
  two_prop_data <- reactive({
    req(input$two_test_type == "Proportion Test")
    req(input$two_prop_manual_data, input$two_prop_manual_n)
    x <- as.numeric(strsplit(input$two_prop_manual_data, ",")[[1]])
    n <- as.numeric(strsplit(input$two_prop_manual_n, ",")[[1]])
    list(x = x, n = n)
  })
  
  # --- helper to pull x & y from any of the three sources ---
  get_samples <- function(src, file, col1, col2, ds, ds1, ds2, man1, man2) {
    if (src == "file") {
      two_df <- read.csv(file$datapath)
      x  <- two_df[[col1]] 
      y  <- two_df[[col2]]
    } else if (src == "builtin") {
      d  <- get(ds)
      x  <- d[[ds1]]
      y  <- d[[ds2]]
    } else {
      x  <- as.numeric(strsplit(man1, "[, \\n]+")[[1]])
      y  <- as.numeric(strsplit(man2, "[, \\n]+")[[1]])
    }
    list(x = x, y = y)
  }
  
  observeEvent(input$two_run, {
    
    α   <- input$two_alpha
    alt <- if (is.null(input$alternative)) "two.sided" else input$alternative
    res <- NULL
    concl <- NULL
    
    # ---- Normality Check ----
    if (input$two_test_type == "Normality Check") {
      d <- get_samples(
        input$two_nc_data_source,
        input$two_nc_file,           # fileInput obj
        input$two_nc_column1,        # col for X
        input$two_nc_column2,        # col for Y
        input$two_nc_dataset,        # dataset name
        input$two_nc_dataset_col1,   # col for X
        input$two_nc_dataset_col2,   # col for Y
        input$two_nc_manual1,
        input$two_nc_manual2
      )
      t1 <- shapiro.test(d$x)
      t2 <- shapiro.test(d$y)
      res <- list(SampleX = t1, SampleY = t2)
      if (t1$p.value > α && t2$p.value > α) {
        concl <- paste0("<ul>",
                        "<li><b>X : </b> Fail to reject H₀ at α = ", α, " (p = ", round(t1$p.value, 4), ")</li>",
                        "<li><b>Y : </b> Fail to reject H₀ at α = ", α, " (p = ", round(t2$p.value, 4), ")</li>",
                        "</ul>"
        )
      } else if (t1$p.value > α && t2$p.value < α){
        concl <- paste0("<ul>",
                        "<li><b>X : </b> Fail to reject H₀ at α = ", α, " (p = ", round(t1$p.value, 4), ")</li>",
                        "<li><b>Y : </b> Reject H₀ at α = ", α, " (p = ", round(t2$p.value, 4), ")</li>",
                        "</ul>"
        )
      } else if (t1$p.value < α && t2$p.value < α){
        concl <- paste0("<ul>",
                        "<li><b>X : </b> Reject H₀ at α = ", α, " (p = ", round(t1$p.value, 4), ")</li>",
                        "<li><b>Y : </b> Reject H₀ at α = ", α, " (p = ", round(t2$p.value, 4), ")</li>",
                        "</ul>"
        )
      } else if (t1$p.value < α && t2$p.value > α){
        concl <- paste0("<ul>",
                        "<li><b>X : </b> Reject H₀ at α = ", α, " (p = ", round(t1$p.value, 4), ")</li>",
                        "<li><b>Y : </b> Fail to reject H₀ at α = ", α, " (p = ", round(t2$p.value, 4), ")</li>",
                        "</ul>"
        )
      }
      
    }
    
    # ---- Location Parameter Test ----
    else if (input$two_test_type == "Location Parameter Test") {
      
      # summary-stat branch
      if (input$two_mean_data_source == "summary") {
        if (input$two_mean_test_type == "Z-test  (Independent)" && input$two_sig1_known=="Yes" && input$two_sig2_known=="Yes") {
          z <- zsum.test(
            mean.x = input$two_xbar1, sigma.x = input$sigma1, n.x = input$two_n1,
            mean.y = input$two_xbar2, sigma.y = input$sigma2, n.y = input$two_n2,
            mu = input$two_mu0_diff, alternative = alt, conf.level = 1 - α
          )
          res <- z
        } else {
          t <- tsum.test(
            mean.x = input$two_xbar1, s.x = input$s1, n.x = input$two_n1,
            mean.y = input$two_xbar2, s.y = input$s2, n.y = input$two_n2,
            mu = input$two_mu0_diff, alternative = alt, conf.level = 1 - α
          )
          res <- t
        }
      }
      
      # raw-data branch
      else {
        d  <- get_samples(
          input$two_mean_data_source,
          input$two_mean_file,
          input$two_mean_col1,
          input$two_mean_col2,
          input$two_mean_dataset,
          input$two_mean_dataset_col1,
          input$two_mean_dataset_col2,
          input$two_mean_manual1,
          input$two_mean_manual2
        )
        x  <- d$x; y <- d$y
        res <- switch(input$two_mean_test_type,
                      `Z-test  (Independent)`   = z.test(x, y,
                                                  sigma.x = input$two_sigma_z1, sigma.y = input$two_sigma_z2,
                                                  mu = input$two_mu0_diff, alternative = alt, conf.level = 1-α),
                      `t-test (Independent & equal variance)` = t.test(x, y, var.equal = TRUE,
                                                  mu = input$two_mu0_diff, alternative = alt, conf.level = 1-α),
                      `t-test (Independent & not equal variance)`     = t.test(x, y, var.equal = FALSE,
                                                  mu = input$two_mu0_diff, alternative = alt, conf.level = 1-α),
                      `t-test (Paired)`    = t.test(x, y, paired = TRUE,
                                                  mu = input$two_mu0_diff, alternative = alt, conf.level = 1-α),
                      `Wilcoxon (Independent)` = wilcox.test(x, y,
                                                       alternative = alt, conf.int = TRUE, conf.level = 1-α),
                      `Wilcoxon (Paired)` = wilcox.test(x, y, paired = TRUE,
                                                      alternative = alt, conf.int = TRUE, conf.level = 1-α)
        )
      }
      
      if (res$p.value > α) {
        concl <- paste0("Fail to reject H₀ at α = ", α, " (p = ", round(res$p.value, 4), ")")
      } else{
        concl <- paste0("Reject H₀ at α = ", α, " (p = ", round(res$p.value, 4), ")")
      } 
    }
    
    # ---- Variance Test ----
    else if (input$two_test_type == "Variance Test") {
      d <- get_samples(
        input$two_var_data_source,
        input$two_var_file,
        input$two_var_col1,
        input$two_var_col2,
        input$two_var_dataset,
        input$two_var_dataset_col1,
        input$two_var_dataset_col2,
        input$two_var_manual1,
        input$two_var_manual2
      )
      x <- d$x; y <- d$y
      res <- switch(input$two_var_test_type,
                    `F-test`     = var.test(x, y, ratio = input$two_theta0, alternative = alt, conf.level = 1-α),
                    `Mood's test`= mood.test(x, y, alternative = alt, conf.int = TRUE, conf.level = 1-α)
      )
      if (res$p.value > α) {
        concl <- paste0("Fail to reject H₀ at α = ", α, " (p = ", round(res$p.value, 4), ")")
      } else{
        concl <- paste0("Reject H₀ at α = ", α, " (p = ", round(res$p.value, 4), ")")
      } 
    }
    
    # ---- Proportion Test ----
    else if(input$two_test_type == "Proportion Test") {
      
      data <- two_prop_data()
      validate(
        need(length(data$x) == length(data$n), "Number of successes and totals must match"),
        need(all(data$x <= data$n), "Successes cannot exceed totals")
      )
      test <- prop.test(x = data$x, n = data$n, conf.level = 1 - input$two_alpha)
      
      output$two_result <- renderPrint({
        cat("============= PROPORTION TEST DATA ==============\n")
        cat("Successes:", paste(data$x, collapse=", "), "\n")
        cat("Totals:   ", paste(data$n, collapse=", "), "\n")
        cat("\n================ TEST RESULTS ================\n")
        print(test)
      })
      
      output$two_conclusion <- renderUI({
        HTML(paste0(
          "<b>Conclusion: </b>", 
          if (test$p.value < input$two_alpha) {
            paste0("Reject H₀ at α = ", input$two_alpha, " (p = ", round(test$p.value, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$two_alpha, " (p = ", round(test$p.value, 4), ")")
          }
        ))
      })
      return()
    }
    
    # Default output processing for all other test types
    output$two_result <- renderPrint({
      if (!is.null(res)) print(res)
    })
    
    output$two_conclusion <- renderUI({
      if (!is.null(concl)) HTML(paste0("<b>Conclusion:</b> ", concl))
    })
  })
  #Three Population Samples
  # Update column selectors
  observe({
    # Normality Check
    if(input$three_test_type == "Normality Check" && input$three_nc_data_source == "file" && !is.null(input$three_nc_file)) {
      df <- read.csv(input$three_nc_file$datapath)
      updateSelectizeInput(session, "three_nc_columns", choices = names(df))
    }
    
    # Mean Test
    if(input$three_test_type == "Location Parameter Test" && input$three_mean_data_source == "file" && !is.null(input$three_mean_file)) {
      df <- read.csv(input$three_mean_file$datapath)
      updateSelectInput(session, "three_mean_response_file", choices = names(df))
      updateSelectInput(session, "three_mean_group_file", choices = names(df))
      updateSelectizeInput(session, "three_mean_cols_file", choices = names(df))
    }
    
    # Variance Test
    if(input$three_test_type == "Variance Test" && input$three_var_data_source == "file" && !is.null(input$three_var_file)) {
      df <- read.csv(input$three_var_file$datapath)
      updateSelectInput(session, "three_var_response_file", choices = names(df))
      updateSelectInput(session, "three_var_group_file", choices = names(df))
      updateSelectizeInput(session, "three_var_cols_file", choices = names(df))
    }
  })
  
  # Update built-in dataset columns
  observe({
    # Normality Check
    if(input$three_test_type == "Normality Check" && input$three_nc_data_source == "builtin") {
      df <- get(input$three_nc_dataset)
      updateSelectizeInput(session, "three_nc_dataset_cols", choices = names(df))
    }
    
    # Mean Test
    if(input$three_test_type == "Location Parameter Test" && input$three_mean_data_source == "builtin") {
      df <- get(input$three_mean_dataset)
      updateSelectInput(session, "three_mean_response_builtin", choices = names(df))
      updateSelectInput(session, "three_mean_group_builtin", choices = names(df))
      updateSelectizeInput(session, "three_mean_cols_builtin", choices = names(df))
    }
    
    # Variance Test
    if(input$three_test_type == "Variance Test" && input$three_var_data_source == "builtin") {
      df <- get(input$three_var_dataset)
      updateSelectInput(session, "three_var_response_builtin", choices = names(df))
      updateSelectInput(session, "three_var_group_builtin", choices = names(df))
      updateSelectizeInput(session, "three_var_cols_builtin", choices = names(df))
    }
    
  })
  
  test_three_result <- reactiveVal(NULL)
  
  # Data handling
  three_nc_data <- reactive({
    req(input$three_test_type == "Normality Check")
    if(input$three_nc_data_source == "file") {
      req(input$three_nc_file, input$three_nc_columns)
      df <- read.csv(input$three_nc_file$datapath)
      lapply(input$three_nc_columns, function(col) df[[col]])
    } else if(input$three_nc_data_source == "builtin") {
      req(input$three_nc_dataset, input$three_nc_dataset_cols)
      df <- get(input$three_nc_dataset)
      lapply(input$three_nc_dataset_cols, function(col) df[[col]])
    } else if(input$three_nc_data_source == "manual") {
      req(input$three_nc_manual_data)
      groups <- strsplit(input$three_nc_manual_data, ";")[[1]]
      lapply(groups, function(g) as.numeric(unlist(strsplit(trimws(g), "[, ]+"))))
    }
  })
  
  three_mean_data <- reactive({
    req(input$three_test_type == "Location Parameter Test")
    
    if (input$three_mean_data_source == "file") {
      df <- read.csv(input$three_mean_file$datapath)
      if (input$three_mean_input_mode_file == "group") {
        req(input$three_mean_response_file, input$three_mean_group_file)
        list(type = "group", response = df[[input$three_mean_response_file]], group = df[[input$three_mean_group_file]])
      } else {
        req(input$three_mean_cols_file)
        list(type = "cols", data = df[input$three_mean_cols_file])
      }
    } else if (input$three_mean_data_source == "builtin") {
      df <- get(input$three_mean_dataset)
      if (input$three_mean_input_mode_builtin == "group") {
        req(input$three_mean_response_builtin, input$three_mean_group_builtin)
        list(type = "group", response = df[[input$three_mean_response_builtin]], group = df[[input$three_mean_group_builtin]])
      } else {
        req(input$three_mean_cols_builtin)
        list(type = "cols", data = df[input$three_mean_cols_builtin])
      }
    } else if (input$three_mean_data_source == "manual") {
      req(input$three_mean_manual_data)
      groups <- strsplit(input$three_mean_manual_data, ";")[[1]]
      data_list <- lapply(groups, function(g) as.numeric(unlist(strsplit(trimws(g), "[, ]+"))))
      df_long <- stack(setNames(data_list, paste0("Group", seq_along(data_list))))
      list(type = "manual", data = df_long)
    }
  })
  
  
  
  three_var_data <- reactive({
    req(input$three_test_type == "Variance Test")
    
    if (input$three_var_data_source == "file") {
      df <- read.csv(input$three_var_file$datapath)
      if (input$three_var_input_mode_file == "group") {
        req(input$three_var_response_file, input$three_var_group_file)
        list(type = "group", response = df[[input$three_var_response_file]], group = df[[input$three_var_group_file]])
      } else {
        req(input$three_var_cols_file)
        list(type = "cols", data = df[input$three_var_cols_file])
      }
    } else if (input$three_var_data_source == "builtin") {
      df <- get(input$three_var_dataset)
      if (input$three_var_input_mode_builtin == "group") {
        req(input$three_var_response_builtin, input$three_var_group_builtin)
        list(type = "group", response = df[[input$three_var_response_builtin]], group = df[[input$three_var_group_builtin]])
      } else {
        req(input$three_var_cols_builtin)
        list(type = "cols", data = df[input$three_var_cols_builtin])
      }
    } else if (input$three_var_data_source == "manual") {
      req(input$three_var_manual_data)
      groups <- strsplit(input$three_var_manual_data, ";")[[1]]
      data_list <- lapply(groups, function(g) as.numeric(unlist(strsplit(trimws(g), "[, ]+"))))
      df_long <- stack(setNames(data_list, paste0("Group", seq_along(data_list))))
      list(type = "manual", data = df_long)
    }
  })
  
  
  
  three_prop_data <- reactive({
    req(input$three_test_type == "Proportion Test")
    req(input$three_prop_manual_data, input$three_prop_manual_n)
    x <- as.numeric(strsplit(input$three_prop_manual_data, ",")[[1]])
    n <- as.numeric(strsplit(input$three_prop_manual_n, ",")[[1]])
    list(x = x, n = n)
  })
  
  observeEvent(input$three_run, {
    tryCatch({
      if(input$three_test_type == "Normality Check") {
        three_data_list <- three_nc_data()
        three_tests <- lapply(three_data_list, function(x) shapiro.test(x))
        output$three_result <- renderPrint({
          cat("Normality Check Results\n")
          cat("=======================\n\n")
          
          # Use a regular for loop instead of lapply
          for(i in 1:length(three_tests)) {
            three_group_name <- if(length(three_data_list) > 1) paste("Group", i) else "Data"
            three_test <- three_tests[[i]]
            data <- three_data_list[[i]]
            
            # Create separator line
            sep_line <- paste(rep("-", nchar(three_group_name) + 2), collapse = "")
            
            cat(three_group_name, "\n")
            cat(sep_line, "\n")
            cat("Shapiro-Wilk Test:\n")
            cat("W =", round(three_test$statistic, 4), "\n")
            cat("p-value =", format.pval(three_test$p.value, digits = 4), "\n\n")
            cat("\n", paste(rep("=", 50)), "\n\n")
          }
        })
        test_three_result(three_tests)
      }
      else if(input$three_test_type == "Location Parameter Test") {
        three_data <- three_mean_data()
        
        output$three_result <- renderPrint({
          cat("=============== LOCATION PARAMETER TEST DATA ===============\n")
          
          if(three_data$type == "group") {
            cat("Selected Response and Group Sample:\n")
            print(head(data.frame(Response = three_data$response, Group = three_data$group)))
            cat("\n====================== TEST RESULTS ======================\n")
            if(input$three_mean_test_type == "anova") {
              test <- aov(three_data$response ~ factor(three_data$group))
              print(summary(test))
              test_three_result(test)
            } else {
              test <- kruskal.test(three_data$response ~ factor(three_data$group))
              print(test)
              test_three_result(test)
            }
            
          } else if(three_data$type == "cols") {
            cat("Selected Columns Sample:\n")
            print(head(three_data$data))
            cat("\n====================== TEST RESULTS ======================\n")
            if(input$three_mean_test_type == "anova") {
              df_long <- stack(three_data$data)
              test <- aov(values ~ ind, data = df_long)
              print(summary(test))
              test_three_result(test)
            } else {
              df_long <- stack(three_data$data)
              test <- kruskal.test(values ~ ind, data = df_long)
              print(test)
              test_three_result(test)
            }
          } else if (three_data$type == "manual") {
            cat("Manual Entry Data Sample:\n")
            print(head(three_data$data))
            cat("\n====================== TEST RESULTS ======================\n")
            if (input$three_mean_test_type == "anova") {
              test <- aov(values ~ ind, data = three_data$data)
              print(summary(test))
              test_three_result(test)
            } else {
              test <- kruskal.test(values ~ ind, data = three_data$data)
              print(test)
              test_three_result(test)
            }
          }
        })
      }
      else if (input$three_test_type == "Variance Test") {
        three_data <- three_var_data()
        
        output$three_result <- renderPrint({
          cat("============= VARIANCE TEST DATA ==============\n")
          
          if (three_data$type == "group") {
            cat("Selected Response and Group Sample:\n")
            df_print <- data.frame(Response = three_data$response, Group = three_data$group)
            print(head(df_print))
            
            if (input$three_var_test_type == "bartlett") {
              test <- bartlett.test(three_data$response ~ factor(three_data$group))
            } else {
              test <- fligner.test(three_data$response ~ factor(three_data$group))
            }
            
            cat("\n================ TEST RESULTS ================\n")
            print(test)
            test_three_result(test)
            
          } else if (three_data$type == "cols") {
            cat("Selected Columns Sample:\n")
            print(head(three_data$data))
            
            # Reshape to long format
            df_long <- stack(three_data$data)
            
            if (input$three_var_test_type == "bartlett") {
              test <- bartlett.test(values ~ ind, data = df_long)
            } else {
              test <- fligner.test(values ~ ind, data = df_long)
            }
            
            cat("\n================ TEST RESULTS ================\n")
            print(test)
            test_three_result(test)
          } else if (three_data$type == "manual") {
            cat("Manual Entry Data Sample:\n")
            print(head(three_data$data))
            
            if (input$three_var_test_type == "bartlett") {
              test <- bartlett.test(values ~ ind, data = three_data$data)
            } else {
              test <- fligner.test(values ~ ind, data = three_data$data)
            }
            
            cat("\n================ TEST RESULTS ================\n")
            print(test)
            test_three_result(test)
          }
          
        })
      }
      
      else if(input$three_test_type == "Proportion Test") {
        three_data <- three_prop_data()
        validate(
          need(length(three_data$x) == length(three_data$n), "Number of successes and totals must match"),
          need(all(three_data$x <= three_data$n), "Successes cannot exceed totals")
        )
        test <- prop.test(x = three_data$x, n = three_data$n, conf.level = 1 - input$three_alpha)
        output$three_result <- renderPrint({
          cat("============= PROPORTION TEST DATA ==============\n")
          cat("Successes:", paste(three_data$x, collapse=", "), "\n")
          cat("Totals:   ", paste(three_data$n, collapse=", "), "\n")
          cat("\n================= TEST RESULTS =================\n")
          print(test)
        })
        test_three_result(test)
      }
      
      # Generate conclusion
      output$three_conclusion <- renderUI({
        req(test_three_result())
        three_tests <- test_three_result()
        
        # Handle multiple groups for normality check
        if(input$three_test_type == "Normality Check") {
          three_conclusions <- lapply(1:length(three_tests), function(i) {
            pval <- three_tests[[i]]$p.value
            group_text <- if(length(three_tests) > 1) paste("Group", i) else "Data"
            
            if(pval < input$three_alpha) {
              paste0(group_text, ": Reject H₀ at α = ", input$three_alpha, 
                     " (p = ", round(pval, 4), ")")
            } else {
              paste0(group_text, ": Fail to reject H₀ at α = ", input$three_alpha,
                     " (p = ", round(pval, 4), ")")
            }
          })
          
          three_conclusion <- paste(three_conclusions, collapse = "<br>")
        } else if(input$three_test_type == "Location Parameter Test") {
          test <- three_tests
          pval <- tryCatch({
            if(input$three_mean_test_type == "anova") {
              # Extract p-value from ANOVA summary
              summary(test)[[1]]$`Pr(>F)`[1]
            } else {
              # Kruskal-Wallis test
              test$p.value
            }
          }, error = function(e) NA_real_)
          
          validate(need(!is.na(pval), "Could not determine p-value"))
          
          three_conclusion <- if (pval < input$three_alpha) {
            paste0("Reject H₀ at α = ", input$three_alpha, " (p = ", round(pval, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$three_alpha, " (p = ", round(pval, 4), ")")
          }
          
          HTML(three_conclusion)
        } else {
          test <- three_tests
          pval <- test$p.value
          
          three_conclusion <- if (pval < input$three_alpha) {
            paste0("Reject H₀ at α = ", input$three_alpha, " (p = ", round(pval, 4), ")")
          } else {
            paste0("Fail to reject H₀ at α = ", input$three_alpha, " (p = ", round(pval, 4), ")")
          }
        }
        
        HTML(gsub("\n", "<br>",three_conclusion))
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Clear outputs when test type changes
  observeEvent(input$three_test_type, {
    output$three_result <- renderPrint({ })
    output$three_conclusion <- renderUI({ })
  })
}
# Run the application
shinyApp(ui, server)