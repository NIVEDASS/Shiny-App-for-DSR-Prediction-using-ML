shinyUI(
  fluidPage(#theme = shinytheme("cerulean"),
    
    inlineCSS("#Mosaic { font-size:30px; }
                 .Mosaic { color: red; border: 1px solid black;}
                  .navbar-default { background-color: #9615DB !important;}"),
    theme = shinytheme("cerulean"), 
    #shinythemes::themeSelector(),
    useShinyjs(),
  # Application title
  navbarPage("DSR  Prediction for Sensitivity Analysis",
  #navbarPage("",
    
    # tabPanel("Select Station",
    #          br(),
    #          
  sidebarLayout(
    #tabPanel("Select Station",
    br(),
    
    sidebarPanel(
             selectInput(inputId = "station_code", label ="Station Code:", choices = list("APA" = "APA",
                                                                                          "APP" = "APP",
                                                                                          "ASH" = "ASH",
                                                                                          "ASY" = "ASY",
                                                                                          "ATH" = "ATH",
                                                                                          "AWV" = "AWV",
                                                                                          "BGO" = "BGO",
                                                                                          "BML" = "BML",
                                                                                          "BMT" = "BMT",
                                                                                          "BPO" = "BPO",
                                                                                          "BRP" = "BRP",
                                                                                          "BTL" = "BTL",
                                                                                          "Bucklands_Raws" = "Bucklands_Raws",
                                                                                          "BUR" = "BUR",
                                                                                          "Butchers_Dam_Raws" = "Butchers_Dam_Raws",
                                                                                          "Cape Campbell" = "Cape Campbell",
                                                                                          "CDT" = "CDT",
                                                                                          "CHA" = "CHA",
                                                                                          "CLV" = "CLV",
                                                                                          "CLY" = "CLY",
                                                                                          "COR" = "COR",
                                                                                          "CPX" = "CPX",
                                                                                          "CRK" = "CRK",
                                                                                          "CRM" = "CRM",
                                                                                          "CRT" = "CRT",
                                                                                          "CYB" = "CYB",
                                                                                          "DAR" = "DAR",
                                                                                          "DNA" = "DAR",
                                                                                          "DNP" = "DNP",
                                                                                          "DOV" = "DOV",
                                                                                          "DPS" = "DPS",
                                                                                          "ELT" = "ELT",
                                                                                          "GAL" = "GAL",
                                                                                          "GBI" = "GBI",
                                                                                          "GLD" = "GLD",
                                                                                          "GSA" = "GSA",
                                                                                          "GWA" = "GWA",
                                                                                          "HAN" = "HAN",
                                                                                          "HAW" = "HAW",
                                                                                          "Hicks Bay SYNOP" = "Hicks Bay SYNOP",
                                                                                          "HIR" = "HIR",
                                                                                          "HKA" = "HKA",
                                                                                          "HNA" = "HNA",
                                                                                          "HOK" = "HOK",
                                                                                          "HRB" = "HRB",
                                                                                          "HWT" = "HWT",
                                                                                          "KAI" = "KAI",
                                                                                          "KAW" = "KAW",
                                                                                          "KHD" = "KHD",
                                                                                          "KIX" = "KIX",
                                                                                          "KOE" = "KOE",
                                                                                          "KWK" = "KWK",
                                                                                          "KX" = "KX",
                                                                                          "LAE" = "LAE",
                                                                                          "LBX" = "LBX",
                                                                                          "LEV" = "LEV",
                                                                                          "LNX" = "LNX",
                                                                                          "LUX" = "LUX",
                                                                                          "MAH" = "MAH",
                                                                                          "MCR" = "MCR",
                                                                                          "MHX" = "MHX",
                                                                                          "MIN" = "MIN",
                                                                                          "MKA" = "MKA",
                                                                                          "MLX" = "MLX",
                                                                                          "MOS" = "MOS",
                                                                                          "MSD" = "MSD",
                                                                                          "MSX" = "MSX",
                                                                                          "MTB" = "MTB",
                                                                                          "MTE" = "MTE",
                                                                                          "MTK" = "MTK",
                                                                                          "MTS" = "MTS",
                                                                                          "MUR" = "MUR",
                                                                                          "NGX" = "NGX",
                                                                                          "NPA" = "NPA",
                                                                                          "NRA" = "NRA",
                                                                                          "NRB" = "NRB",
                                                                                          "NSA" = "NSA",
                                                                                          "NVA" = "NVA",
                                                                                          "OKT" = "OKT",
                                                                                          "ONG" = "ONG",
                                                                                          "OPO" = "OPO",
                                                                                          "OSN" = "OSN",
                                                                                          "OUA" = "OUA",
                                                                                          "Patumahoe" = "Patumahoe",
                                                                                          "PAX" = "PAX",
                                                                                          "PEX" = "PEX",
                                                                                          "PKA" = "PKA",
                                                                                          "PTU" = "PTU",
                                                                                          "RAI" = "RAI",
                                                                                          "RHU" = "RHU",
                                                                                          "RLY" = "RLY",
                                                                                          "RNP" = "RNP",
                                                                                          "ROA" = "ROA",
                                                                                          "ROT" = "ROT",
                                                                                          "RUX" = "RUX",
                                                                                          "SDN" = "SDN",
                                                                                          "SLP" = "SLP",
                                                                                          "TAH" = "TAH",
                                                                                          "TEK" = "TEK",
                                                                                          "TEP" = "TEP",
                                                                                          "TGA" = "TGA",
                                                                                          "THA" = "THA",
                                                                                          "THE" = "THE",
                                                                                          "TPE" = "TPE",
                                                                                          "TPN" = "TPN",
                                                                                          "TRQ" = "TRQ",
                                                                                          "TRX" = "TRX",
                                                                                          "TUA" = "TUA",
                                                                                          "TUT" = "TUT",
                                                                                          "WAH" = "WAH",
                                                                                          "Waharau Raws" = "Waharau Raws",
                                                                                          "Waitomo Raws" = "Waitomo Raws",
                                                                                          "WAV" = "WAV",
                                                                                          "WBA" = "WBA",
                                                                                          "WBD" = "WBD",
                                                                                          "WDH" = "WDH",
                                                                                          "WFA" = "WFA",
                                                                                          "WGF" = "WGF",
                                                                                          "WGM" = "WGM",
                                                                                          "WGO" = "WGO",
                                                                                          "WHB" = "WHB",
                                                                                          "WHG" = "WHG",
                                                                                          "WHR" = "WHR",
                                                                                          "WKA" = "WKA",
                                                                                          "WKB" = "WKB",
                                                                                          "WNA" = "WNA",
                                                                                          "WND" = "WND",
                                                                                          "WPK" = "WPK",
                                                                                          "WRA" = "WRA",
                                                                                          "WRY" = "WRY",
                                                                                          "WSA" = "WSA",
                                                                                          "WTA" = "WTA",
                                                                                          "WUA" = "WUA"), selected = "CHA"),
             actionButton("goButton", "Submit")
             
    )),
    
    
    
    mainPanel(
      
      
      navbarPage("",
    
    
    
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             
             withSpinner(plotOutput(outputId = "BoxPlots"), type = 5, color = "#4eaff2", size = 1),
             withSpinner(plotOutput(outputId = "Missing"), type = 5, color = "#4eaff2", size = 1),
             withSpinner(plotOutput(outputId = "Corr"),type = 5, color = "#4eaff2", size = 1),
             withSpinner(DT::dataTableOutput(outputId = "Table"), type = 5, color = "#4eaff2", size = 1)
    ), 
    # tabPanel("Split",
    #          sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
    #          verbatimTextOutput(outputId = "SplitSummary")
    # ),
    # tabPanel("Available methods",
    #          h3("Regression methods in caret"),
    #          shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    # ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             #helpText("The preprocessing steps and their order are important. ", 
                      #"See:", a("Documentation", href="https://www.rdocumentation.org/packages/recipes/versions/0.1.13")),
             
             tabsetPanel(
               tabPanel("Split",
                        sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
                        verbatimTextOutput(outputId = "SplitSummary")
               )
               
             ),
             
             tabsetPanel(type = "pills",
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "NullMetrics"), type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe")
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "GlmnetMetrics"), type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               # tabPanel("PLS Model",
               #          verbatimTextOutput(outputId = "PlsModelSummary0"),
               #          fluidRow(
               #            column(width = 4, 
               #                   selectizeInput(inputId = "PlsPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing", 
               #                                  choices = ppchoices,  
               #                                  multiple = TRUE, 
               #                                  selected = c("knnimpute","dummy")) 
               #            ),
               #            column(width = 1, 
               #                   actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "PlsMetrics"),
               #          hr(),
               #          plotOutput(outputId = "PlsModelPlots"),
               #          verbatimTextOutput(outputId = "PlsRecipe"),
               #          verbatimTextOutput(outputId = "PlsModelSummary2")
               # ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "RpartMetrics"),type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               # tabPanel("ols Model",
               #          verbatimTextOutput(outputId = "olsModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "olsPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c()),
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "olsGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "olsGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "olsMetrics"),
               #          hr(),
               #          plotOutput(outputId = "olsModelPlots"),
               #          plotOutput(outputId = "olsModelTree"),
               #          verbatimTextOutput(outputId = "olsRecipe"),
               # ),

               # tabPanel("BRNN Model",
               #           verbatimTextOutput(outputId = "BrnnModelSummary0"),
               #           fluidRow(
               #             column(width = 4,
               #                    selectizeInput(inputId = "BrnnPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("knnimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "BrnnGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "BrnnGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "BrnnMetrics"),
               #          hr(),
               #          plotOutput(outputId = "BrnnModelPlots"),
               #          verbatimTextOutput(outputId = "BrnnRecipe"),
               #          verbatimTextOutput(outputId = "BrnnModelSummary2")
               # ),
               # tabPanel("Parallel RF Model",
               #          verbatimTextOutput(outputId = "parRFModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "parRFPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("knnimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "parRFGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "parRFGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "parRFMetrics"),
               #          hr(),
               #          plotOutput(outputId = "parRFModelPlots"),
               #          verbatimTextOutput(outputId = "parRFRecipe"),
               #          verbatimTextOutput(outputId = "parRFModelSummary2")
               # ),
               # tabPanel("BstLm Model problem",
               #          verbatimTextOutput(outputId = "BstLmModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "BstLmPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("knnimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "BstLmGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "BstLmGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "BstLmMetrics"),
               #          hr(),
               #          plotOutput(outputId = "BstLmModelPlots"),
               #          verbatimTextOutput(outputId = "BstLmRecipe"),
               #          verbatimTextOutput(outputId = "BstLmModelSummary2")
               # ),
               # 
               # tabPanel("icr Model",
               #          verbatimTextOutput(outputId = "icrModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "icrPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("knnimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "icrGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "icrGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "icrMetrics"),
               #          hr(),
               #          plotOutput(outputId = "icrModelPlots"),
               #          verbatimTextOutput(outputId = "icrRecipe"),
               #          verbatimTextOutput(outputId = "icrModelSummary2")
               # ),
               # tabPanel("lasso Model",
               #          verbatimTextOutput(outputId = "lassoModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "lassoPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("knnimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "lassoGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "lassoGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "lassoMetrics"),
               #          hr(),
               #          plotOutput(outputId = "lassoModelPlots"),
               #          verbatimTextOutput(outputId = "lassoRecipe"),
               #          verbatimTextOutput(outputId = "lassoModelSummary2")
               # ),
               # 
               # tabPanel("pcr Model",
               #          verbatimTextOutput(outputId = "pcrModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "pcrPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("knnimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "pcrGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "pcrGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "pcrMetrics"),
               #          hr(),
               #          plotOutput(outputId = "pcrModelPlots"),
               #          verbatimTextOutput(outputId = "pcrRecipe"),
               #          verbatimTextOutput(outputId = "pcrModelSummary2")
               # ),

               tabPanel("svmLinear Model",
                        verbatimTextOutput(outputId = "svmLinearModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "svmLinearPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("knnimpute","dummy"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "svmLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmLinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "svmLinearMetrics"), type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "svmLinearModelPlots"),
                        verbatimTextOutput(outputId = "svmLinearRecipe"),
                        verbatimTextOutput(outputId = "svmLinearModelSummary2")
               ),


               tabPanel("rf Model",
                        verbatimTextOutput(outputId = "rfModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "rfPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("rfimpute","dummy"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "rfGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rfGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "rfMetrics"),type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "rfModelPlots"),
                        verbatimTextOutput(outputId = "rfRecipe"),
                        verbatimTextOutput(outputId = "rfModelSummary2")
               ),
               # tabPanel("lmStepAIC Model",
               #          verbatimTextOutput(outputId = "lmStepAICModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "lmStepAICPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("lmStepAICimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "lmStepAICGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "lmStepAICGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "lmStepAICMetrics"),
               #          hr(),
               #          plotOutput(outputId = "lmStepAICModelPlots"),
               #          verbatimTextOutput(outputId = "lmStepAICRecipe"),
               #          verbatimTextOutput(outputId = "lmStepAICModelSummary2")
               # ),

               # tabPanel("leapForward Model",
               #          verbatimTextOutput(outputId = "leapForwardModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "leapForwardPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("leapForwardimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "leapForwardGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "leapForwardGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "leapForwardMetrics"),
               #          hr(),
               #          plotOutput(outputId = "leapForwardModelPlots"),
               #          verbatimTextOutput(outputId = "leapForwardRecipe"),
               #          verbatimTextOutput(outputId = "leapForwardModelSummary2")
               # ),
               # 
               # 
               # tabPanel("bagEarthGCV Model",
               #          verbatimTextOutput(outputId = "bagEarthGCVModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "bagEarthGCVPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("bagEarthGCVimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "bagEarthGCVGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "bagEarthGCVGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "bagEarthGCVMetrics"),
               #          hr(),
               #          plotOutput(outputId = "bagEarthGCVModelPlots"),
               #          verbatimTextOutput(outputId = "bagEarthGCVRecipe"),
               #          verbatimTextOutput(outputId = "bagEarthGCVModelSummary2")
               # ),
               # tabPanel("bridge Model",
               #          verbatimTextOutput(outputId = "bridgeModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "bridgePreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("bridgeimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "bridgeGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "bridgeGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "bridgeMetrics"),
               #          hr(),
               #          plotOutput(outputId = "bridgeModelPlots"),
               #          verbatimTextOutput(outputId = "bridgeRecipe"),
               #          verbatimTextOutput(outputId = "bridgeModelSummary2")
               # ),
               # 
               # tabPanel("gaussprPoly Model",
               #          verbatimTextOutput(outputId = "gaussprPolyModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "gaussprPolyPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("gaussprPolyimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "gaussprPolyGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "gaussprPolyGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "gaussprPolyMetrics"),
               #          hr(),
               #          plotOutput(outputId = "gaussprPolyModelPlots"),
               #          verbatimTextOutput(outputId = "gaussprPolyRecipe"),
               #          verbatimTextOutput(outputId = "gaussprPolyModelSummary2")
               # ),
               # 
               # tabPanel("gaussprRadial Model",
               #          verbatimTextOutput(outputId = "gaussprRadialModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "gaussprRadialPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("gaussprRadialimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "gaussprRadialGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "gaussprRadialGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "gaussprRadialMetrics"),
               #          hr(),
               #          plotOutput(outputId = "gaussprRadialModelPlots"),
               #          verbatimTextOutput(outputId = "gaussprRadialRecipe"),
               #          verbatimTextOutput(outputId = "gaussprRadialModelSummary2")
               # ),
               # tabPanel("bam Model",
               #          verbatimTextOutput(outputId = "bamModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "bamPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("bamimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "bamGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "bamGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "bamMetrics"),
               #          hr(),
               #          plotOutput(outputId = "bamModelPlots"),
               #          verbatimTextOutput(outputId = "bamRecipe"),
               #          verbatimTextOutput(outputId = "bamModelSummary2")
               # ),

               
               tabPanel("cubist Model",
                        verbatimTextOutput(outputId = "cubistModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "cubistPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("cubistimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "cubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "cubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "cubistMetrics"), type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "cubistModelPlots"),
                        verbatimTextOutput(outputId = "cubistRecipe"),
                        verbatimTextOutput(outputId = "cubistModelSummary2")
               ),
               tabPanel("xgbLinear Model",
                        verbatimTextOutput(outputId = "xgbLinearModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "xgbLinearPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("xgbLinearimpute","dummy"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "xgbLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "xgbLinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "xgbLinearMetrics"),type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "xgbLinearModelPlots"),
                        verbatimTextOutput(outputId = "xgbLinearRecipe"),
                        verbatimTextOutput(outputId = "xgbLinearModelSummary2")
               ),
               # 
               # 
               tabPanel("gbm_h2oModel error",
                        verbatimTextOutput(outputId = "gbm_h2oModelSummary0"),
                        fluidRow(
                          column(width = 4,
                                 selectizeInput(inputId = "gbm_h2oPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing",
                                                choices = ppchoices,
                                                multiple = TRUE,
                                                selected = c("gbm_h2oimpute","dummy"))
                          ),
                          column(width = 1,
                                 actionButton(inputId = "gbm_h2oGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gbm_h2oGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "gbm_h2oMetrics"), type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "gbm_h2oModelPlots"),
                        verbatimTextOutput(outputId = "gbm_h2oRecipe"),
                        verbatimTextOutput(outputId = "gbm_h2oModelSummary2")
               ),


               # tabPanel("splsModel",
               #          verbatimTextOutput(outputId = "splsModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "splsPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("splsimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "splsGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "splsGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "splsMetrics"),
               #          hr(),
               #          plotOutput(outputId = "splsModelPlots"),
               #          verbatimTextOutput(outputId = "splsRecipe"),
               #          verbatimTextOutput(outputId = "splsModelSummary2")
               # ),
               # 
               # 
               # tabPanel("krlsPolyModel",
               #          verbatimTextOutput(outputId = "krlsPolyModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "krlsPolyPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("krlsPolyimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "krlsPolyGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "krlsPolyGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "krlsPolyMetrics"),
               #          hr(),
               #          plotOutput(outputId = "krlsPolyModelPlots"),
               #          verbatimTextOutput(outputId = "krlsPolyRecipe"),
               #          verbatimTextOutput(outputId = "krlsPolyModelSummary2")
               # ),
               # tabPanel("rvmPolyModel",
               #          verbatimTextOutput(outputId = "rvmPolyModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "rvmPolyPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("rvmPolyimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "rvmPolyGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "rvmPolyGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "rvmPolyMetrics"),
               #          hr(),
               #          plotOutput(outputId = "rvmPolyModelPlots"),
               #          verbatimTextOutput(outputId = "rvmPolyRecipe"),
               #          verbatimTextOutput(outputId = "rvmPolyModelSummary2")
               # ),
               # tabPanel("ANFISModel",
               #          verbatimTextOutput(outputId = "ANFISModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "ANFISPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("ANFISimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "ANFISGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "ANFISGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "ANFISMetrics"),
               #          hr(),
               #          plotOutput(outputId = "ANFISModelPlots"),
               #          verbatimTextOutput(outputId = "ANFISRecipe"),
               #          verbatimTextOutput(outputId = "ANFISModelSummary2")
               # ),
               # 
               # tabPanel("DENFISModel",
               #          verbatimTextOutput(outputId = "DENFISModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "DENFISPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("DENFISimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "DENFISGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "DENFISGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "DENFISMetrics"),
               #          hr(),
               #          plotOutput(outputId = "DENFISModelPlots"),
               #          verbatimTextOutput(outputId = "DENFISRecipe"),
               #          verbatimTextOutput(outputId = "DENFISModelSummary2")
               # ),
               # tabPanel("SBCModel",
               #          verbatimTextOutput(outputId = "SBCModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "SBCPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("SBCimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "SBCGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "SBCGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "SBCMetrics"),
               #          hr(),
               #          plotOutput(outputId = "SBCModelPlots"),
               #          verbatimTextOutput(outputId = "SBCRecipe"),
               #          verbatimTextOutput(outputId = "SBCModelSummary2")
               # ),
               # tabPanel("rqncModel",
               #          verbatimTextOutput(outputId = "rqncModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "rqncPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("rqncimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "rqncGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "rqncGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "rqncMetrics"),
               #          hr(),
               #          plotOutput(outputId = "rqncModelPlots"),
               #          verbatimTextOutput(outputId = "rqncRecipe"),
               #          verbatimTextOutput(outputId = "rqncModelSummary2")
               # ),
               # tabPanel("mlpMLModel",
               #          verbatimTextOutput(outputId = "mlpMLModelSummary0"),
               #          fluidRow(
               #            column(width = 4,
               #                   selectizeInput(inputId = "mlpMLPreprocess", # name this control <Method>Preprocess
               #                                  label = "Pre-processing",
               #                                  choices = ppchoices,
               #                                  multiple = TRUE,
               #                                  selected = c("mlpMLimpute","dummy"))
               #            ),
               #            column(width = 1,
               #                   actionButton(inputId = "mlpMLGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "mlpMLGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "mlpMLMetrics"),
               #          hr(),
               #          plotOutput(outputId = "mlpMLModelPlots"),
               #          verbatimTextOutput(outputId = "mlpMLRecipe"),
               #          verbatimTextOutput(outputId = "mlpMLModelSummary2")
               # ),

               
               tabPanel("M5Model",
                        verbatimTextOutput(outputId = "M5ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "M5Preprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("M5impute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "M5Go", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "M5Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "M5Metrics"),type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "M5ModelPlots"),
                        verbatimTextOutput(outputId = "M5Recipe"),
                        verbatimTextOutput(outputId = "M5ModelSummary2")
               ),
               tabPanel("M5RulesModel",
                        verbatimTextOutput(outputId = "M5RulesModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "M5RulesPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("M5Rulesimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "M5RulesGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "M5RulesGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        withSpinner(tableOutput(outputId = "M5RulesMetrics"), type = 5, color = "#4eaff2", size = 1),
                        hr(),
                        plotOutput(outputId = "M5RulesModelPlots"),
                        verbatimTextOutput(outputId = "M5RulesRecipe"),
                        verbatimTextOutput(outputId = "M5RulesModelSummary2")
               )
               
               
               # maintenance point ------------------------------------------------------------------------------
               # add further tabs (with controls) here
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             withSpinner(plotOutput(outputId = "SelectionBoxPlot"), type = 5, color = "#4eaff2", size = 1),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             withSpinner(verbatimTextOutput(outputId = "TestSummary"),type = 5, color = "#4eaff2", size = 1),
             fluidRow(
               column(offset = 2, width=4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width=2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width=2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    ),
    tabPanel("Ordinary Least Squares",
             #htmlOutput(outputId = "Title"),
             h3("Interaction Between Variables"),
             withSpinner(verbatimTextOutput(outputId = "H1"), type = 5, color = "#4eaff2", size = 1),
             verbatimTextOutput(outputId = "H2"),
             verbatimTextOutput(outputId = "H6"),
             h3("Cross Validation Grid Search"),
             verbatimTextOutput(outputId = "H3"),
             verbatimTextOutput(outputId = "H4"),
             plotOutput(outputId = "H5")
             # fluidRow(
             #   column(offset = 2, width=4,
             #          plotOutput(outputId = "TestPlot1")
             #   ),
             #   column(width=2,
             #          plotOutput(outputId = "TestResiduals")
             #   ),
             #   column(width=2,
             #          plotOutput(outputId = "TrainResiduals"),
             #   )
             # )

    )
  )
))))
