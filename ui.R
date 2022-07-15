library(shiny)
library(markdown)
#library(plotly)
#library(shinybusy)
library(shinythemes)
library(shinycssloaders)
library(DT)

sidebarWidth <- 4

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "dbRUSP",
    theme = shinytheme("superhero"),
    
    #tags$head(tags$style("div.dataTables_scrollHead span {color: white;}")),
    

    ############ about ###########
    tabPanel(
      "About",
      includeMarkdown("content/about.md")
    ),
    
    
    ############ gestational age and birth weight ###############
    
    tabPanel(
      "GA and BW",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "GABW",
            tabPanel(
              "Analyte",
              value = "analytesGABW",
              tags$div(
                title = "Select one analyte",
                selectInput(
                  "analyteGABW",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = FALSE,
                  selected = which(analytes_all=="C3")
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethGABWSel",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthGABW")
              ),
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexGABW",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcGABW",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnGABW",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),

              
              
              # hr(),
              # 
              # tags$div(
              #   title = "Select to show smooth line (it will take about 2 mins)",
              #   checkboxInput("trendGABWSel", label = "Show smooth line", value = FALSE)
              # ),
              
              hr(),
              
              actionButton("GABWSubmit", "Submit")
            ),
            
            
            tabPanel(
              "Ratios",
              value = "ratioGABW",
              
              tags$div(
                title = "Select numerator and denominator for the ratio. If multiple analytes are selected in numerator or denominator, they will be added",
                selectInput(
                  "numeratorGABW",
                  label = h4("Numerator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C3")
                ),
                
                selectInput(
                  "denominatorGABW",
                  label = h4("Denominator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C2")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethGABWSelRatio",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthGABWRatio")
              ),
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexGABWRatio",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcGABWRatio",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnGABWRatio",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
              
              # hr(),
              # 
              # tags$div(
              #   title = "Select to show smooth line (it will take about 2 mins)",
              #   checkboxInput("trendGABWSelRatio", label = "Show smooth line", value = FALSE)
              # ),
              
              hr(),
              
              
              actionButton("GABWRatioSubmit", "Submit")
            )
            
          )
        ),
        
        mainPanel(
          shinycssloaders::withSpinner(plotOutput("heatGABW")),
          hr(),
          shinycssloaders::withSpinner(plotOutput("trendGABW"))
        )
      )
    ),
    
    
    ################# ethnicity  #######################
    
    tabPanel(
      "Ethnicity",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "ETHNICITY",
            tabPanel(
              "Analyte",
              value = "analytesEth",
              tags$div(
                title = "Select one analyte",
                selectInput(
                  "analyteEth",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = FALSE,
                  selected = which(analytes_all=="C3")
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethEthSel",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthEth")
              ),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwEth",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaEth",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexEth",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcEth",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnEth",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
            
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareEth", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='Ethnicity'])), 
                             selected = 1)
              ),
              
              hr(),
              
              actionButton("ethSubmit", "Submit")
            ),
            
            
            tabPanel(
              "Ratios",
              value = "ratioEth",
              
              tags$div(
                title = "Select numerator and denominator for the ratio. If multiple analytes are selected in numerator or denominator, they will be added",
                selectInput(
                  "numeratorEth",
                  label = h4("Numerator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C3")
                ),
                
                selectInput(
                  "denominatorEth",
                  label = h4("Denominator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C2")
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethEthSelRatio",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthEthRatio")
              ),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwEthRatio",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaEthRatio",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexEthRatio",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),

              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcEthRatio",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnEthRatio",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareEthRatio", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='Ethnicity'])), 
                             selected = 1)
              ),
              
              hr(),
              
              actionButton("ethRatioSubmit", "Submit")
            )
            
          )
        ),
        
        mainPanel(
          plotOutput("boxplotEth"),
          hr(),
          DTOutput("tableEth")
        )
      )
    ),

    ####################### sex #############################
    
    tabPanel(
      "Sex",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "SEX",
            tabPanel(
              "Analyte",
              value = "analytesSex",
              tags$div(
                title = "Select one analyte",
                selectInput(
                  "analyteSex",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = FALSE,
                  selected = which(analytes_all=="C3")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwSex",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaSex",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethSexSel",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthSex")
              ),
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcSex",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnSex",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareSex", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='Sex'])), 
                             selected = 1)
              ),
              
              hr(),
              
              actionButton("sexSubmit", "Submit")
            ),
            
            
            tabPanel(
              "Ratios",
              value = "ratioSex",
              
              tags$div(
                title = "Select numerator and denominator for the ratio. If multiple analytes are selected in numerator or denominator, they will be added",
                selectInput(
                  "numeratorSex",
                  label = h4("Numerator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C3")
                ),
                
                selectInput(
                  "denominatorSex",
                  label = h4("Denominator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C2")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwSexRatio",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaSexRatio",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethSexSelRatio",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthSexRatio")
              ),
              
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcSexRatio",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnSexRatio",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
              
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareSexRatio", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='Sex'])), 
                             selected = 1)
              ),
              
              hr(),
              
              actionButton("sexRatioSubmit", "Submit")
            )
            
          )
        ),
        
        mainPanel(
          plotOutput("boxplotSex"),
          hr(),
          DTOutput("tableSex")
        )
      )
    ),
    

    ############ AaBC ############
    tabPanel(
      "Age at blood collection",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "AABC",
            tabPanel(
              "Analyte",
              value = "analytesAabc",
              tags$div(
                title = "Select one analyte",
                selectInput(
                  "analyteAabc",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = FALSE,
                  selected = which(analytes_all=="C3")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwAabc",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaAabc",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
            
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethAabcSel",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthAabc")
              ),
              
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexAabc",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnAabc",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareAabc", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='Aabc'])), 
                             selected = 1)
              ),
              
              # hr(),
              # 
              # tags$div(
              #   title = "Select to show smooth line (it will take about 2 mins)",
              #   checkboxInput("trendAabcSel", label = "Show smooth line", value = FALSE)
              # ),
              
              hr(),
              
              actionButton("aabcSubmit", "Submit")
            ),
            
            
            tabPanel(
              "Ratios",
              value = "ratioAabc",
              
              tags$div(
                title = "Select numerator and denominator for the ratio. If multiple analytes are selected in numerator or denominator, they will be added",
                selectInput(
                  "numeratorAabc",
                  label = h4("Numerator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C3")
                ),
                
                selectInput(
                  "denominatorAabc",
                  label = h4("Denominator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C2")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwAabcRatio",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaAabcRatio",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethAabcSelRatio",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthAabcRatio")
              ),
              
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexAabcRatio",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              tags$div(
                title = "Select TPN status to include in the figure",
                checkboxGroupInput(
                  "tpnAabcRatio",
                  label = h4("TPN"),
                  choices = makeList(tpn_group),
                  selected = 1
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareAabcRatio", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='Aabc'])), 
                             selected = 1)
              ),
              
              # hr(),
              # 
              # tags$div(
              #   title = "Select to show smooth line (it will take about 2 mins)",
              #   checkboxInput("trendAabcRatioSel", label = "Show smooth line", value = FALSE)
              # ),
              
              hr(),
              
              actionButton("aabcRatioSubmit", "Submit")
            )
            
          )
        ),
        
        mainPanel(
          shinycssloaders::withSpinner(plotOutput("boxplotAabc")),
          hr(),
          shinycssloaders::withSpinner(plotOutput("trendplotAabc"))
        )
      )
    ),
    

    ################# TPN ##############
    tabPanel(
      "TPN",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "TPN",
            tabPanel(
              "Analyte",
              value = "analytesTPN",
              tags$div(
                title = "Select one analyte",
                selectInput(
                  "analyteTPN",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = FALSE,
                  selected = which(analytes_all=="C3")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwTPN",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaTPN",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethTPNSel",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthTPN")
              ),
              
              tags$div(
                title = "Select Sex status to include in the figure",
                checkboxGroupInput(
                  "sexTPN",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcTPN",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareTPN", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='TPN'])), 
                             selected = 1)
              ),
              
              hr(),
              
              actionButton("tpnSubmit", "Submit")
            ),
            
            
            tabPanel(
              "Ratios",
              value = "ratioTPN",
              
              tags$div(
                title = "Select numerator and denominator for the ratio. If multiple analytes are selected in numerator or denominator, they will be added",
                selectInput(
                  "numeratorTPN",
                  label = h4("Numerator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C3")
                ),
                
                selectInput(
                  "denominatorTPN",
                  label = h4("Denominator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C2")
                )
              ),
              
              hr(),
              
              
              tags$div(
                title = "Select birth weight range to include in the figure",
                checkboxGroupInput(
                  "bwTPNRatio",
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select gestational age range to include in the figure",
                checkboxGroupInput(
                  "gaTPNRatio",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  selected = 2:4
                )
              ),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethTPNSelRatio",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthTPNRatio")
              ),
              
              tags$div(
                title = "Select Sex to include in the figure",
                checkboxGroupInput(
                  "sexTPNRatio",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
                )
              ),
              
              
              tags$div(
                title = "Select AaBC to include in the figure",
                checkboxGroupInput(
                  "aabcTPNRatio",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  selected = 2
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Compare difference between groups within the selected category",
                radioButtons("compareTPNRatio", label = h3("Select comparing groups"),
                             choices = c(makeList(compare_group[compare_group!='TPN'])), 
                             selected = 1)
              ),
              
              hr(),
              
              actionButton("tpnRatioSubmit", "Submit")
            )
            
          )
        ),
        
        mainPanel(
          plotOutput("boxplotTPN"),
          hr(),
          DTOutput("tableTPN")
        )
      )
    ),
    
      
    
    ######## Multiple comparison #######
    tabPanel(
      "Multiple Comparison",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "multiCompare",
            tabPanel(
              "Analyte(s)",
              value = "analytesMC",
              tags$div(
                title = "Select one or multiple analytes",
                selectInput(
                  "analyteMC",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all%in% c("Valine", "Citrulline", "C3"))
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select birth weight range to compare",
                selectInput(
                  "bwMC", 
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  multiple = TRUE,
                  selected = 4
                )
              ),
              
              
              
              tags$div(
                title = "Select gestational age range to compare",
                selectInput(
                  "gaMC",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  multiple = TRUE,
                  selected = 3
                )
              ),
              
              
              hr(),
              
              
              tags$div(
                title = "Select a ethnicity group to compare",
                radioButtons(
                  "ethMCSel",
                  label = h4("Ethnicity"),
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthMC")
              ),
              
              tags$div(
                title = "Select sex to compare",
                selectInput(
                  "sexMC",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  multiple = TRUE,
                  selected = 2
                )
              ),
              
              tags$div(
                title = "Select a range of age at boold collection to compare",
                selectInput(
                  "aabcMC",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  multiple = TRUE,
                  selected = 4
                )
              ),
              
              tags$div(
                title = "Select TPN status to compare",
                selectInput(
                  "tpnMC",
                  label = h4("Status of Total Parenteral Nutrition"),
                  choices = makeList(tpn_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              hr(),
              
              actionButton("mcSubmit", "Submit")
            ),
            tabPanel(
              "Ratios",
              value = "ratioMC",
              
              tags$div(
                title = "Select numerator and denominator for the ratio. If multiple analytes are selected in numerator or denominator, they will be added",
                selectInput(
                  "numeratorMC",
                  label = h4("Numerator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C3")
                ),
                selectInput(
                  "denominatorMC",
                  label = h4("Denominator"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
                  selected = which(analytes_all=="C2")
                )
              ),
              
              hr(),
              
              tags$div(
                title = "Select birth weight range to compare",
                selectInput(
                  "bwMCRatio", 
                  label = h4("Birth Weight (g)"),
                  choices = makeList(bw_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              
              
              tags$div(
                title = "Select gestational age range to compare",
                selectInput(
                  "gaMCRatio",
                  label = h4("Gestational Age (week)"),
                  choices = makeList(ga_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              
              hr(),
              
              
              tags$div(
                title = "Select a ethnicity group to compare",
                radioButtons(
                  "ethMCSelRatio",
                  label = NULL,
                  choices = list("Major ethnicity groups" = 1, "Detailed ethnicity groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthMCRatio")
              ),
              
              tags$div(
                title = "Select sex to compare",
                selectInput(
                  "sexMCRatio",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              tags$div(
                title = "Select a range of age at boold collection to compare",
                selectInput(
                  "aabcMCRatio",
                  label = h4("AaBC (hour)"),
                  choices = makeList(aabc_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              tags$div(
                title = "Select TPN status to compare",
                selectInput(
                  "tpnMCRatio",
                  label = h4("Status of Total Parenteral Nutrition"),
                  choices = makeList(tpn_group),
                  multiple = TRUE,
                  selected = 2
                )
              ),
              
              hr(),
              
              actionButton("mcRatioSubmit", "Submit")
            )
          )
        ),
        mainPanel(
          div(style='height:300px;overflow-y: scroll;',
              uiOutput("uiMC")
          ),
        
          #plotOutput("figureMC"),
          hr(),
          DTOutput("tableMC"),
          hr(),
          htmlOutput("infoMC")
        )
      )
    ),
    
    
    ######## Metabolite to condition #######
    tabPanel(
      "Metabolite to condition",
      DT::dataTableOutput("tab_1"),
      tableOutput("tab_2")
      )
  )
)
