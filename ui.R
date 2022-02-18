library(shiny)
library(markdown)
library(plotly)
#library(shinybusy)
library(shinythemes)

sidebarWidth <- 4

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "RUSPDB",
    theme = shinytheme("superhero"),
    
    ####### 
    ## about
    tabPanel(
      "About"
    ),
    
    
    #######
    ## gestational age and birth weight
    
    tabPanel(
      "GA and BW"
    ),
    
    ######
    ## ethnicity
    
    tabPanel(
      "Ethnicity"
    ),
    
    
    ######
    ## sex
    
    tabPanel(
      "Sex",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "SEX",
            tabPanel(
              "Analyte(s)",
              value = "analytesSex",
              tags$div(
                title = "Select one or multiple analytes",
                selectInput(
                  "analyteSex",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
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
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethSexSel",
                  label = NULL,
                  choices = list("Major ethnic groups" = 1, "Detailed ethnic groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthSex")
              ),
              
              tags$div(
                title = "Select Aabc to include in the figure",
                checkboxGroupInput(
                  "sexAabc",
                  label = h4("Aabc"),
                  choices = makeList(aabc_group),
                  selected = 1:length(aabc_group)
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
                  label = NULL,
                  choices = list("Major ethnic groups" = 1, "Detailed ethnic groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthSexRatio")
              ),
              
              
              tags$div(
                title = "Select sex to include in the figure",
                checkboxGroupInput(
                  "sexSexRatio",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  selected = 1:length(sex_group)
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
          plotOutput("boxplotSex")
        )
      )
    ),
    
    ####### 
    ## aabc
    tabPanel(
      "Age at blood collection",
      sidebarLayout(
        sidebarPanel(
          width = sidebarWidth,
          tabsetPanel(
            type = "tabs",
            id = "AABC",
            tabPanel(
              "Analyte(s)",
              value = "analytesAabc",
              tags$div(
                title = "Select one or multiple analytes",
                selectInput(
                  "analyteAabc",
                  label = h4("Analyte"),
                  choices = makeList(analytes_all),
                  multiple = TRUE,
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
              
              hr(),
              
              tags$div(
                title = "Select a ethnicity group(s) to include in the figure",
                radioButtons(
                  "ethAabcSel",
                  label = NULL,
                  choices = list("Major ethnic groups" = 1, "Detailed ethnic groups" = 2),
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
                  label = NULL,
                  choices = list("Major ethnic groups" = 1, "Detailed ethnic groups" = 2),
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
              
              hr(),
              
              actionButton("aabcRatioSubmit", "Submit")
            )
            
          )
        ),
        
        mainPanel(
          plotOutput("boxplotAabc"),
          hr(),
          plotOutput("trendplotAabc")
        )
      )
    ),
    
    
    tabPanel(
      "TPN"
    ),
    
    
    ####### 
    # multiple comparison
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
                  selected = which(analytes_all=="C3")
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
                  selected = 1
                )
              ),
              
              
              
              tags$div(
                title = "Select gestational age range to compare",
                selectInput(
                  "gaMC",
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
                  "ethMCSel",
                  label = NULL,
                  choices = list("Major ethnic groups" = 1, "Detailed ethnic groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthMC")
              ),
              
              tags$div(
                title = "Select a range of age at boold collection to compare",
                selectInput(
                  "aabcMC",
                  label = h4("Age at Blood Collection (hour)"),
                  choices = makeList(aabc_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              
              tags$div(
                title = "Select sex to compare",
                selectInput(
                  "sexMC",
                  label = h4("Sex"),
                  choices = makeList(sex_group),
                  multiple = TRUE,
                  selected = 1
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
                  choices = list("Major ethnic groups" = 1, "Detailed ethnic groups" = 2),
                  selected = 1
                ),
                
                uiOutput("uiEthMCRatio")
              ),
              
              tags$div(
                title = "Select a range of age at boold collection to compare",
                selectInput(
                  "aabcMCRatio",
                  label = h4("Age at Blood Collection (hour)"),
                  choices = makeList(aabc_group),
                  multiple = TRUE,
                  selected = 1
                )
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
                title = "Select TPN status to compare",
                selectInput(
                  "tpnMCRatio",
                  label = h4("Status of Total Parenteral Nutrition"),
                  choices = makeList(tpn_group),
                  multiple = TRUE,
                  selected = 1
                )
              ),
              
              hr(),
              
              actionButton("mcRatioSubmit", "Submit")
            )
          )
        ),
        mainPanel(
          plotOutput("figureMC"),
          hr(),
          htmlOutput("infoMC")
        )
      )
    )
    
    
    ######
  )
)
