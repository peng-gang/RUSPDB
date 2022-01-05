library(shiny)
library(markdown)
library(plotly)
#library(shinybusy)

source("parameters.R")
source("functions.R")

library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(navbarPage(
  "RUSPDB",
  theme = shinytheme("superhero"),
  
  tabPanel("About"
           
  ),
  
  tabPanel("Gestational Age and Birth Weight",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tabsetPanel(
                 type = "tabs",
                 id = "meta2",
                 tabPanel(
                   "Analytes",
                   value = "analytes2",
                   tags$div(
                     title = "Select one or multiple analytes",
                     selectInput(
                       "analyte2",
                       label = h4("Analyte"),
                       choices = makeList(analytes_all),
                       selected = which(analytes_all=="C3")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     titlea = "Select birth weight range to include in the figure",
                     selectInput("bw2", label = h4("Birth Weight (g)"),
                                 choices = makeList(bw_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     selectInput("race2",
                                 label = h4("Race/Ethnicity"),
                                 choices = makeList(race_group),
                                 selected = 1:length(race_group))
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     selectInput("compare2",
                                 label = h3("Select comparing groups"),
                                 choices = c(makeList(compare_group)), 
                                 selected = 1)
                   ),
                   
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sex2",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpn2",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   
                   hr(),
                   
                   actionButton("btnSingle2", "Submit")
                 ),
                 
                 
                 tabPanel(
                   "Ratios",
                   value = "ratio2",
                   tags$div(
                     title = "Select numerator and denominator for the ratio. If multiple analytes are selected, they will be added",
                     selectInput(
                       "numerator2",
                       label = h4("Numerator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C3")
                     ),
                     selectInput(
                       "denominator2",
                       label = h4("Denominator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C2")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Select birth weight range to include in the figure",
                     checkboxGroupInput(
                       "bwRatio2",
                       label = h4("Birth Weight (g)"),
                       choices = makeList(bw_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     checkboxGroupInput(
                       "gaRatio2",
                       label = h4("Gestational Age (week)"),
                       choices = makeList(ga_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     checkboxGroupInput(
                       "raceRatio2",
                       label = h4("Race/Ethnicity"),
                       choices = makeList(race_group),
                       selected = 1:length(race_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sexRatio2",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpnRatio2",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     radioButtons("compareRatio2", label = h3("Select comparing groups"),
                                  choices = c(makeList(compare_group)), 
                                  selected = 1)
                   ),
                   
                   hr(),
                   
                   actionButton("btnRatio2", "Submit")
                 )
               )
             ),
             
             mainPanel(
               plotlyOutput("boxplot2"),
             )
           ) 
  ),
  
  tabPanel("Sex",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tabsetPanel(
                 type = "tabs",
                 id = "meta3",
                 tabPanel(
                   "Analytes",
                   value = "analytes3",
                   tags$div(
                     title = "Select one or multiple analytes",
                     selectInput(
                       "analyte3",
                       label = h4("Analyte"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected = which(analytes_all=="C3")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     titlea = "Select birth weight range to include in the figure",
                     selectInput("bw3", label = h4("Birth Weight (g)"),
                                 choices = makeList(bw_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     selectInput("ga3",
                                 label = h4("Gestational Age (week)"),
                                 choices = makeList(ga_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     selectInput("race3",
                                 label = h4("Race/Ethnicity"),
                                 choices = makeList(race_group),
                                 selected = 1:length(race_group),
                                 multiple = T)
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     selectInput("compare3",
                                 label = h3("Select comparing groups"),
                                 choices = c(makeList(compare_group)), 
                                 selected = 1)
                   ),
                   
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpn3",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   
                   hr(),
                   
                   actionButton("btnSingle3", "Submit")
                 ),
                 
                 
                 tabPanel(
                   "Ratios",
                   value = "ratio3",
                   tags$div(
                     title = "Select numerator and denominator for the ratio. If multiple analytes are selected, they will be added",
                     selectInput(
                       "numerator3",
                       label = h4("Numerator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C3")
                     ),
                     selectInput(
                       "denominator3",
                       label = h4("Denominator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C2")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Select birth weight range to include in the figure",
                     checkboxGroupInput(
                       "bwRatio3",
                       label = h4("Birth Weight (g)"),
                       choices = makeList(bw_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     checkboxGroupInput(
                       "gaRatio3",
                       label = h4("Gestational Age (week)"),
                       choices = makeList(ga_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     checkboxGroupInput(
                       "raceRatio3",
                       label = h4("Race/Ethnicity"),
                       choices = makeList(race_group),
                       selected = 1:length(race_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sexRatio3",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpnRatio3",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     radioButtons("compareRatio3", label = h3("Select comparing groups"),
                                  choices = c(makeList(compare_group)), 
                                  selected = 1)
                   ),
                   
                   hr(),
                   
                   actionButton("btnRatio3", "Submit")
                 )
               )
             ),
             
             mainPanel(
               plotlyOutput("boxplot3"),
             )
           ) 
  ),
  
  tabPanel("Age at Blood Collection",

           sidebarLayout(
             sidebarPanel(
               width = 3,
               tabsetPanel(
                 type = "tabs",
                 id = "meta",
                 tabPanel(
                   "Analytes",
                   value = "analytes",
                   tags$div(
                     title = "Select one or multiple analytes",
                     selectInput(
                       "analyte",
                       label = h4("Analyte"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected = which(analytes_all=="C3")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     titlea = "Select birth weight range to include in the figure",
                     selectInput("bw", label = h4("Birth Weight (g)"),
                                 choices = makeList(bw_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     selectInput("ga",
                                 label = h4("Gestational Age (week)"),
                                 choices = makeList(ga_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     selectInput("race",
                                 label = h4("Race/Ethnicity"),
                                 choices = makeList(race_group),
                                 selected = 1:length(race_group),
                                 multiple = T)
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     selectInput("compare",
                                 label = h3("Select comparing groups"),
                                 choices = c(makeList(compare_group)), 
                                 selected = 1)
                   ),
                   
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sex",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpn",
                       label = h4("TPN Status"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   
                   hr(),
                   
                   actionButton("btnSingle", "Submit")
                 ),
                 
                 
                 tabPanel(
                   "Ratios",
                   value = "ratio",
                   tags$div(
                     title = "Select numerator and denominator for the ratio. If multiple analytes are selected, they will be added",
                     selectInput(
                       "numerator",
                       label = h4("Numerator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C3")
                     ),
                     selectInput(
                       "denominator",
                       label = h4("Denominator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C2")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Select birth weight range to include in the figure",
                     checkboxGroupInput(
                       "bwRatio",
                       label = h4("Birth Weight (g)"),
                       choices = makeList(bw_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     checkboxGroupInput(
                       "gaRatio",
                       label = h4("Gestational Age (week)"),
                       choices = makeList(ga_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     checkboxGroupInput(
                       "raceRatio",
                       label = h4("Race/Ethnicity"),
                       choices = makeList(race_group),
                       selected = 1:length(race_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sexRatio",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpnRatio",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     radioButtons("compareRatio", label = h3("Select comparing groups"),
                                  choices = c(makeList(compare_group)), 
                                  selected = 1)
                   ),
                   
                   hr(),
                   
                   actionButton("btnRatio", "Submit")
                 )
               )
             ),
             
             mainPanel(
               plotlyOutput("boxplot"),
               hr(),
               plotlyOutput("trendplot")
             )
           )
           ),
  
  
  tabPanel("Ethnicity",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tabsetPanel(
                 type = "tabs",
                 id = "meta4",
                 tabPanel(
                   "Analytes",
                   value = "analytes4",
                   tags$div(
                     title = "Select one or multiple analytes",
                     selectInput(
                       "analyte4",
                       label = h4("Analyte"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected = which(analytes_all=="C3")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     titlea = "Select birth weight range to include in the figure",
                     selectInput("bw4", label = h4("Birth Weight (g)"),
                                 choices = makeList(bw_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     selectInput("ga4",
                                 label = h4("Gestational Age (week)"),
                                 choices = makeList(ga_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     selectInput("race4",
                                 label = h4("Race/Ethnicity"),
                                 choices = makeList(race_group),
                                 selected = 1:length(race_group),
                                 multiple = T)
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     selectInput("compare4",
                                 label = h3("Select comparing groups"),
                                 choices = c(makeList(compare_group)), 
                                 selected = 1)
                   ),
                   
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sex4",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpn4",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   
                   hr(),
                   
                   actionButton("btnSingle4", "Submit")
                 ),
                 
                 
                 tabPanel(
                   "Ratios",
                   value = "ratio4",
                   tags$div(
                     title = "Select numerator and denominator for the ratio. If multiple analytes are selected, they will be added",
                     selectInput(
                       "numerator4",
                       label = h4("Numerator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C3")
                     ),
                     selectInput(
                       "denominator4",
                       label = h4("Denominator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C2")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Select birth weight range to include in the figure",
                     checkboxGroupInput(
                       "bwRatio4",
                       label = h4("Birth Weight (g)"),
                       choices = makeList(bw_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     checkboxGroupInput(
                       "gaRatio4",
                       label = h4("Gestational Age (week)"),
                       choices = makeList(ga_group),
                       selected = 2:4
                     )
                   ),
                   
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sexRatio4",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpnRatio4",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     radioButtons("compareRatio4", label = h3("Select comparing groups"),
                                  choices = c(makeList(compare_group)), 
                                  selected = 1)
                   ),
                   
                   hr(),
                   
                   actionButton("btnRatio4", "Submit")
                 )
               )
             ),
             
             mainPanel(
               plotlyOutput("boxplot4"),
             )
           ) 
  ),
  
  tabPanel("Total Parenteral Nutrition"
           ,
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tabsetPanel(
                 type = "tabs",
                 id = "meta5",
                 tabPanel(
                   "Analytes",
                   value = "analytes5",
                   tags$div(
                     title = "Select one or multiple analytes",
                     selectInput(
                       "analyte5",
                       label = h4("Analyte"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected = which(analytes_all=="C3")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     titlea = "Select birth weight range to include in the figure",
                     selectInput("bw5", label = h4("Birth Weight (g)"),
                                 choices = makeList(bw_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     selectInput("ga5",
                                 label = h4("Gestational Age (week)"),
                                 choices = makeList(ga_group),
                                 selected = 2:4, multiple = T)
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     selectInput("race5",
                                 label = h4("Race/Ethnicity"),
                                 choices = makeList(race_group),
                                 selected = 1:length(race_group),
                                 multiple = T)
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     selectInput("compare5",
                                 label = h3("Select comparing groups"),
                                 choices = c(makeList(compare_group)), 
                                 selected = 1)
                   ),
                   
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sex5",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpn5",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   
                   hr(),
                   
                   actionButton("btnSingle5", "Submit")
                 ),
                 
                 
                 tabPanel(
                   "Ratios",
                   value = "ratio5",
                   tags$div(
                     title = "Select numerator and denominator for the ratio. If multiple analytes are selected, they will be added",
                     selectInput(
                       "numerator5",
                       label = h4("Numerator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C3")
                     ),
                     selectInput(
                       "denominator5",
                       label = h4("Denominator"),
                       choices = makeList(analytes_all),
                       multiple = TRUE,
                       selected =  which(analytes_all=="C2")
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Select birth weight range to include in the figure",
                     checkboxGroupInput(
                       "bwRatio5",
                       label = h4("Birth Weight (g)"),
                       choices = makeList(bw_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select gestational age range to include in the figure",
                     checkboxGroupInput(
                       "gaRatio5",
                       label = h4("Gestational Age (week)"),
                       choices = makeList(ga_group),
                       selected = 2:4
                     )
                   ),
                   
                   tags$div(
                     title = "Select race/ethnicity group(s) to include in the figure",
                     checkboxGroupInput(
                       "raceRatio5",
                       label = h4("Race/Ethnicity"),
                       choices = makeList(race_group),
                       selected = 1:length(race_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select sex to include in the figure",
                     checkboxGroupInput(
                       "sexRatio5",
                       label = h4("Sex"),
                       choices = makeList(sex_group),
                       selected = 1:length(sex_group)
                     )
                   ),
                   
                   tags$div(
                     title = "Select TPN status to include in the figure",
                     checkboxGroupInput(
                       "tpnRatio5",
                       label = h4("TPN"),
                       choices = makeList(tpn_group),
                       selected = 1
                     )
                   ),
                   
                   hr(),
                   
                   tags$div(
                     title = "Compare difference between groups within the selected category",
                     radioButtons("compareRatio5", label = h3("Select comparing groups"),
                                  choices = c(makeList(compare_group)), 
                                  selected = 1)
                   ),
                   
                   hr(),
                   
                   actionButton("btnRatio5", "Submit")
                 )
               )
             ),
             
             mainPanel(
               plotlyOutput("boxplot5"),
             )
           ) 
  ),
  
  tabPanel(
      "Multiple Comparison",
      sidebarLayout(
          sidebarPanel(
              width = 3,
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
                      value = "ratioMC"
                  )
              )
          ),
          mainPanel(
              plotlyOutput("figureMC"),
              hr(),
              htmlOutput("infoMC")
          )
      )
  )
))
