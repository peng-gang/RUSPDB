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
  theme = shinytheme("cerulean"),
  
  tabPanel("About"
           
  ),
  
  tabPanel("Gestational Age and Birth Weight"
           
  ),
  
  tabPanel("Sex"
           
  ),
  
  tabPanel("Age at Blood Collection",
           
           titlePanel("RUSPtools",
                      title = tags$strong("Visualization of the relation between Metabolite Levels and Age at Blood Collection")),
           
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
                     title = "Select birth weight range to include in the figure",
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
                       label = h4("TPN"),
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
                 ),
                 
                 tabPanel(
                   "About",
                   # value = "about",
                   includeMarkdown("content/About.md")
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
  
  
  tabPanel("Ethnicity"
           
  ),
  
  tabPanel("Total Parenteral Nutrition"
           
  ),
  
  tabPanel("Multiple Comparison"
           
  )
))
