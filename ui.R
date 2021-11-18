library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
    "RUSPDB",
    theme = shinytheme("spacelab"),
    
    tabPanel("About"
             
    ),
    
    tabPanel("Gestational Age and Birth Weight"
        
    ),
    
    tabPanel("Sex"
        
    ),
    
    tabPanel("Age at Blood Collection"
             
    ),
    
    
    tabPanel("Ethnicity"
             
    ),
    
    tabPanel("Total Parenteral Nutrition"
             
    ),
    
    tabPanel("Multiple Comparison"
             
    )
))
