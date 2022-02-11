library(shiny)
library(plotly)
#library(shinybusy)

#load('data/500KCleanJan0422.RData')

#source("parameters.R")
#source("functions.R")
#source("functionsMC.R")


# meta_data$race <- flag_race
# meta_data$aac <- aac
# meta_data$birthweight <- flag_bw
# meta_data$ga <- flag_ga
# meta_data$sex <- flag_sex
# meta_data$tpn <- flag_tpn
# meta_data$include <- idx_include

shinyServer(function(input, output, session) {
  
  
  
  ############################
  ## aabc
  
  # ethnicity selection: major groups or detailed groups (analytes panel)
  output$uiEthAabc <- renderUI({
    switch(input$ethAabcSel,
           "1" = selectInput(
             "ethAabc",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethAabc",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:length(ethnicity_group_details)
           )
    )
  })
  
  
  # ethnicity selection: major groups or detailed groups (ratio panel)
  output$uiEthAabcRatio <- renderUI({
    switch(input$ethAabcSelRatio,
           "1" = selectInput(
             "ethAabcRatio",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethAabcRatio",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:length(ethnicity_group_details)
           )
    )
  })
  
  
  # boxplot comparing difference between aabc groups
  plotBoxplotAabc <- eventReactive(c(input$aabcSubmit, input$aabcRatioSubmit, input$AABC),{
    if(input$AABC == "analytesAabc"){
      return(plotBoxplotAabcAnalytes(
        input$analyteAabc, input$bwAabc, input$gaAabc, input$ethAabcSel, input$ethAabc, 
        input$sexAabc, input$tpnAabc, input$compareAabc
      ))
    } else {
      return(
        plotBoxplotAabcRatio(
          input$numeratorAabc, input$denominatorAabc, input$bwAabcRatio, input$gaAabcRatio, input$ethAabcSelRatio, input$ethAabcRatio, 
          input$sexAabcRatio, input$tpnAabcRatio, input$compareAabcRatio
        )
      )
    }
  })
  
  # metabolic changes over aabc
  plotTrendplotAabc <- eventReactive(c(input$aabcSubmit, input$aabcRatioSubmit, input$AABC),{
    if(input$AABC == "analytesAabc"){
      return(
        plotTrendplotAabcAnalytes(
          input$analyteAabc, input$bwAabc, input$gaAabc, input$ethAabcSel, input$ethAabc, 
          input$sexAabc, input$tpnAabc, input$compareAabc
        )
      )
    } else {
      return(
        plotTrendplotAabcRatio(
          input$numeratorAabc, input$denominatorAabc, input$bwAabcRatio, input$gaAabcRatio, input$ethAabcSelRatio, input$ethAabcRatio, 
          input$sexAabcRatio, input$tpnAabcRatio, input$compareAabcRatio
        )
      )
    }
  })
  
  output$boxplotAabc <- renderPlot({
    plotBoxplotAabc()
  })
  
  
  output$trendplotAabc <- renderPlot({
    plotTrendplotAabc()
  })
  
  
  
  ############################
  ##  Multiple Comparison
  
  #ethnicity selection: major groups or detailed groups (analytes panel)
  output$uiEthMC <- renderUI({
    switch(input$ethMCSel,
           "1" = selectInput(
             "ethMC",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1
           ),
           
           "2" = selectInput(
             "ethMC",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1
           )
        )
  })
  
  
  #ethnicity selection: major groups or detailed groups (ratio panel)
  output$uiEthMCRatio <- renderUI({
    switch(input$ethMCSelRatio,
           "1" = selectInput(
             "ethMCRatio",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1
           ),
           
           "2" = selectInput(
             "ethMCRatio",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1
           )
    )
  })
  
  
  # boxplot for multiple comparison
  plotMC <- eventReactive(c(input$mcSubmit, input$mcRatioSubmit, input$multiCompare),{
    if(input$multiCompare == "analytesMC"){
      return(plotMCAnalytes(
        input$analyteMC, input$bwMC, input$gaMC, input$ethMCSel, input$ethMC, 
        input$aabcMC, input$sexMC, input$tpnMC
      ))
    } else {
      return(plotMCRatio(
        input$numeratorMC, input$denominatorMC, input$bwMCRatio, input$gaMCRatio, 
        input$ethMCSelRatio, input$ethMCRatio, 
        input$aabcMCRatio, input$sexMCRatio, input$tpnMCRatio
      ))
    }
  })
  
  # comparison information 
  getMCInfo <- eventReactive(c(input$mcSubmit, input$mcRatioSubmit, input$multiCompare),{
    if(input$multiCompare == "analytesMC"){
      return(getMCInfoAnalytes(
        input$analyteMC, input$bwMC, input$gaMC, input$ethMCSel, input$ethMC, 
        input$aabcMC, input$sexMC, input$tpnMC
      ))
    } else {
      return(getMCInfoRatio(
        input$numeratorMC, input$denominatorMC, input$bwMCRatio, input$gaMCRatio, 
        input$ethMCSelRatio, input$ethMCRatio, 
        input$aabcMCRatio, input$sexMCRatio, input$tpnMCRatio
      ))
    }
  })
  
  output$figureMC <- renderPlot({
    plotMC()
  })
  
  
  output$infoMC <- renderUI({
    getMCInfo()
  })
})
