library(shiny)
#library(plotly)
#library(shinybusy)

load('data/500KCleanJan0422.RData')

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
  
  ########### GABW ###########
  output$uiEthGABW <- renderUI({
    switch(input$ethGABWSel,
           "1" = selectInput(
             "ethGABW",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethGABW",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  
  output$uiEthGABWRatio <- renderUI({
    switch(input$ethGABWSelRatio,
           "1" = selectInput(
             "ethGABWRatio",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethGABWRatio",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  
  plotHeatGABW <- eventReactive(c(input$GABWSubmit, input$GABWRatioSubmit, input$GABW), {
    if(input$GABW == "analytesGABW"){
      return(
        plotHeatGABWAnalytes(
          input$analyteGABW, input$ethGABWSel, input$ethGABW, 
          input$sexGABW, input$aabcGABW, input$tpnGABW
        )
      )
    } else {
      return(
        plotHeatGABWRatio(
          input$numeratorGABW, input$denominatorGABW, input$ethGABWSelRatio, input$ethGABWRatio,
          input$sexGABWRatio, input$aabcGABWRatio, input$tpnGABWRatio
        )
      )
    }
  })
  
  plotTrendGABW <- eventReactive(c(input$GABWSubmit, input$GABWRatioSubmit, input$GABW), {
    if(input$GABW == "analytesGABW"){
      if(!input$trendGABWSel){
        gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
          geom_text(aes(x=x, y=y, label=label)) + 
          scale_x_continuous(limits = c(0,1)) + 
          scale_y_continuous(limits = c(0,1)) + 
          theme_void() + theme(text = element_text(size = 24))
        return(gp)
      }
      return(
        plotTrendGABWAnalytes(
          input$analyteGABW, input$ethGABWSel, input$ethGABW, 
          input$sexGABW, input$aabcGABW, input$tpnGABW
        )
      )
    } else {
      if(!input$trendGABWSelRatio){
        gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
          geom_text(aes(x=x, y=y, label=label)) + 
          scale_x_continuous(limits = c(0,1)) + 
          scale_y_continuous(limits = c(0,1)) + 
          theme_void() + theme(text = element_text(size = 24))
        return(gp)
      }
      return(
        plotTrendGABWRatio(
          input$numeratorGABW, input$denominatorGABW, input$ethGABWSelRatio, input$ethGABWRatio,
          input$sexGABWRatio, input$aabcGABWRatio, input$tpnGABWRatio
        )
      )
    }
  })
  
  
  output$heatGABW <- renderPlot({
    plotHeatGABW()
  })
  
  output$trendGABW <- renderPlot({
    plotTrendGABW()
  })
  
  
  
  ########### Ethnicity #############
  
  plotBoxplotEth <- eventReactive(c(input$ethSubmit, input$ethRatioSubmit, input$ENTHNICITY),{
    if(input$ENTHNICITY == "analytesEth"){
      return(plotBoxplotEthAnalytes(
        input$analyteEth, input$bwEth, input$gaEth, input$ethEthSel, 
        input$sexEth, input$aabcEth, input$tpnEth, input$compareEth
      ))
    } else {
      return(
        plotBoxplotEthRatio(
          input$numeratorEth, input$denominatorEth, input$bwEthRatio, input$gaEthRatio, input$ethEthSelRatio, 
          input$sexEthRatio, input$aabcEthRatio, input$tpnEthRatio, input$compareEthRatio
        )
      )
    }
  })
  
  
  output$boxplotEth <- renderPlot({
    plotBoxplotEth()
  })
  
  



  ############## Sex ############

  output$uiEthSex <- renderUI({
    switch(input$ethSexSel,
           "1" = selectInput(
             "ethSex",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethSex",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  
  output$uiEthSexRatio <- renderUI({
    switch(input$ethSexSelRatio,
           "1" = selectInput(
             "ethSexRatio",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethSexRatio",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  plotBoxplotSex <- eventReactive(c(input$sexSubmit, input$sexRatioSubmit, input$SEX),{
    if(input$SEX == "analytesSex"){
      return(plotBoxplotSexAnalytes(
        input$analyteSex, input$bwSex, input$gaSex, input$ethSexSel, input$ethSex, 
        input$aabcSex, input$tpnSex, input$compareSex
      ))
    } else {
      return(
        plotBoxplotSexRatio(
          input$numeratorSex, input$denominatorSex, input$bwSexRatio, input$gaSexRatio, input$ethSexSelRatio, input$ethSexRatio, 
          input$sexAabcRatio, input$tpnSexRatio, input$compareSexRatio
        )
      )
    }
  })
  
  
  output$boxplotSex <- renderPlot({
    plotBoxplotSex()
  })
  
  
  
  

  ############## aabc #############
  
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
             selected = 1:6
           )
    )
  })
  
  
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
             selected = 1:6
           )
    )
  })
  
  
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
  
  plotTrendplotAabc <- eventReactive(c(input$aabcSubmit, input$aabcRatioSubmit, input$AABC),{
    if(input$AABC == "analytesAabc"){
      if(!input$trendAabcSel){
        gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
          geom_text(aes(x=x, y=y, label=label)) + 
          scale_x_continuous(limits = c(0,1)) + 
          scale_y_continuous(limits = c(0,1)) + 
          theme_void() + theme(text = element_text(size = 24))
        return(gp)
      }
      return(
        plotTrendplotAabcAnalytes(
          input$analyteAabc, input$bwAabc, input$gaAabc, input$ethAabcSel, input$ethAabc, 
          input$sexAabc, input$tpnAabc, input$compareAabc
        )
      )
    } else {
      if(!input$trendAabcRatioSel){
        gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
          geom_text(aes(x=x, y=y, label=label)) + 
          scale_x_continuous(limits = c(0,1)) + 
          scale_y_continuous(limits = c(0,1)) + 
          theme_void() + theme(text = element_text(size = 24))
        return(gp)
      }
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
  
  #################### TPN #######################################
  output$uiEthTPN <- renderUI({
    switch(input$ethTPNSel,
           "1" = selectInput(
             "ethTPN",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethTPN",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  
  output$uiEthTPNRatio <- renderUI({
    switch(input$ethTPNSelRatio,
           "1" = selectInput(
             "ethTPNRatio",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethTPNRatio",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  plotBoxplotTPN <- eventReactive(c(input$tpnSubmit, input$tpnRatioSubmit, input$TPN),{
    if(input$TPN == "analytesTPN"){
      return(plotBoxplotTPNAnalytes(
        input$analyteTPN, input$bwTPN, input$gaTPN, input$ethTPNSel, input$ethTPN, 
        input$aabcTPN, input$sexTPN, input$compareTPN
      ))
    } else {
      return(
        plotBoxplotTPNRatio(
          input$numeratorTPN, input$denominatorTPN, input$bwTPNRatio, input$gaTPNRatio, input$ethTPNSelRatio, input$ethTPNRatio, 
          input$sexTPNRatio, input$sexTPNRatio, input$compareTPNRatio
        )
      )
    }
  })
  
  
  output$boxplotTPN <- renderPlot({
    plotBoxplotTPN()
  })
  
  
  
  ############ Multiple Comparison ###########
  
  #ethnicity selection: major groups or detailed groups (analytes panel)
  output$uiEthMC <- renderUI({
    switch(input$ethMCSel,
           "1" = selectInput(
             "ethMC",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 3
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
             selected = 3
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
