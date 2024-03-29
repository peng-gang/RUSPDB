library(shiny)
#library(plotly)
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
             selected = 1:length(ethnicity_group_details)
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
             selected = 1:length(ethnicity_group_details)
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
      # if(!input$trendGABWSel){
      #   gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
      #     geom_text(aes(x=x, y=y, label=label)) + 
      #     scale_x_continuous(limits = c(0,1)) + 
      #     scale_y_continuous(limits = c(0,1)) + 
      #     theme_void() + theme(text = element_text(size = 24))
      #   return(gp)
      # }
      return(
        plotTrendGABWAnalytes(
          input$analyteGABW, input$ethGABWSel, input$ethGABW, 
          input$sexGABW, input$aabcGABW, input$tpnGABW
        )
      )
    } else {
      # if(!input$trendGABWSelRatio){
      #   gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
      #     geom_text(aes(x=x, y=y, label=label)) + 
      #     scale_x_continuous(limits = c(0,1)) + 
      #     scale_y_continuous(limits = c(0,1)) + 
      #     theme_void() + theme(text = element_text(size = 24))
      #   return(gp)
      # }
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
  
  output$uiEthEth <- renderUI({
    switch(input$ethEthSel,
           "1" = selectInput(
             "ethEth",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethEth",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  output$uiEthEthRatio <- renderUI({
    switch(input$ethEthSelRatio,
           "1" = selectInput(
             "ethEthRatio",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 1:length(ethnicity_group)
           ),
           
           "2" = selectInput(
             "ethEthRatio",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(ethnicity_group_details),
             multiple = TRUE,
             selected = 1:6
           )
    )
  })
  
  plotBoxplotEth <- eventReactive(c(input$ethSubmit, input$ethRatioSubmit, input$ETHNICITY),{
    if(input$ETHNICITY == "analytesEth"){
      return(plotBoxplotEthAnalytes(
        input$analyteEth, input$bwEth, input$gaEth, input$ethEthSel, input$ethEth,
        input$sexEth, input$aabcEth, input$tpnEth, input$compareEth
      ))
    } else {
      return(
        plotBoxplotEthRatio(
          input$numeratorEth, input$denominatorEth, input$bwEthRatio, input$gaEthRatio, input$ethEthSelRatio, input$ethEthRatio,
          input$sexEthRatio, input$aabcEthRatio, input$tpnEthRatio, input$compareEthRatio
        )
      )
    }
  })
  
  createTableEth <- eventReactive(c(input$ethSubmit, input$ethRatioSubmit, input$ETHNICITY),{
    if(input$ETHNICITY == "analytesEth"){
      return(createTableEthAnalytes(
        input$analyteEth, input$bwEth, input$gaEth, input$ethEthSel, input$ethEth,
        input$sexEth, input$aabcEth, input$tpnEth, input$compareEth
      ))
    } else {
      return(
        createTableEthRatio(
          input$numeratorEth, input$denominatorEth, input$bwEthRatio, input$gaEthRatio, input$ethEthSelRatio, input$ethEthRatio,
          input$sexEthRatio, input$aabcEthRatio, input$tpnEthRatio, input$compareEthRatio
        )
      )
    }
  })
  
  
  output$boxplotEth <- renderPlot({
    plotBoxplotEth()
  })
  
  output$tableEth <- renderDT(
    createTableEth()
  )
  
  



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
             selected = 1:length(ethnicity_group_details)
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
             selected = 1:length(ethnicity_group_details)
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
          input$aabcSexRatio, input$tpnSexRatio, input$compareSexRatio
        )
      )
    }
  })
  
  createTableSex <- eventReactive(c(input$sexSubmit, input$sexRatioSubmit, input$SEX),{
    if(input$SEX == "analytesSex"){
      return(createTableSexAnalytes(
        input$analyteSex, input$bwSex, input$gaSex, input$ethSexSel, input$ethSex, 
        input$aabcSex, input$tpnSex, input$compareSex
      ))
    } else {
      return(
        createTableSexRatio(
          input$numeratorSex, input$denominatorSex, input$bwSexRatio, input$gaSexRatio, input$ethSexSelRatio, input$ethSexRatio, 
          input$aabcSexRatio, input$tpnSexRatio, input$compareSexRatio
        )
      )
    }
  })
  
  
  output$boxplotSex <- renderPlot({
    plotBoxplotSex()
  })
  
  
  output$tableSex <- renderDT(
  #   {
  #   if(input$compareSex == "1"){
  #     datatable(createTableSex(),  
  #               style = "bootstrap4",
  #               rownames = FALSE) %>% 
  #       formatRound(2, 2)
  #       
  #   } else {
  #     datatable(createTableSex(),
  #               style = "bootstrap4",
  #               rownames = FALSE) %>% 
  #       formatRound(3, 2)
  #   }
  # }
    createTableSex()
  )
  
  
  

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
             selected = 1:length(ethnicity_group_details)
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
             selected = 1:length(ethnicity_group_details)
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
      # if(!input$trendAabcSel){
      #   gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
      #     geom_text(aes(x=x, y=y, label=label)) + 
      #     scale_x_continuous(limits = c(0,1)) + 
      #     scale_y_continuous(limits = c(0,1)) + 
      #     theme_void() + theme(text = element_text(size = 24))
      #   return(gp)
      # }
      return(
        plotTrendplotAabcAnalytes(
          input$analyteAabc, input$bwAabc, input$gaAabc, input$ethAabcSel, input$ethAabc, 
          input$sexAabc, input$tpnAabc, input$compareAabc
        )
      )
    } else {
      # if(!input$trendAabcRatioSel){
      #   gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select 'Show trending plot' and click 'Submit' from the left panel to show this figure.\n It will take about two minutes because of large sample size.")) + 
      #     geom_text(aes(x=x, y=y, label=label)) + 
      #     scale_x_continuous(limits = c(0,1)) + 
      #     scale_y_continuous(limits = c(0,1)) + 
      #     theme_void() + theme(text = element_text(size = 24))
      #   return(gp)
      # }
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
             selected = 1:length(ethnicity_group_details)
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
             selected = 1:length(ethnicity_group_details)
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
          input$aabcTPNRatio, input$sexTPNRatio, input$compareTPNRatio
        )
      )
    }
  })
  
  createTableTPN <- eventReactive(c(input$tpnSubmit, input$tpnRatioSubmit, input$TPN),{
    if(input$TPN == "analytesTPN"){
      return(createTableTPNAnalytes(
        input$analyteTPN, input$bwTPN, input$gaTPN, input$ethTPNSel, input$ethTPN, 
        input$aabcTPN, input$sexTPN, input$compareTPN
      ))
    } else {
      return(
        createTableTPNRatio(
          input$numeratorTPN, input$denominatorTPN, input$bwTPNRatio, input$gaTPNRatio, input$ethTPNSelRatio, input$ethTPNRatio, 
          input$aabcTPNRatio, input$sexTPNRatio, input$compareTPNRatio
        )
      )
    }
  })
  
  
  output$boxplotTPN <- renderPlot({
    plotBoxplotTPN()
  })
  
  output$tableTPN <- renderDT(
    createTableTPN()
  )
  
  
  
  ############ Multiple Comparison ###########
  
  #ethnicity selection: major groups or detailed groups (analytes panel)
  output$uiEthMC <- renderUI({
    switch(input$ethMCSel,
           "1" = selectInput(
             "ethMC",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(ethnicity_group),
             multiple = TRUE,
             selected = 2
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
  
  
  output$uiMC <- renderUI({
    plotOutput("figureMC", height = "300px")
  })
  
  # boxplot for multiple comparison
  plotMC <- eventReactive(c(input$mcSubmit, input$mcRatioSubmit, input$multiCompare),{
    if(input$multiCompare == "analytesMC"){
      output$uiMC <- renderUI({
        plotOutput("figureMC", height = {
          paste0(ceiling(length(isolate(input$analyteMC))/3)*300, "px")
        })
      })
      
      return(plotMCAnalytes(
        input$analyteMC, input$bwMC, input$gaMC, input$ethMCSel, input$ethMC, 
        input$aabcMC, input$sexMC, input$tpnMC
      ))
    } else {
      output$uiMC <- renderUI({
        plotOutput("figureMC", height = "300px")
      })
      
      return(plotMCRatio(
        input$numeratorMC, input$denominatorMC, input$bwMCRatio, input$gaMCRatio, 
        input$ethMCSelRatio, input$ethMCRatio, 
        input$aabcMCRatio, input$sexMCRatio, input$tpnMCRatio
      ))
    }
  })
  
  createTableMC <- eventReactive(c(input$mcSubmit, input$mcRatioSubmit, input$multiCompare),{
    if(input$multiCompare == "analytesMC"){
      return(createTableMCAnalytes(
        input$analyteMC, input$bwMC, input$gaMC, input$ethMCSel, input$ethMC, 
        input$aabcMC, input$sexMC, input$tpnMC
      ))
    } else {
      return(createTableMCRatio(
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
  
  
  output$tableMC <- renderDT(
    createTableMC()
  )
  
  
  output$infoMC <- renderUI({
    getMCInfo()
  })

  
  
  df$OMIM[complete.cases(df$OMIM)] <- paste0("<a href='",df$OMIM[complete.cases(df$OMIM)],"' target='_blank'>",df$OMIM[complete.cases(df$OMIM)],"</a>")
  render_dt = function(data) {
    renderDT(data)
  }
  dt_1 <- datatable(df[1:83,], rownames = FALSE, escape=F, options = list(pageLength = 100),style = "bootstrap4")
  dt_2 <- datatable(df[84:91,], rownames = FALSE, escape=F, options = list(pageLength = 100),style = "bootstrap4")
  
  output$tab_1 = render_dt(dt_1)
  output$tab_2 = renderTable(df[84:91,],colnames=F,na=" ")
  
})
