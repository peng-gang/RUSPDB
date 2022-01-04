library(shiny)
library(plotly)
#library(shinybusy)


source("parameters.R")
source("functions.R")

load('data/500KCleanDec2221.RData')
meta_data$race <- flag_race
meta_data$aac <- aac
meta_data$birthweight <- flag_bw
meta_data$ga <- flag_ga
meta_data$sex <- flag_sex
meta_data$tpn <- flag_tpn
meta_data$include <- idx_include

shinyServer(function(input, output, session) {
  btnBoxPlot <- eventReactive(c(input$btnSingle, input$btnRatio), {
    
    metaData <- NULL
    bwIdx <- NULL
    gaIdx <- NULL
    raceIdx <- NULL
    sexIdx <- NULL
    tpnIdx <- NULL
    compareIdx <- NULL
    
    if(input$meta == "analytes"){
      ana_sel <- input$analyte
      if(length(ana_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        
        #updateSelectInput(
        #  session, "analyte", selected = which(analytes_all=="C3")
        #)
        
        return(NULL)
        
        #meta <- meta_data[, which(analytes_all=="C3")]
        #return(list(meta=meta, name = "C3"))
        #return(list(meta=NULL, name = NULL))
      } else if(length(ana_sel) == 1){
        meta <- meta_data[, as.integer(ana_sel)]
        metaData <- list(meta=meta, name = analytes_all[as.integer(ana_sel)])
      } else {
        meta <- rowSums(meta_data[, as.integer(ana_sel)])
        metaData <- list(meta=meta, name = paste(analytes_all[sort(as.integer(ana_sel))], collapse = " + "))
      }
      
      bw_sel <- input$bw
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$ga
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$race
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      sex_sel <- input$sex
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      tpn_sel <- input$tpn
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compare)
      
    } else if(input$meta == "ratio"){
      num_sel <- input$numerator
      den_sel <- input$denominator
      meta_num <- NULL
      meta_den <- NULL
      name_num <- NULL
      name_den <- NULL
      
      if(length(num_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "numerator", selected = which(analytes_all=="C3")
        #)
        return(NULL)
        #meta_num <- meta_data[, which(analytes_all=="C3")]
        #name_num <- "C3"
        #return(list(meta=NULL, name = NULL))
      } else if(length(num_sel) == 1){
        meta_num <- meta_data[, as.integer(num_sel)]
        name_num <- analytes_all[as.integer(num_sel)]
      } else {
        meta_num <- rowSums(meta_data[, as.integer(num_sel)])
        name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
      }
      
      
      if(length(den_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "denominator", selected = which(analytes_all=="C2")
        #)
        return(NULL)
        #meta_den <- meta_data[, which(analytes_all=="C2")]
        #name_den <- "C2"
        #return(list(meta=NULL, name = NULL))
      } else if(length(den_sel) == 1){
        meta_den <- meta_data[, as.integer(den_sel)]
        name_den <- analytes_all[as.integer(den_sel)]
      } else {
        meta_den <- rowSums(meta_data[, as.integer(den_sel)])
        name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
      }
      
      meta <- meta_num / meta_den
      metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
      
      bw_sel <- input$bwRatio
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$gaRatio
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$raceRatio
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      sex_sel <- input$sexRatio
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      tpn_sel <- input$tpnRatio
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compareRatio)
    }
    
    if(compareIdx==1){
      #renderPlot(NULL)
      idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
      return(aacBoxPlot72(metaData$meta, aac, idx_include, idx_sel, metaData$name))
    } else {
      #renderPlot(NULL)
      idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
      #output$boxplot <- renderPlot(NULL)
      #idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
      return(aacBoxPlotCompare72(metaData$meta, aac, compareIdx, idx_include, idx_sel, 
                                 flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
                                 metaData$name))
    }
  })
  
  btnTrendPlot <- eventReactive(c(input$btnSingle, input$btnRatio), {
    metaData <- NULL
    bwIdx <- NULL
    gaIdx <- NULL
    raceIdx <- NULL
    sexIdx <- NULL
    tpnIdx <- NULL
    compareIdx <- NULL
    
    if(input$meta == "analytes"){
      ana_sel <- input$analyte
      if(length(ana_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        
        #updateSelectInput(
        #  session, "analyte", selected = which(analytes_all=="C3")
        #)
        
        return(NULL)
        
        #meta <- meta_data[, which(analytes_all=="C3")]
        #return(list(meta=meta, name = "C3"))
        #return(list(meta=NULL, name = NULL))
      } else if(length(ana_sel) == 1){
        meta <- meta_data[, as.integer(ana_sel)]
        metaData <- list(meta=meta, name = analytes_all[as.integer(ana_sel)])
      } else {
        meta <- rowSums(meta_data[, as.integer(ana_sel)])
        metaData <- list(meta=meta, name = paste(analytes_all[sort(as.integer(ana_sel))], collapse = " + "))
      }
      
      bw_sel <- input$bw
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$ga
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$race
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      sex_sel <- input$sex
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      tpn_sel <- input$tpn
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compare)
      
    } else if(input$meta == "ratio"){
      num_sel <- input$numerator
      den_sel <- input$denominator
      meta_num <- NULL
      meta_den <- NULL
      name_num <- NULL
      name_den <- NULL
      
      if(length(num_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "numerator", selected = which(analytes_all=="C3")
        #)
        return(NULL)
        #meta_num <- meta_data[, which(analytes_all=="C3")]
        #name_num <- "C3"
        #return(list(meta=NULL, name = NULL))
      } else if(length(num_sel) == 1){
        meta_num <- meta_data[, as.integer(num_sel)]
        name_num <- analytes_all[as.integer(num_sel)]
      } else {
        meta_num <- rowSums(meta_data[, as.integer(num_sel)])
        name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
      }
      
      
      if(length(den_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "denominator", selected = which(analytes_all=="C2")
        #)
        return(NULL)
        #meta_den <- meta_data[, which(analytes_all=="C2")]
        #name_den <- "C2"
        #return(list(meta=NULL, name = NULL))
      } else if(length(den_sel) == 1){
        meta_den <- meta_data[, as.integer(den_sel)]
        name_den <- analytes_all[as.integer(den_sel)]
      } else {
        meta_den <- rowSums(meta_data[, as.integer(den_sel)])
        name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
      }
      
      meta <- meta_num / meta_den
      metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
      
      bw_sel <- input$bwRatio
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$gaRatio
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$raceRatio
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      sex_sel <- input$sexRatio
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      tpn_sel <- input$tpnRatio
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compareRatio)
    }
    
    if(compareIdx==1){
      #output$trendplot <- renderPlot(NULL)
      #idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
      idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
      return(aacTrend(metaData$meta, aac, idx_include, idx_sel, metaData$name))
    } else {
      #output$trendplot <- renderPlot(NULL)
      idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
      return(aacTrendCompare(metaData$meta, aac, compareIdx, idx_include, idx_sel, 
                             flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
                             metaData$name))
    }
  })
  
  
  # 
  # output$boxplot <- renderPlotly({
  #   btnBoxPlot()
  # })
  # 
  
  btnBoxPlot2 <- eventReactive(c(input$btnSingle2, input$btnRatio2), {
    
    metaData <- NULL
    bwIdx <- NULL
    gaIdx <- NULL
    raceIdx <- NULL
    sexIdx <- NULL
    tpnIdx <- NULL
    compareIdx <- NULL
    
    if(input$meta2 == "analytes2"){
      ana_sel <- input$analyte2
      if(length(ana_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        
        #updateSelectInput(
        #  session, "analyte", selected = which(analytes_all=="C3")
        #)
        
        return(NULL)
        
      } else if(length(ana_sel) == 1){
        meta <- meta_data[, as.integer(ana_sel)]
        metaData <- list(meta=meta, name = analytes_all[as.integer(ana_sel)])
      } else {
        meta <- rowSums(meta_data[, as.integer(ana_sel)])
        metaData <- list(meta=meta, name = paste(analytes_all[sort(as.integer(ana_sel))], collapse = " + "))
      }
      
      bw_sel <- input$bw2
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      
      race_sel <- input$race2
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      sex_sel <- input$sex2
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      tpn_sel <- input$tpn2
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compare2)
      
    } else if(input$meta2 == "ratio2"){
      num_sel <- input$numerator2
      den_sel <- input$denominator2
      meta_num <- NULL
      meta_den <- NULL
      name_num <- NULL
      name_den <- NULL
      
      if(length(num_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "numerator", selected = which(analytes_all=="C3")
        #)
        return(NULL)
        #meta_num <- meta_data[, which(analytes_all=="C3")]
        #name_num <- "C3"
        #return(list(meta=NULL, name = NULL))
      } else if(length(num_sel) == 1){
        meta_num <- meta_data[, as.integer(num_sel)]
        name_num <- analytes_all[as.integer(num_sel)]
      } else {
        meta_num <- rowSums(meta_data[, as.integer(num_sel)])
        name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
      }
      
      
      if(length(den_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "denominator", selected = which(analytes_all=="C2")
        #)
        return(NULL)
        #meta_den <- meta_data[, which(analytes_all=="C2")]
        #name_den <- "C2"
        #return(list(meta=NULL, name = NULL))
      } else if(length(den_sel) == 1){
        meta_den <- meta_data[, as.integer(den_sel)]
        name_den <- analytes_all[as.integer(den_sel)]
      } else {
        meta_den <- rowSums(meta_data[, as.integer(den_sel)])
        name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
      }
      
      meta <- meta_num / meta_den
      metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
      
      bw_sel <- input$bwRatio2
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$gaRatio2
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$raceRatio2
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      sex_sel <- input$sexRatio2
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      tpn_sel <- input$tpnRatio2
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compareRatio2)
    }
    
    if(compareIdx==1){
      #renderPlot(NULL)
      idx_sel <- bwIdx  & raceIdx & sexIdx & tpnIdx
      data <- meta_data[idx_sel & meta_data$include, ]
      print(data)
      variable <- colnames(meta_data)[as.integer(input$analyte2)]
      ggplot(data=data, aes_string(x="ga", y=variable)) + geom_boxplot()
    } else {
      idx_sel <- bwIdx  & raceIdx & sexIdx & tpnIdx
      data <- meta_data[idx_sel & meta_data$include, ]
      variable <- colnames(meta_data)[as.integer(input$analyte2)]
      ggplot(data=data, aes_string(x="ga", y=variable)) + geom_boxplot()
    }
  })
  
  btnBoxPlot3 <- eventReactive(c(input$btnSingle2, input$btnRatio2), {
    
    metaData <- NULL
    bwIdx <- NULL
    gaIdx <- NULL
    raceIdx <- NULL
    sexIdx <- NULL
    tpnIdx <- NULL
    compareIdx <- NULL
    
    if(input$meta3 == "analytes3"){
      ana_sel <- input$analyte3
      if(length(ana_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        
        #updateSelectInput(
        #  session, "analyte", selected = which(analytes_all=="C3")
        #)
        
        return(NULL)
        
      } else if(length(ana_sel) == 1){
        meta <- meta_data[, as.integer(ana_sel)]
        metaData <- list(meta=meta, name = analytes_all[as.integer(ana_sel)])
      } else {
        meta <- rowSums(meta_data[, as.integer(ana_sel)])
        metaData <- list(meta=meta, name = paste(analytes_all[sort(as.integer(ana_sel))], collapse = " + "))
      }
      
      bw_sel <- input$bw3
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$ga3
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$race3
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      
      tpn_sel <- input$tpn3
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compare3)
      
    } else if(input$meta3 == "ratio3"){
      num_sel <- input$numerator3
      den_sel <- input$denominator3
      meta_num <- NULL
      meta_den <- NULL
      name_num <- NULL
      name_den <- NULL
      
      if(length(num_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "numerator", selected = which(analytes_all=="C3")
        #)
        return(NULL)
        #meta_num <- meta_data[, which(analytes_all=="C3")]
        #name_num <- "C3"
        #return(list(meta=NULL, name = NULL))
      } else if(length(num_sel) == 1){
        meta_num <- meta_data[, as.integer(num_sel)]
        name_num <- analytes_all[as.integer(num_sel)]
      } else {
        meta_num <- rowSums(meta_data[, as.integer(num_sel)])
        name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
      }
      
      
      if(length(den_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "denominator", selected = which(analytes_all=="C2")
        #)
        return(NULL)
        #meta_den <- meta_data[, which(analytes_all=="C2")]
        #name_den <- "C2"
        #return(list(meta=NULL, name = NULL))
      } else if(length(den_sel) == 1){
        meta_den <- meta_data[, as.integer(den_sel)]
        name_den <- analytes_all[as.integer(den_sel)]
      } else {
        meta_den <- rowSums(meta_data[, as.integer(den_sel)])
        name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
      }
      
      meta <- meta_num / meta_den
      metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
      
      bw_sel <- input$bwRatio3
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$gaRatio3
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$raceRatio3
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      
      tpn_sel <- input$tpnRatio3
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compareRatio3)
    }
    
    if(compareIdx==1){
      #renderPlot(NULL)
      idx_sel <- bwIdx & gaIdx & raceIdx & tpnIdx
      data <- meta_data[idx_sel & meta_data$include, ]
      data <- data[data$sex!="NA", ]
      variable <- colnames(meta_data)[as.integer(input$analyte3)]
      ggplot(data=data, aes_string(x="sex", y=variable)) + geom_boxplot()
    } else {
      idx_sel <- bwIdx & gaIdx & raceIdx  & tpnIdx
      data <- meta_data[idx_sel & meta_data$include, ]
      data <- data[data$sex!="NA", ]
      variable <- colnames(meta_data)[as.integer(input$analyte3)]
      ggplot(data=data, aes_string(x="sex", y=variable)) + geom_boxplot()
    }
  })
  
  btnBoxPlot4 <- eventReactive(c(input$btnSingle2, input$btnRatio2), {
    metaData <- NULL
    bwIdx <- NULL
    gaIdx <- NULL
    raceIdx <- NULL
    sexIdx <- NULL
    tpnIdx <- NULL
    compareIdx <- NULL
    
    if(input$meta4 == "analytes4"){
      ana_sel <- input$analyte4
      if(length(ana_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        
        
        return(NULL)
        
      } else if(length(ana_sel) == 1){
        meta <- meta_data[, as.integer(ana_sel)]
        metaData <- list(meta=meta, name = analytes_all[as.integer(ana_sel)])
      } else {
        meta <- rowSums(meta_data[, as.integer(ana_sel)])
        metaData <- list(meta=meta, name = paste(analytes_all[sort(as.integer(ana_sel))], collapse = " + "))
      }
      
      bw_sel <- input$bw4
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$ga4
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$race4
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      
      tpn_sel <- input$tpn4
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      ga_sel <- input$ga4
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }

      
      sex_sel <- input$sex4
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      compareIdx <- as.integer(input$compare4)
      
    } else if(input$meta4 == "ratio4"){
      num_sel <- input$numerator4
      den_sel <- input$denominator4
      meta_num <- NULL
      meta_den <- NULL
      name_num <- NULL
      name_den <- NULL
      
      if(length(num_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "numerator", selected = which(analytes_all=="C3")
        #)
        return(NULL)
        #meta_num <- meta_data[, which(analytes_all=="C3")]
        #name_num <- "C3"
        #return(list(meta=NULL, name = NULL))
      } else if(length(num_sel) == 1){
        meta_num <- meta_data[, as.integer(num_sel)]
        name_num <- analytes_all[as.integer(num_sel)]
      } else {
        meta_num <- rowSums(meta_data[, as.integer(num_sel)])
        name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
      }
      
      
      if(length(den_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "denominator", selected = which(analytes_all=="C2")
        #)
        return(NULL)
        #meta_den <- meta_data[, which(analytes_all=="C2")]
        #name_den <- "C2"
        #return(list(meta=NULL, name = NULL))
      } else if(length(den_sel) == 1){
        meta_den <- meta_data[, as.integer(den_sel)]
        name_den <- analytes_all[as.integer(den_sel)]
      } else {
        meta_den <- rowSums(meta_data[, as.integer(den_sel)])
        name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
      }
      
      meta <- meta_num / meta_den
      metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
      
      bw_sel <- input$bwRatio4
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$gaRatio4
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      
      
      
      tpn_sel <- input$tpnRatio4
      if(length(tpn_sel) == length(tpn_group)){
        tpnIdx <- rep(TRUE, nrow(meta_data))
      } else {
        tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
      }
      
      compareIdx <- as.integer(input$compareRatio4)
    }
    
    if(compareIdx==1){
      #renderPlot(NULL)
      idx_sel <- bwIdx & gaIdx  & tpnIdx & sexIdx 
      data <- meta_data[idx_sel & meta_data$include, ]
      variable <- colnames(meta_data)[as.integer(input$analyte4)]
      ggplot(data=data, aes_string(x="race", y=variable)) + geom_boxplot()
    } else {
      idx_sel <- bwIdx & gaIdx   & tpnIdx & sexIdx 
      data <- meta_data[idx_sel & meta_data$include, ]
      variable <- colnames(meta_data)[as.integer(input$analyte4)]
      ggplot(data=data, aes_string(x="race", y=variable)) + geom_boxplot()
    }
  })
  
  btnBoxPlot5 <- eventReactive(c(input$btnSingle2, input$btnRatio2), {
    metaData <- NULL
    bwIdx <- NULL
    gaIdx <- NULL
    raceIdx <- NULL
    sexIdx <- NULL
    tpnIdx <- NULL
    compareIdx <- NULL
    
    if(input$meta5 == "analytes5"){
      ana_sel <- input$analyte5
      if(length(ana_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        
        
        return(NULL)
        
      } else if(length(ana_sel) == 1){
        meta <- meta_data[, as.integer(ana_sel)]
        metaData <- list(meta=meta, name = analytes_all[as.integer(ana_sel)])
      } else {
        meta <- rowSums(meta_data[, as.integer(ana_sel)])
        metaData <- list(meta=meta, name = paste(analytes_all[sort(as.integer(ana_sel))], collapse = " + "))
      }
      
      bw_sel <- input$bw5
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$ga5
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      race_sel <- input$race5
      if(length(race_sel) == length(race_group)){
        raceIdx <- rep(TRUE, nrow(meta_data))
      } else {
        raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
      }
      
      
      
      ga_sel <- input$ga5
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      
      sex_sel <- input$sex5
      if(length(sex_sel) == length(sex_group)){
        sexIdx <- rep(TRUE, nrow(meta_data))
      } else {
        sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
      }
      
      compareIdx <- as.integer(input$compare5)
      
    } else if(input$meta5 == "ratio5"){
      num_sel <- input$numerator5
      den_sel <- input$denominator5
      meta_num <- NULL
      meta_den <- NULL
      name_num <- NULL
      name_den <- NULL
      
      if(length(num_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "numerator", selected = which(analytes_all=="C3")
        #)
        return(NULL)
        #meta_num <- meta_data[, which(analytes_all=="C3")]
        #name_num <- "C3"
        #return(list(meta=NULL, name = NULL))
      } else if(length(num_sel) == 1){
        meta_num <- meta_data[, as.integer(num_sel)]
        name_num <- analytes_all[as.integer(num_sel)]
      } else {
        meta_num <- rowSums(meta_data[, as.integer(num_sel)])
        name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
      }
      
      
      if(length(den_sel) == 0){
        ## ERROR MESSAGE
        showModal(
          modalDialog(
            title = "Warning!",
            "At least one analyte should be selected and the default analyte is selected",
            easyClose = TRUE
          )
        )
        #updateSelectInput(
        #  session, "denominator", selected = which(analytes_all=="C2")
        #)
        return(NULL)
        #meta_den <- meta_data[, which(analytes_all=="C2")]
        #name_den <- "C2"
        #return(list(meta=NULL, name = NULL))
      } else if(length(den_sel) == 1){
        meta_den <- meta_data[, as.integer(den_sel)]
        name_den <- analytes_all[as.integer(den_sel)]
      } else {
        meta_den <- rowSums(meta_data[, as.integer(den_sel)])
        name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
      }
      
      meta <- meta_num / meta_den
      metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
      
      bw_sel <- input$bwRatio5
      if(length(bw_sel) == length(bw_group)){
        bwIdx <- rep(TRUE, nrow(meta_data))
      } else {
        bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
      }
      
      ga_sel <- input$gaRatio5
      if(length(ga_sel) == length(ga_group)){
        gaIdx <- rep(TRUE, nrow(meta_data))
      } else {
        gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
      }
      
      
    
      
      compareIdx <- as.integer(input$compareRatio5)
    }
    
    if(compareIdx==1){
      #renderPlot(NULL)
      idx_sel <- bwIdx & gaIdx  & raceIdx & sexIdx 
      data <- meta_data[idx_sel & meta_data$include, ]
      data <- data[data$tpn!="NA",]
      variable <- colnames(meta_data)[as.integer(input$analyte4)]
      ggplot(data=data, aes_string(x="tpn", y=variable)) + geom_boxplot()
    } else {
      idx_sel <- bwIdx & gaIdx   & raceIdx & sexIdx 
      data <- meta_data[idx_sel & meta_data$include, ]
      data <- data[data$tpn!="NA",]
      variable <- colnames(meta_data)[as.integer(input$analyte4)]
      ggplot(data=data, aes_string(x="tpn", y=variable)) + geom_boxplot()
    }
  })
  
  
  output$boxplot2 <- renderPlotly({
    btnBoxPlot2()
  })
  
  output$boxplot <- renderPlotly({
    btnBoxPlot()
  })
  
  output$boxplot3 <- renderPlotly({
    btnBoxPlot3()
  })
  
  output$boxplot4 <- renderPlotly({
    btnBoxPlot4()
  })
  
  output$boxplot5 <- renderPlotly({
    btnBoxPlot5()
  })

  
  output$trendplot <- renderPlotly({
    btnTrendPlot()
  })
  
  
  output$uiEthMC <- renderUI({
    switch(input$ethMCSel,
           "1" = selectInput(
             "ethMC",
             label = h4("Major Ethnicity Groups"),
             choices = makeList(race_group),
             multiple = TRUE,
             selected = 1
           ),
           
           "2" = selectInput(
             "gaMC",
             label = h4("Detailed Ethnicity Groups"),
             choices = makeList(race_group_details),
             multiple = TRUE,
             selected = 1
           )
        )
  })
})
