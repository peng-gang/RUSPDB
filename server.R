library(shiny)
library(plotly)
#library(shinybusy)


source("parameters.R")
source("functions.R")

load('/Users/oliviazhang/Desktop/500KClean.RData')

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
  
  # btnBoxPlotRatio <- eventReactive(input$btnRatio, {
  #   compareIdx <- as.integer(input$compareRatio)
  #   
  #   #meta data
  #   metaData <- NULL
  #   num_sel <- input$numerator
  #   den_sel <- input$denominator
  #   meta_num <- NULL
  #   meta_den <- NULL
  #   name_num <- NULL
  #   name_den <- NULL
  #   
  #   if(length(num_sel) == 0){
  #     ## ERROR MESSAGE
  #     showModal(
  #       modalDialog(
  #         title = "Warning!",
  #         "At least one analyte should be selected and the default analyte is selected",
  #         easyClose = TRUE
  #       )
  #     )
  #     updateSelectInput(
  #       session, "numerator", selected = which(analytes_all=="C3")
  #     )
  #     return(NULL)
  #     #meta_num <- meta_data[, which(analytes_all=="C3")]
  #     #name_num <- "C3"
  #     #return(list(meta=NULL, name = NULL))
  #   } else if(length(num_sel) == 1){
  #     meta_num <- meta_data[, as.integer(num_sel)]
  #     name_num <- analytes_all[as.integer(num_sel)]
  #   } else {
  #     meta_num <- rowSums(meta_data[, as.integer(num_sel)])
  #     name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
  #   }
  #   
  #   
  #   if(length(den_sel) == 0){
  #     ## ERROR MESSAGE
  #     showModal(
  #       modalDialog(
  #         title = "Warning!",
  #         "At least one analyte should be selected and the default analyte is selected",
  #         easyClose = TRUE
  #       )
  #     )
  #     updateSelectInput(
  #       session, "denominator", selected = which(analytes_all=="C2")
  #     )
  #     return(NULL)
  #     #meta_den <- meta_data[, which(analytes_all=="C2")]
  #     #name_den <- "C2"
  #     #return(list(meta=NULL, name = NULL))
  #   } else if(length(den_sel) == 1){
  #     meta_den <- meta_data[, as.integer(den_sel)]
  #     name_den <- analytes_all[as.integer(den_sel)]
  #   } else {
  #     meta_den <- rowSums(meta_data[, as.integer(den_sel)])
  #     name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
  #   }
  #   
  #   meta <- meta_num / meta_den
  #   metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
  #   
  #   # bwIdx
  #   bwIdx <- NULL
  #   bw_sel <- input$bwRatio
  #   if(length(bw_sel) == length(bw_group)){
  #     bwIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
  #   }
  #   
  #   #gaIdx
  #   gaIdx <- NULL
  #   ga_sel <- input$gaRatio
  #   if(length(ga_sel) == length(ga_group)){
  #     gaIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
  #   }
  #   
  #   #raceIdx
  #   raceIdx <- NULL
  #   race_sel <- input$raceRatio
  #   if(length(race_sel) == length(race_group)){
  #     raceIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
  #   }
  #   
  #   #sexIdx
  #   sexIdx <- NULL
  #   sex_sel <- input$sexRatio
  #   if(length(sex_sel) == length(sex_group)){
  #     sexIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
  #   }
  #   
  #   #tpnIdx
  #   tpnIdx <- NULL
  #   tpn_sel <- input$tpnRatio
  #   if(length(tpn_sel) == length(tpn_group)){
  #     tpnIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
  #   }
  #   
  #   if(compareIdx==1){
  #     #renderPlot(NULL)
  #     idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
  #     aacBoxPlot(metaData$meta, flag_aac, idx_include, idx_sel, metaData$name)
  #   } else {
  #     #renderPlot(NULL)
  #     idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
  #     #output$boxplot <- renderPlot(NULL)
  #     #idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
  #     aacBoxPlotCompare(metaData$meta, flag_aac, compareIdx, idx_include, idx_sel, 
  #                       flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
  #                       metaData$name)
  #   }
  # })
  # 
  # btnTrendPlotRatio <- eventReactive(input$btnRatio, {
  #   compareIdx <- as.integer(input$compareRatio)
  #   
  #   #meta data
  #   metaData <- NULL
  #   num_sel <- input$numerator
  #   den_sel <- input$denominator
  #   meta_num <- NULL
  #   meta_den <- NULL
  #   name_num <- NULL
  #   name_den <- NULL
  #   
  #   if(length(num_sel) == 0){
  #     ## ERROR MESSAGE
  #     showModal(
  #       modalDialog(
  #         title = "Warning!",
  #         "At least one analyte should be selected and the default analyte is selected",
  #         easyClose = TRUE
  #       )
  #     )
  #     updateSelectInput(
  #       session, "numerator", selected = which(analytes_all=="C3")
  #     )
  #     return(NULL)
  #     #meta_num <- meta_data[, which(analytes_all=="C3")]
  #     #name_num <- "C3"
  #     #return(list(meta=NULL, name = NULL))
  #   } else if(length(num_sel) == 1){
  #     meta_num <- meta_data[, as.integer(num_sel)]
  #     name_num <- analytes_all[as.integer(num_sel)]
  #   } else {
  #     meta_num <- rowSums(meta_data[, as.integer(num_sel)])
  #     name_num <- paste0("(", paste(analytes_all[sort(as.integer(num_sel))], collapse = " + "), ")")
  #   }
  #   
  #   
  #   if(length(den_sel) == 0){
  #     ## ERROR MESSAGE
  #     showModal(
  #       modalDialog(
  #         title = "Warning!",
  #         "At least one analyte should be selected and the default analyte is selected",
  #         easyClose = TRUE
  #       )
  #     )
  #     updateSelectInput(
  #       session, "denominator", selected = which(analytes_all=="C2")
  #     )
  #     return(NULL)
  #     #meta_den <- meta_data[, which(analytes_all=="C2")]
  #     #name_den <- "C2"
  #     #return(list(meta=NULL, name = NULL))
  #   } else if(length(den_sel) == 1){
  #     meta_den <- meta_data[, as.integer(den_sel)]
  #     name_den <- analytes_all[as.integer(den_sel)]
  #   } else {
  #     meta_den <- rowSums(meta_data[, as.integer(den_sel)])
  #     name_den <- paste0("(", paste(analytes_all[sort(as.integer(den_sel))], collapse = " + "), ")")
  #   }
  #   
  #   meta <- meta_num / meta_den
  #   metaData <- list(meta=meta, name = paste0(name_num, "/", name_den))
  #   
  #   # bwIdx
  #   bwIdx <- NULL
  #   bw_sel <- input$bwRatio
  #   if(length(bw_sel) == length(bw_group)){
  #     bwIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     bwIdx <- flag_bw %in% bw_group[as.integer(bw_sel)]
  #   }
  #   
  #   #gaIdx
  #   gaIdx <- NULL
  #   ga_sel <- input$gaRatio
  #   if(length(ga_sel) == length(ga_group)){
  #     gaIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     gaIdx <- flag_ga %in% ga_group[as.integer(ga_sel)]
  #   }
  #   
  #   #raceIdx
  #   raceIdx <- NULL
  #   race_sel <- input$raceRatio
  #   if(length(race_sel) == length(race_group)){
  #     raceIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     raceIdx <- flag_race %in% race_group[as.integer(race_sel)]
  #   }
  #   
  #   #sexIdx
  #   sexIdx <- NULL
  #   sex_sel <- input$sexRatio
  #   if(length(sex_sel) == length(sex_group)){
  #     sexIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     sexIdx <- flag_sex %in% sex_group[as.integer(sex_sel)]
  #   }
  #   
  #   #tpnIdx
  #   tpnIdx <- NULL
  #   tpn_sel <- input$tpnRatio
  #   if(length(tpn_sel) == length(tpn_group)){
  #     tpnIdx <- rep(TRUE, nrow(meta_data))
  #   } else {
  #     tpnIdx <- flag_tpn %in% tpn_group[as.integer(tpn_sel)]
  #   }
  #   
  #   if(compareIdx==1){
  #     #output$trendplot <- renderPlot(NULL)
  #     idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
  #     aacTrend(metaData$meta, aac, idx_include, idx_sel, metaData$name)
  #   } else {
  #     #output$trendplot <- renderPlot(NULL)
  #     idx_sel <- bwIdx & gaIdx & raceIdx & sexIdx & tpnIdx
  #     aacTrendCompare(metaData$meta, aac, compareIdx, idx_include, idx_sel, 
  #                     flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
  #                     metaData$name)
  #   }
  # })
  
  output$boxplot <- renderPlotly({
    #bp <- NULL
    #show_spinner()
    # if(as.integer(compareIndex())==1){
    #   renderPlot(NULL)
    #   idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
    #   aacBoxPlot(getMeta()$meta, flag_aac, idx_include, idx_sel, getMeta()$name)
    # } else {
    #   #output$boxplot <- renderPlot(NULL)
    #   idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
    #   aacBoxPlotCompare(getMeta()$meta, flag_aac, as.integer(compareIndex()), idx_include, idx_sel, 
    #              flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
    #              getMeta()$name)
    # }
    #hide_spinner()
    #bp
    
    
    btnBoxPlot()
  })
  
  
  output$trendplot <- renderPlotly({
    #tp <- NULL
    #show_spinner()
    # if(as.integer(compareIndex())==1){
    #   #output$trendplot <- renderPlot(NULL)
    #   idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
    #   aacTrend(getMeta()$meta, aac, idx_include, idx_sel, getMeta()$name)
    # } else {
    #   #output$trendplot <- renderPlot(NULL)
    #   idx_sel <- bwIndex() & gaIndex() & raceIndex() & sexIndex() & tpnIndex()
    #   aacTrendCompare(getMeta()$meta, aac, as.integer(compareIndex()), idx_include, idx_sel, 
    #                   flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
    #                   getMeta()$name)
    # }
    #hide_spinner()
    #tp
    
    btnTrendPlot()
    
  })
  
})
