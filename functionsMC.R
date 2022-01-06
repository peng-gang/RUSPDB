# functions for multiple comparison

library(ggplot2)
library(shiny)
library(effsize)


# create boxplot for analyte(s) between selected group and common group
# input: selected index from ui
plotMCAnalytes <- function(
  analyteMC, bwMC, gaMC, ethMCSel, ethMC, 
  aabcMC, sexMC, tpnMC){
  
  # print("analyteMC")
  # print(analyteMC)
  # print("bwMC")
  # print(bwMC)
  # print("gaMC")
  # print(gaMC)
  # print("ethMCSel")
  # print(ethMCSel)
  # print("ethMC")
  # print(ethMC)
  # print("aabcMC")
  # print(aabcMC)
  # print("sexMC")
  # print(sexMC)
  # print("tpnMC")
  # print(tpnMC)
  
  if(is.null(ethMC)){
    ethMC == "1"
  }
  
  if(length(analyteMC) == 0){
    meta <- NULL
    metaName <- NULL
    showModal(
      modalDialog(
        title = "Warning!",
        "At least one analyte should be selected",
        easyClose = TRUE
      )
    )
    
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select at least one analyte from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
    
  }  else if(length(analyteMC) == 1){
    meta <- meta_data[, as.integer(analyteMC)]
    metaName <- analytes_all[as.integer(analyteMC)]
  } else {
    meta <- meta_data[, as.integer(analyteMC)]
    metaName <- paste(analytes_all[sort(as.integer(analyteMC))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwMC) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwMC)]
  }
  
  if(length(gaMC) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaMC)]
  }
  
  if(ethMCSel == "1"){
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
    }
  } else {
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethMC)]
    }
  }
  
  if(length(aabcMC) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcMC)]
  }
  
  
  if(length(sexMC) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexMC)]
  }
  
  
  if(length(tpnMC) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnMC)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & idxTPN
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  if(length(analyteMC) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = c(x[idxSel], x[idxCommon]),
    group = c(
      rep(paste0("Selected (n = ", sum(idxSel), ")"), sum(idxSel)), 
      rep(paste0("Common (n = ", sum(idxCommon), ")"), sum(idxCommon))
    ),
    stringsAsFactors = FALSE
  )
  
  dplot$group <- factor(dplot$group, levels = sort(unique(dplot$group))[c(2,1)])
  
  colnames(dplot) <- c(metaName, "Group")
  
  gp <- ggplot(dplot) + geom_boxplot(aes_string(x="Group", y= metaName)) + 
    labs(x= "", y = metaName) +
    theme_light()
  
  return(gp)
  
}


# create boxplot for ratio between selected group and common group
# input: selected index from ui
plotMCRatio <- function(
  numeratorMC, denominatorMC, bwMC, gaMC, ethMCSel, ethMC, 
  aabcMC, sexMC, tpnMC){
  
  # print("numeratorMC")
  # print(numeratorMC)
  # print("denominatorMC")
  # print(denominatorMC)
  # print("bwMC")
  # print(bwMC)
  # print("gaMC")
  # print(gaMC)
  # print("ethMCSel")
  # print(ethMCSel)
  # print("ethMC")
  # print(ethMC)
  # print("aabcMC")
  # print(aabcMC)
  # print("sexMC")
  # print(sexMC)
  # print("tpnMC")
  # print(tpnMC)
  
  
  if(is.null(ethMC)){
    ethMC == "1"
  }
  
  if(length(numeratorMC) == 0){
    numer <- NULL
    numerName <- NULL
    showModal(
      modalDialog(
        title = "Warning!",
        "At least one analyte should be selected for numerator",
        easyClose = TRUE
      )
    )
    
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select at least one analyte for numerator from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
    
  }  else if(length(numeratorMC) == 1){
    numer <- meta_data[, as.integer(numeratorMC)]
    numerName <- analytes_all[as.integer(numeratorMC)]
  } else {
    numer <- meta_data[, as.integer(numeratorMC)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorMC))], collapse = " + ")
  }
  
  
  if(length(denominatorMC) == 0){
    denom <- NULL
    denomName <- NULL
    showModal(
      modalDialog(
        title = "Warning!",
        "At least one analyte should be selected for denominator",
        easyClose = TRUE
      )
    )
    
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select at least one analyte for denominator from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
    
  }  else if(length(denominatorMC) == 1){
    denom <- meta_data[, as.integer(denominatorMC)]
    denomName <- analytes_all[as.integer(denominatorMC)]
  } else {
    denom <- meta_data[, as.integer(denominatorMC)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorMC))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwMC) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwMC)]
  }
  
  if(length(gaMC) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaMC)]
  }
  
  if(ethMCSel == "1"){
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
    }
  } else {
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethMC)]
    }
  }
  
  if(length(aabcMC) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcMC)]
  }
  
  
  if(length(sexMC) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexMC)]
  }
  
  
  if(length(tpnMC) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnMC)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & idxTPN
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  if(length(numeratorMC) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorMC) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    ratio = c(x[idxSel], x[idxCommon])/c(y[idxSel], y[idxCommon]),
    group = c(
      rep(paste0("Selected (n = ", sum(idxSel), ")"), sum(idxSel)), 
      rep(paste0("Common (n = ", sum(idxCommon), ")"), sum(idxCommon))
    ),
    stringsAsFactors = FALSE
  )
  
  dplot$group <- factor(dplot$group, levels = sort(unique(dplot$group))[c(2,1)])
  
  colnames(dplot) <- c("Ratio", "Group")
  
  gp <- ggplot(dplot) + geom_boxplot(aes(x=Group, y= Ratio)) + 
    labs(x= "", y = ratioName) +
    theme_light()
  
  return(gp)
}




# information of comparing analyte(s) between selected group and common group
# input: selected index from ui
getMCInfoAnalytes <- function(
  analyteMC, bwMC, gaMC, ethMCSel, ethMC, 
  aabcMC, sexMC, tpnMC){
  if(is.null(ethMC)){
    ethMC == "1"
  }
  
  if(length(analyteMC) == 0){
    meta <- NULL
    metaName <- NULL
    
    return(HTML("At least one analyte should be selected"))
    
  }  else if(length(analyteMC) == 1){
    meta <- meta_data[, as.integer(analyteMC)]
    metaName <- analytes_all[as.integer(analyteMC)]
  } else {
    meta <- meta_data[, as.integer(analyteMC)]
    metaName <- paste(analytes_all[sort(as.integer(analyteMC))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  bwInfo <- NULL
  gaInfo <- NULL
  ethInfo <- NULL
  aabcInfo <- NULL
  sexInfo <- NULL
  tpnInfo <- NULL
  
  if(length(bwMC) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwMC)]
    bwInfo <- paste(bw_group[as.integer(bwMC)], collapse = ";")
  } else {
    bwInfo <- "All"
  }
  
  if(length(gaMC) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaMC)]
    gaInfo <- paste(ga_group[as.integer(gaMC)], collapse = ";")
  } else {
    gaInfo <- All
  }
  
  if(ethMCSel == "1"){
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
      ethInfo <- paste(ethnicity_group[as.integer(ethMC)], collapse = ";")
    } else {
      ethInfo <- "All"
    }
  } else {
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethMC)]
      ethInfo <- paste(ethnicity_group_details[as.integer(ethMC)], collapse = ";")
    } else {
      ethInfo <- "All"
    }
  }
  
  if(length(aabcMC) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcMC)]
    aabcInfo <- paste(aabc_group[as.integer(aabcMC)], collapse = ";")
  } else {
    aabcInfo <- "All"
  }
  
  
  if(length(sexMC) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexMC)]
    sexInfo <- sex_group[as.integer(sexMC)]
  } else {
    sexInfo <- "All"
  }
  
  
  if(length(tpnMC) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnMC)]
    tpnInfo <- tpn_group[as.integer(tpnMC)]
  } else {
    tpnInfo <- "All"
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & idxTPN
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  if(length(analyteMC) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  rltCD <- cohen.d(x[idxSel], x[idxCommon])
  rltTT <- t.test(x[idxSel], x[idxCommon])
  sSel <- summary(x[idxSel])
  sCommon <- summary(x[idxCommon])
  
  selInfo <- ""
  
  if(!is.null(bwInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Birth Weight: ", bwInfo)
  }
  
  if(!is.null(gaInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Gestational age: ", gaInfo)
  }
  
  if(!is.null(ethInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Ethnicity groups: ", ethInfo)
  }
  
  
  if(!is.null(aabcInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Age at Blood Collection: ", aabcInfo)
  }
  
  
  if(!is.null(sexInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Sex: ", sexInfo)
  }
  
  
  if(!is.null(tpnInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Total parenteral nutrition (TPN): ", tpnInfo)
  }
  
  commonInfo <- "Newborns with birth weight between 2500-4000g, gestational age between 37-41 weeks, age at blood collection between 24-48 hours, and without TPN"
  
  
  
  return(HTML(
    paste(
      paste0("Mean of selected group: ", format(round(sSel[4], 2), nsmall = 2)),
      paste0("Mean of common group: ", format(round(sCommon[4], 2), nsmall = 2)),
      
      paste0("Median of selected group: ", format(round(sSel[3], 2), nsmall = 2)),
      paste0("Median of common group: ", format(round(sCommon[3], 2), nsmall = 2)),
      
      paste0("Cohen's d between two groups: ", format(round(rltCD$estimate, 2), nsmall = 2)),
      
      "<br/>",
      
      paste0("Selected group: "),
      
      selInfo,
      
      "<br/>",
      
      paste0("Common group: "),
      
      commonInfo,
      
      sep = "<br/>"
    )
  ))
}
  

getMCInfoRatio <- function(
  numeratorMC, denominatorMC, bwMC, gaMC, ethMCSel, ethMC, 
  aabcMC, sexMC, tpnMC){
  
  if(is.null(ethMC)){
    ethMC == "1"
  }
  
  if(length(numeratorMC) == 0){
    numer <- NULL
    numerName <- NULL
    
    return(HTML("Please select at least one analyte for numerator from the left panel"))
    
  }  else if(length(numeratorMC) == 1){
    numer <- meta_data[, as.integer(numeratorMC)]
    numerName <- analytes_all[as.integer(numeratorMC)]
  } else {
    numer <- meta_data[, as.integer(numeratorMC)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorMC))], collapse = " + ")
  }
  
  
  if(length(denominatorMC) == 0){
    denom <- NULL
    denomName <- NULL
    
    return(HTML("Please select at least one analyte for denominator from the left panel"))
    
  }  else if(length(denominatorMC) == 1){
    denom <- meta_data[, as.integer(denominatorMC)]
    denomName <- analytes_all[as.integer(denominatorMC)]
  } else {
    denom <- meta_data[, as.integer(denominatorMC)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorMC))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  bwInfo <- NULL
  gaInfo <- NULL
  ethInfo <- NULL
  aabcInfo <- NULL
  sexInfo <- NULL
  tpnInfo <- NULL
  
  if(length(bwMC) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwMC)]
    bwInfo <- paste(bw_group[as.integer(bwMC)], collapse = ";")
  } else {
    bwInfo <- "All"
  }
  
  if(length(gaMC) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaMC)]
    gaInfo <- paste(ga_group[as.integer(gaMC)], collapse = ";")
  } else {
    gaInfo <- All
  }
  
  if(ethMCSel == "1"){
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
      ethInfo <- paste(ethnicity_group[as.integer(ethMC)], collapse = ";")
    } else {
      ethInfo <- "All"
    }
  } else {
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethMC)]
      ethInfo <- paste(ethnicity_group_details[as.integer(ethMC)], collapse = ";")
    } else {
      ethInfo <- "All"
    }
  }
  
  if(length(aabcMC) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcMC)]
    aabcInfo <- paste(aabc_group[as.integer(aabcMC)], collapse = ";")
  } else {
    aabcInfo <- "All"
  }
  
  
  if(length(sexMC) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexMC)]
    sexInfo <- sex_group[as.integer(sexMC)]
  } else {
    sexInfo <- "All"
  }
  
  
  if(length(tpnMC) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnMC)]
    tpnInfo <- tpn_group[as.integer(tpnMC)]
  } else {
    tpnInfo <- "All"
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & idxTPN
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  if(length(numeratorMC) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorMC) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  ratio <- x/y
  
  rltCD <- cohen.d(ratio[idxSel], ratio[idxCommon])
  rltTT <- t.test(ratio[idxSel], ratio[idxCommon])
  sSel <- summary(ratio[idxSel])
  sCommon <- summary(ratio[idxCommon])
  
  selInfo <- ""
  
  if(!is.null(bwInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Birth Weight: ", bwInfo)
  }
  
  if(!is.null(gaInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Gestational age: ", gaInfo)
  }
  
  if(!is.null(ethInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Ethnicity groups: ", ethInfo)
  }
  
  
  if(!is.null(aabcInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Age at Blood Collection: ", aabcInfo)
  }
  
  
  if(!is.null(sexInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Sex: ", sexInfo)
  }
  
  
  if(!is.null(tpnInfo)){
    if(selInfo!=""){
      selInfo <- paste0(selInfo, "<br/>")
    }
    selInfo <- paste0(selInfo, "Total parenteral nutrition (TPN): ", tpnInfo)
  }
  
  
  commonInfo <- "Newborns with birth weight between 2500-4000g, gestational age between 37-41 weeks, age at blood collection between 24-48 hours, and without TPN"
  
  
  return(HTML(
    paste(
      paste0("Mean of selected group: ", format(round(sSel[4], 2), nsmall = 2)),
      paste0("Mean of common group: ", format(round(sCommon[4], 2), nsmall = 2)),
      
      paste0("Median of selected group: ", format(round(sSel[3], 2), nsmall = 2)),
      paste0("Median of common group: ", format(round(sCommon[3], 2), nsmall = 2)),
      
      paste0("Cohen's d between two groups: ", format(round(rltCD$estimate, 2), nsmall = 2)),
      
      "<br/>",
      
      paste0("Selected group: "),
      
      selInfo,
      
      "<br/>",
      
      paste0("Common group: "),
      
      commonInfo,
      
      sep = "<br/>"
    )
  ))
}
  
  
  