# functions for multiple comparison

library(ggplot2)
library(ggpubr)
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
    ethMC = "2"
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
    meta <- meta_data[, sort(as.integer(analyteMC))]
    metaName <- analytes_all[sort(as.integer(analyteMC))]
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
    if(length(ethMC) > 0 && length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
    }
  } else {
    if(length(ethMC) > 0 && length(ethMC) < length(ethnicity_group_details)){
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
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  gps <- list()
  for(i in 1:length(metaName)){
    if(length(metaName) == 1){
      x = meta
    } else {
      x <- meta[,i]
    }
    
    
    dplot <- data.frame(
      x = c(x[idxSel], x[idxCommon]),
      group = c(
        rep("Selected", sum(idxSel)), 
        rep("Common", sum(idxCommon))
      ),
      stringsAsFactors = FALSE
    )
    
    dplot$group <- factor(dplot$group, levels = sort(unique(dplot$group))[c(2,1)])
    
    colnames(dplot) <- c("meta", "Group")
    
    #print(metaName)
    gp <- ggplot(dplot) + geom_boxplot(aes(x=Group, y= meta)) + 
      labs(x= "", y = bquote(.(metaName[i])(mu*mol/L))) +
      theme_light()
    
    gps[[i]] <- gp
  }
  
  nc <- min(length(metaName), 3)
  gp <- ggarrange(plotlist = gps, ncol = nc, nrow = ceiling(length(metaName)/nc))
  # if(length(analyteMC) == 1){
  #   x <- meta
  # } else {
  #   x <- rowSums(meta)
  # }
  # 
  # dplot <- data.frame(
  #   x = c(x[idxSel], x[idxCommon]),
  #   group = c(
  #     rep(paste0("Selected (n = ", sum(idxSel), ")"), sum(idxSel)), 
  #     rep(paste0("Common (n = ", sum(idxCommon), ")"), sum(idxCommon))
  #   ),
  #   stringsAsFactors = FALSE
  # )
  # 
  # dplot$group <- factor(dplot$group, levels = sort(unique(dplot$group))[c(2,1)])
  # 
  # colnames(dplot) <- c("meta", "Group")
  # 
  # #print(metaName)
  # gp <- ggplot(dplot) + geom_boxplot(aes(x=Group, y= meta)) + 
  #   labs(x= "", y = metaName) +
  #   theme_light()
  
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
    ethMC = "3"
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
    if(length(ethMC) > 0 && length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
    }
  } else {
    if(length(ethMC) > 0 && length(ethMC) < length(ethnicity_group_details)){
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
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
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
    ethMC = "2"
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
  
  if(sum(idxSel) == 0){
    return(HTML("No newborn in the selected group"))
  }
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  # if(length(analyteMC) == 1){
  #   x <- meta
  # } else {
  #   x <- rowSums(meta)
  # }
  # 
  # rltCD <- cohen.d(x[idxSel], x[idxCommon])
  # rltTT <- t.test(x[idxSel], x[idxCommon])
  # sSel <- summary(x[idxSel])
  # sCommon <- summary(x[idxCommon])
  
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
  
  commonInfo <- "Birth weight: 2500-4000g <br/> Gestational age: 37-41 weeks <br/> Age at blood collection: 24-48 hours <br/>  TPN: NoTPN"
  
  
  return(HTML(
    paste(
      #paste0("Mean of the selected group: ", format(round(sSel[4], 2), nsmall = 2)),
      #paste0("Mean of the common group: ", format(round(sCommon[4], 2), nsmall = 2)),
      
      #paste0("Median of the selected group: ", format(round(sSel[3], 2), nsmall = 2)),
      #paste0("Median of the common group: ", format(round(sCommon[3], 2), nsmall = 2)),
      
      #paste0("Cohen's d between two groups: ", format(round(rltCD$estimate, 2), nsmall = 2), 
      #       "(", format(round(rltCD$conf.int[1], 2), nsmall = 2), ",", 
      #       format(round(rltCD$conf.int[2], 2), nsmall = 2), ")"),
      
      #"<br/>",
      
      paste0("Selected group (n=", sum(idxSel), "): "),
      
      selInfo,
      
      "<br/>",
      
      paste0("Common group (n=", sum(idxCommon), "): "),
      
      commonInfo,
      
      sep = "<br/>"
    )
  ))
}
  

# information of comparing ratio between selected group and common group
# input: selected index from ui
getMCInfoRatio <- function(
  numeratorMC, denominatorMC, bwMC, gaMC, ethMCSel, ethMC, 
  aabcMC, sexMC, tpnMC){
  
  if(is.null(ethMC)){
    ethMC = "3"
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
  
  if(sum(idxSel) == 0){
    return(HTML("No newborn in the selected group"))
  }
  
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
    selInfo <- paste0(selInfo, "TPN: ", tpnInfo)
  }
  
  
  commonInfo <- "Birth weight: 2500-4000g <br/> Gestational age: 37-41 weeks <br/> Age at blood collection: 24-48 hours <br/>  TPN: NoTPN"
  
  
  return(HTML(
    paste(
      # paste0("Mean of the selected group: ", formatRatio(sSel[4])),
      # paste0("Mean of the common group: ", formatRatio(sCommon[4])),
      # 
      # paste0("Median of the selected group: ", formatRatio(sSel[3])),
      # paste0("Median of the common group: ", formatRatio(sCommon[3])),
      # 
      # paste0("Cohen's d between two groups: ", formatRatio(rltCD$estimate), 
      #        "(", formatRatio(rltCD$conf.int[1]), ",", 
      #        formatRatio(rltCD$conf.int[2]), ")"),
      # 
      # "<br/>",
      
      paste0("Selected group: (n=", sum(idxSel), ")"),
      
      selInfo,
      
      "<br/>",
      
      paste0("Common group: (n=", sum(idxCommon), ")"),
      
      commonInfo,
      
      sep = "<br/>"
    )
  ))
}
  

createTableMCAnalytes <- function(
    analyteMC, bwMC, gaMC, ethMCSel, ethMC, 
    aabcMC, sexMC, tpnMC){
  if(is.null(ethMC)){
    ethMC = "2"
  }
  
  if(length(analyteMC) == 0){
    meta <- NULL
    metaName <- NULL
    
    return(NULL)
    
  }  else if(length(analyteMC) == 1){
    meta <- meta_data[, as.integer(analyteMC)]
    metaName <- analytes_all[as.integer(analyteMC)]
  } else {
    meta <- meta_data[, sort(as.integer(analyteMC))]
    metaName <- analytes_all[sort(as.integer(analyteMC))]
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  # bwInfo <- NULL
  # gaInfo <- NULL
  # ethInfo <- NULL
  # aabcInfo <- NULL
  # sexInfo <- NULL
  # tpnInfo <- NULL
  
  if(length(bwMC) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwMC)]
    #bwInfo <- paste(bw_group[as.integer(bwMC)], collapse = ";")
  } else {
    #bwInfo <- "All"
  }
  
  if(length(gaMC) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaMC)]
    #gaInfo <- paste(ga_group[as.integer(gaMC)], collapse = ";")
  } else {
    #gaInfo <- All
  }
  
  if(ethMCSel == "1"){
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
      #ethInfo <- paste(ethnicity_group[as.integer(ethMC)], collapse = ";")
    } else {
      #ethInfo <- "All"
    }
  } else {
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethMC)]
      #ethInfo <- paste(ethnicity_group_details[as.integer(ethMC)], collapse = ";")
    } else {
      #ethInfo <- "All"
    }
  }
  
  if(length(aabcMC) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcMC)]
    #aabcInfo <- paste(aabc_group[as.integer(aabcMC)], collapse = ";")
  } else {
    #aabcInfo <- "All"
  }
  
  
  if(length(sexMC) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexMC)]
    #sexInfo <- sex_group[as.integer(sexMC)]
  } else {
    #sexInfo <- "All"
  }
  
  
  if(length(tpnMC) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnMC)]
    #tpnInfo <- tpn_group[as.integer(tpnMC)]
  } else {
    #tpnInfo <- "All"
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & idxTPN
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  idxCommon <- idx_include & 
    flag_bw %in% c("2500-3000", "3001-3500", "3501-4000") &
    flag_ga %in% c("37-38", "39-40", "41") & 
    flag_aabc %in% c("24-48") &
    flag_tpn %in% c("NoTPN")
  
  
  meanSel <- NULL
  meanCommon <- NULL
  medianSel <- NULL
  medianCommon <- NULL
  cd <- NULL
  for(i in 1:length(analyteMC)){
    if(length(analyteMC)==1){
      x <- meta
    } else {
      x <- meta[,i]
    }
    
    rltCD <- cohen.d(x[idxSel], x[idxCommon])
    cd <- c(cd, rltCD$estimate)
    meanSel <- c(meanSel, mean(x[idxSel]))
    meanCommon <- c(meanCommon, mean(x[idxCommon]))
    
    medianSel <- c(medianSel, median(x[idxSel]))
    medianCommon <- c(medianCommon, median(x[idxCommon]))
  }
  
  rlt <- data.frame(
    meanSel,
    meanCommon,
    medianSel,
    medianCommon,
    cd
  )
  
  #colnames(rlt) <- c("Selected", "Common", "Selected", "Common", "Cohen's d")
  rownames(rlt) <- metaName
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(class = 'dt-center', rowspan = 2, " "),
        th(class = 'dt-center', colspan = 2, "Mean"),
        th(class = 'dt-center', colspan = 2, "Median"),
        th(class = 'dt-center', rowspan = 2, "Cohen's d"),
      ),
      tr(
        lapply(rep(c('Selected', 'Common'), 2), th)
      )
    )
  ))
  
  datatable(rlt,  
            style = "bootstrap4",
            container = sketch,
            options = list(
              columnDefs = list(list(className = 'dt-head-center', targets = 0:5))
            ),
            rownames = TRUE) %>% 
    formatRound(1:5, 2)
}





createTableMCRatio <- function(
    numeratorMC, denominatorMC, bwMC, gaMC, ethMCSel, ethMC, 
    aabcMC, sexMC, tpnMC){
  
  if(is.null(ethMC)){
    ethMC = "3"
  }
  
  if(length(numeratorMC) == 0){
    numer <- NULL
    numerName <- NULL
    
    return(NULL)
    
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
  
  # bwInfo <- NULL
  # gaInfo <- NULL
  # ethInfo <- NULL
  # aabcInfo <- NULL
  # sexInfo <- NULL
  # tpnInfo <- NULL
  
  if(length(bwMC) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwMC)]
    #bwInfo <- paste(bw_group[as.integer(bwMC)], collapse = ";")
  } else {
    #bwInfo <- "All"
  }
  
  if(length(gaMC) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaMC)]
    #gaInfo <- paste(ga_group[as.integer(gaMC)], collapse = ";")
  } else {
    #gaInfo <- All
  }
  
  if(ethMCSel == "1"){
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethMC)]
      #ethInfo <- paste(ethnicity_group[as.integer(ethMC)], collapse = ";")
    } else {
      #ethInfo <- "All"
    }
  } else {
    if(length(ethMC) > 0 & length(ethMC) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethMC)]
      #ethInfo <- paste(ethnicity_group_details[as.integer(ethMC)], collapse = ";")
    } else {
      #ethInfo <- "All"
    }
  }
  
  if(length(aabcMC) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcMC)]
    #aabcInfo <- paste(aabc_group[as.integer(aabcMC)], collapse = ";")
  } else {
    #aabcInfo <- "All"
  }
  
  
  if(length(sexMC) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexMC)]
    #sexInfo <- sex_group[as.integer(sexMC)]
  } else {
    #sexInfo <- "All"
  }
  
  
  if(length(tpnMC) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnMC)]
    #tpnInfo <- tpn_group[as.integer(tpnMC)]
  } else {
    #tpnInfo <- "All"
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & idxTPN
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
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
  
  rlt <- data.frame(
    mean(ratio[idxSel]),
    mean(ratio[idxCommon]),
    median(ratio[idxSel]),
    median(ratio[idxCommon]),
    cd = cohen.d(ratio[idxSel], ratio[idxCommon])$estimate
  )
  
  
  rownames(rlt) <- ratioName
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(class = 'dt-center', rowspan = 2, " "),
        th(class = 'dt-center', colspan = 2, "Mean"),
        th(class = 'dt-center', colspan = 2, "Median"),
        th(class = 'dt-center', rowspan = 2, "Cohen's d"),
      ),
      tr(
        lapply(rep(c('Selected', 'Common'), 2), th)
      )
    )
  ))
  
  
  datatable(rlt,  
            style = "bootstrap4",
            container = sketch,
            options = list(
              columnDefs = list(list(className = 'dt-head-center', targets = 0:5))
            ),
            rownames = TRUE) %>% 
    formatRound(1:5, 3)
  

  
}
  
  