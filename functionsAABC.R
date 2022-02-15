library(ggplot2)

## functions for AABC panel

# boxplot of metabolits between aabc groups
plotBoxplotAabcAnalytes <- function(
  analyteAabc, bwAabc, gaAabc, ethAabcSel, ethAabc, 
  sexAabc, tpnAabc, compareAabc){
  
  if(is.null(ethAabc)){
    ethAabc == "1"
  }
  
  
  if(length(analyteAabc) == 0){
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
    
  }  else if(length(analyteAabc) == 1){
    meta <- meta_data[, as.integer(analyteAabc)]
    metaName <- analytes_all[as.integer(analyteAabc)]
  } else {
    meta <- meta_data[, as.integer(analyteAabc)]
    metaName <- paste(analytes_all[sort(as.integer(analyteAabc))], collapse = " + ")
  }
  
  # if no or all items are selected, idx__ would be all TRUE
  # for example, there are three categories in sex: Female   Male     NA 
  # if no item or both Female and Male are selected, idxSex would be all TRUE
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwAabc) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwAabc)]
  }
  
  if(length(gaAabc) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaAabc)]
  }
  
  if(ethAabcSel == "1"){
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethAabc)]
    }
  } else {
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethAabc)]
    }
  }
  
  
  if(length(sexAabc) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexAabc)]
  }
  
  
  if(length(tpnAabc) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnAabc)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxSex & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyteAabc) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = x[idxSel],
    aabc = flag_aabc[idxSel],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  
  
  if(compareAabc=="1"){ # no comparison
    xTicks <- NULL
    for(a in aabc_group){
      num <- sum(dplot$aabc == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=aabc, y = x)) + 
      geom_hline(yintercept = median(dplot$x[dplot$aabc == "24-48"]), color = "#E18727FF") + 
      labs(x="Age at Blood Collection (Hour)", y = metaName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareAabc=="2"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else if(compareAabc=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareAabc=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareAabc=="5"){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                  "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareAabc=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    xTicks <- NULL
    for(a in aabc_group){
      num <- sum(dplot$aabc == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=aabc, y = x, fill = group)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
      labs(x="Age at Blood Collection (Hour)", y = metaName, fill = label) + 
      scale_x_discrete(labels = xTicks) + 
      ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
    #ggplotly(gp) %>% plotly::layout(boxmode = "group")
  }
  
}

# boxplot of ratio between aabc groups
plotBoxplotAabcRatio <- function(
  numeratorAabc, denominatorAabc, bwAabc, gaAabc, ethAabcSel, ethAabc, 
  sexAabc, tpnAabc, compareAabc
){
  if(is.null(ethAabc)){
    ethAabc == "1"
  }
  
  if(length(numeratorAabc) == 0){
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
    
  }  else if(length(numeratorAabc) == 1){
    numer <- meta_data[, as.integer(numeratorAabc)]
    numerName <- analytes_all[as.integer(numeratorAabc)]
  } else {
    numer <- meta_data[, as.integer(numeratorAabc)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorAabc))], collapse = " + ")
  }
  
  
  if(length(denominatorAabc) == 0){
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
    
  }  else if(length(denominatorAabc) == 1){
    denom <- meta_data[, as.integer(denominatorAabc)]
    denomName <- analytes_all[as.integer(denominatorAabc)]
  } else {
    denom <- meta_data[, as.integer(denominatorAabc)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorAabc))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwAabc) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwAabc)]
  }
  
  if(length(gaAabc) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaAabc)]
  }
  
  if(ethAabcSel == "1"){
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethAabc)]
    }
  } else {
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethAabc)]
    }
  }
  
  
  if(length(sexAabc) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexAabc)]
  }
  
  
  if(length(tpnAabc) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnAabc)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxSex & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  if(length(numeratorAabc) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorAabc) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    aabc = flag_aabc[idxSel],
    stringsAsFactors = FALSE
  )
  
  if(compareAabc=="1"){ # no comparison
    xTicks <- NULL
    for(a in aabc_group){
      num <- sum(dplot$aabc == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=aabc, y = x)) + 
      geom_hline(yintercept = median(dplot$x[dplot$aabc == "24-48"]), color = "#E18727FF") + 
      labs(x="Age at Blood Collection (Hour)", y = ratioName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareAabc=="2"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else if(compareAabc=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareAabc=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareAabc=="5"){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareAabc=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    xTicks <- NULL
    for(a in aabc_group){
      num <- sum(dplot$aabc == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=aabc, y = x, fill = group)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
      labs(x="Age at Blood Collection (Hour)", y = ratioName, fill = label) + 
      scale_x_discrete(labels = xTicks) + 
      ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  }
}



# trend of metabolic changes over aabc
plotTrendplotAabcAnalytes <- function(
  analyteAabc, bwAabc, gaAabc, ethAabcSel, ethAabc, 
  sexAabc, tpnAabc, compareAabc
){
  if(is.null(ethAabc)){
    ethAabc == "1"
  }
  
  
  if(length(analyteAabc) == 0){
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
    
  }  else if(length(analyteAabc) == 1){
    meta <- meta_data[, as.integer(analyteAabc)]
    metaName <- analytes_all[as.integer(analyteAabc)]
  } else {
    meta <- meta_data[, as.integer(analyteAabc)]
    metaName <- paste(analytes_all[sort(as.integer(analyteAabc))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwAabc) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwAabc)]
  }
  
  if(length(gaAabc) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaAabc)]
  }
  
  if(ethAabcSel == "1"){
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethAabc)]
    }
  } else {
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethAabc)]
    }
  }
  
  
  if(length(sexAabc) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexAabc)]
  }
  
  
  if(length(tpnAabc) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnAabc)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxSex & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyteAabc) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = x[idxSel],
    aabc = aabc[idxSel]
  )
  
  #colnames(dplot) <- c(metaName, "aabc")
  
  #print(summary(dplot))
  
  if(compareAabc == "1"){ # no comparison
    gp <- ggplot(dplot) + geom_smooth(aes(x=aabc, y = x), method = "gam", formula = y ~ s(x, bs = "cs")) + 
      labs(x = paste0("Age at Blood Collection (Hour)\n(n=", nrow(dplot), ")"), y = metaName) + 
      scale_x_continuous(breaks = seq(0, 168, 24), minor_breaks = NULL) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareAabc=="2"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else if(compareAabc=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareAabc=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareAabc=="5"){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareAabc=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    gp <- ggplot(dplot) + 
      geom_smooth(aes(x=aabc, y = x, color = group, fill = group), method = "gam", formula = y ~ s(x, bs = "cs")) + 
      labs(x = paste0("Age at Blood Collection (Hour)\n(n=", nrow(dplot), ")"), y = metaName) + 
      scale_x_continuous(breaks = seq(0, 168, 24), minor_breaks = NULL) + 
      labs(color = label, fill = label) + ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  }
}




# trend of ratio changes over aabc
plotTrendplotAabcRatio <- function(
  numeratorAabc, denominatorAabc, bwAabc, gaAabc, ethAabcSel, ethAabc, 
  sexAabc, tpnAabc, compareAabc
){
  if(is.null(ethAabc)){
    ethAabc == "1"
  }
  
  if(length(numeratorAabc) == 0){
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
    
  }  else if(length(numeratorAabc) == 1){
    numer <- meta_data[, as.integer(numeratorAabc)]
    numerName <- analytes_all[as.integer(numeratorAabc)]
  } else {
    numer <- meta_data[, as.integer(numeratorAabc)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorAabc))], collapse = " + ")
  }
  
  
  if(length(denominatorAabc) == 0){
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
    
  }  else if(length(denominatorAabc) == 1){
    denom <- meta_data[, as.integer(denominatorAabc)]
    denomName <- analytes_all[as.integer(denominatorAabc)]
  } else {
    denom <- meta_data[, as.integer(denominatorAabc)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorAabc))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwAabc) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwAabc)]
  }
  
  if(length(gaAabc) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaAabc)]
  }
  
  if(ethAabcSel == "1"){
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethAabc)]
    }
  } else {
    if(length(ethAabc) > 0 && length(ethAabc) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethAabc)]
    }
  }
  
  
  if(length(sexAabc) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexAabc)]
  }
  
  
  if(length(tpnAabc) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnAabc)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxSex & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  if(length(numeratorAabc) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorAabc) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    aabc = aabc[idxSel]
  )
  
  #colnames(dplot) <- c(denomName, "aabc")
  
  if(compareAabc == "1"){ # no comparison
    gp <- ggplot(dplot) + geom_smooth(aes(x=aabc, y = x), method = "gam", formula = y ~ s(x, bs = "cs")) + 
      labs(x = paste0("Age at Blood Collection (Hour)\n(n=", nrow(dplot), ")"), y = denomName) + 
      scale_x_continuous(breaks = seq(0, 168, 24), minor_breaks = NULL) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareAabc=="2"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else if(compareAabc=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareAabc=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareAabc=="5"){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareAabc=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    gp <- ggplot(dplot) + 
      geom_smooth(aes(x=aabc, y = x, color = group, fill = group), method = "gam", formula = y ~ s(x, bs = "cs")) + 
      labs(x = paste0("Age at Blood Collection (Hour)\n(n=", nrow(dplot), ")"), y = denomName) + 
      scale_x_continuous(breaks = seq(0, 168, 24), minor_breaks = NULL) + 
      labs(color = label, fill = label) + ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  }
}


