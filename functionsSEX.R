
library(ggplot2)
library(dplyr)
library(DT)

############################################################################################
############################################################################################

plotBoxplotSexAnalytes <- function(
  analyteSex, bwSex, gaSex, ethSexSel, ethSex, 
  aabcSex,tpnSex,compareSex){
  
  if(is.null(ethSex)){
    ethSex = c("1", "2", "3", "4")
  }
  
  
  if(length(analyteSex) == 0){
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
    
  }  else if(length(analyteSex) == 1){
    meta <- meta_data[, as.integer(analyteSex)]
    metaName <- analytes_all[as.integer(analyteSex)]
  } else {
    meta <- meta_data[, as.integer(analyteSex)]
    metaName <- paste(analytes_all[sort(as.integer(analyteSex))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwSex) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwSex)]
  }
  
  if(length(gaSex) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaSex)]
  }
  
  if(ethSexSel == "1"){
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethSex)]
    }
  } else {
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethSex)]
    }
  }
  
  
  if(length(aabcSex) >0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcSex)]
  }
  
  
  if(length(tpnSex) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnSex)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxTPN & flag_sex[rep(TRUE, nrow(meta_data))]!='NA'
  
  
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyteSex) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = x[idxSel],
    sex = flag_sex[idxSel],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  if(compareSex=="1"){
    xTicks <- NULL
    for(a in sex_group){
      num <- sum(dplot$sex == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
  
    gp <- ggplot(dplot) + geom_boxplot(aes(x=sex, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "Female"]), color = "#E18727FF") + 
      labs(x="Sex", y = bquote(.(metaName)(mu*mol/L))) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareSex=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareSex=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareSex=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareSex=="5"&ethSexSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareSex=="5"&ethSexSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareSex=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    xTicks <- NULL
    for(a in sex_group){
      num <- sum(dplot$sex == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=sex, y = x, fill = group)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
      labs(x="Sex", y =  bquote(.(metaName)(mu*mol/L)), fill = label) + 
      scale_x_discrete(labels = xTicks) + 
      ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
    #ggplotly(gp) %>% plotly::layout(boxmode = "group")
  }
  
}
############################################################################################
############################################################################################
plotBoxplotSexRatio <- function(
  numeratorSex, denominatorSex, bwSex, gaSex, ethSexSel, ethSex, 
  aabcSex, tpnSex, compareSex
){
  if(is.null(ethSex)){
    ethSex = c("1", "2", "3", "4")
  }
  
  if(length(numeratorSex) == 0){
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
    
  }  else if(length(numeratorSex) == 1){
    numer <- meta_data[, as.integer(numeratorSex)]
    numerName <- analytes_all[as.integer(numeratorSex)]
  } else {
    numer <- meta_data[, as.integer(numeratorSex)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorSex))], collapse = " + ")
  }
  
  
  if(length(denominatorSex) == 0){
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
    
  }  else if(length(denominatorSex) == 1){
    denom <- meta_data[, as.integer(denominatorSex)]
    denomName <- analytes_all[as.integer(denominatorSex)]
  } else {
    denom <- meta_data[, as.integer(denominatorSex)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorSex))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwSex) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwSex)]
  }
  
  if(length(gaSex) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaSex)]
  }
  
  if(ethSexSel == "1"){
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethSex)]
    }
  } else {
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethSex)]
    }
  }
  
  
  if(length(aabcSex) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcSex)]
  }
  
  
  if(length(tpnSex) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnSex)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxTPN & flag_sex[rep(TRUE, nrow(meta_data))]!='NA'
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  if(length(numeratorSex) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorSex) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    sex = flag_sex[idxSel],
    stringsAsFactors = FALSE
  )
  
  if(compareSex=="1"){
    xTicks <- NULL
    for(a in sex_group){
      num <- sum(dplot$sex == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=sex, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "24-48"]), color = "#E18727FF") + 
      labs(x="Sex", y = ratioName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareSex=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareSex=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareSex=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareSex=="5"&ethSexSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareSex=="5"&ethSexSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareSex=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    xTicks <- NULL
    for(a in sex_group){
      num <- sum(dplot$sex == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=sex, y = x, fill = group)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
      labs(x="Sex", y = ratioName, fill = label) + 
      scale_x_discrete(labels = xTicks) + 
      ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  }
}





createTableSexAnalytes <- function(
  analyteSex, bwSex, gaSex, ethSexSel, ethSex, 
  aabcSex,tpnSex,compareSex){
  
  if(is.null(ethSex)){
    ethSex = c("1", "2", "3", "4")
  }
  
  
  if(length(analyteSex) == 0){
    return(NULL)
  }  else if(length(analyteSex) == 1){
    meta <- meta_data[, as.integer(analyteSex)]
    metaName <- analytes_all[as.integer(analyteSex)]
  } else {
    meta <- meta_data[, as.integer(analyteSex)]
    metaName <- paste(analytes_all[sort(as.integer(analyteSex))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwSex) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwSex)]
  }
  
  if(length(gaSex) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaSex)]
  }
  
  if(ethSexSel == "1"){
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethSex)]
    }
  } else {
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethSex)]
    }
  }
  
  
  if(length(aabcSex) >0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcSex)]
  }
  
  
  if(length(tpnSex) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnSex)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxTPN & (flag_sex!='NA')
  
  
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  if(length(analyteSex) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = x[idxSel],
    sex = flag_sex[idxSel],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  if(compareSex=="1"){
    colnames(dplot) <- c("x", "Sex")
    tb <- dplot %>%
      group_by(Sex) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(2, 2)
  } else {
    if(compareSex=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareSex=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareSex=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareSex=="5"&ethSexSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareSex=="5"&ethSexSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareSex=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    colnames(dplot) <- c("x", "Sex", label)
    tb <- dplot %>%
      group_by_at(c("Sex", label)) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(3, 2)
    
  }
  
}





createTableSexRatio <- function(
  numeratorSex, denominatorSex, bwSex, gaSex, ethSexSel, ethSex, 
  aabcSex, tpnSex, compareSex
){
  if(is.null(ethSex)){
    ethSex = c("1", "2", "3", "4")
  }
  
  if(length(numeratorSex) == 0){
    return(NULL)
    
  }  else if(length(numeratorSex) == 1){
    numer <- meta_data[, as.integer(numeratorSex)]
    numerName <- analytes_all[as.integer(numeratorSex)]
  } else {
    numer <- meta_data[, as.integer(numeratorSex)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorSex))], collapse = " + ")
  }
  
  
  if(length(denominatorSex) == 0){
    return(NULL)
  }  else if(length(denominatorSex) == 1){
    denom <- meta_data[, as.integer(denominatorSex)]
    denomName <- analytes_all[as.integer(denominatorSex)]
  } else {
    denom <- meta_data[, as.integer(denominatorSex)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorSex))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(length(bwSex) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwSex)]
  }
  
  if(length(gaSex) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaSex)]
  }
  
  if(ethSexSel == "1"){
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethSex)]
    }
  } else {
    if(length(ethSex) > 0 && length(ethSex) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethSex)]
    }
  }
  
  
  if(length(aabcSex) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcSex)]
  }
  
  
  if(length(tpnSex) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnSex)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxTPN & flag_sex[rep(TRUE, nrow(meta_data))]!='NA'
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  if(length(numeratorSex) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorSex) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    sex = flag_sex[idxSel],
    stringsAsFactors = FALSE
  )
  
  if(compareSex=="1"){
    colnames(dplot) <- c("x", "Sex")
    tb <- dplot %>%
      group_by(Sex) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(2:3, 3)
  } else {
    if(compareSex=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareSex=="3"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareSex=="4"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareSex=="5"&ethSexSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareSex=="5"&ethSexSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareSex=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else {
      print("ERROR")
    }
    
    colnames(dplot) <- c("x", "Sex", label)
    tb <- dplot %>%
      group_by_at(c("Sex", label)) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(3:4, 3)
  }
}


