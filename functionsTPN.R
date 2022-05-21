
library(ggplot2)







############################################################################################
############################################################################################

plotBoxplotTPNAnalytes <- function(
  analyteTPN, bwTPN, gaTPN, ethTPNSel, ethTPN, 
  aabcTPN,sexTPN,compareTPN){
  
  if(is.null(ethTPN)){
    ethTPN = c("1", "2", "3", "4")
  }
  
  
  if(length(analyteTPN) == 0){
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
    
  }  else if(length(analyteTPN) == 1){
    meta <- meta_data[, as.integer(analyteTPN)]
    metaName <- analytes_all[as.integer(analyteTPN)]
  } else {
    meta <- meta_data[, as.integer(analyteTPN)]
    metaName <- paste(analytes_all[sort(as.integer(analyteTPN))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  
  if(length(bwTPN) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwTPN)]
  }
  
  if(length(gaTPN) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaTPN)]
  }
  
  if(ethTPNSel == "1"){
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethTPN)]
    }
  } else {
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethTPN)]
    }
  }
  
  
  if(length(aabcTPN) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcTPN)]
  }
  
  
  if(length(sexTPN) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexTPN)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & flag_tpn[rep(TRUE, nrow(meta_data))]!='NA'
  
  
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyteTPN) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = x[idxSel],
    tpn = flag_tpn[idxSel],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  if(compareTPN=="1"){
    xTicks <- NULL
    for(a in tpn_group){
      num <- sum(dplot$tpn == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=tpn, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "Female"]), color = "#E18727FF") + 
      labs(x="TPN", y = bquote(.(metaName)(mu*mol/L))) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareTPN=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareTPN=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareTPN=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareTPN=="6"&ethTPNSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareTPN=="6"&ethTPNSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareTPN=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else {
      print("ERROR")
    }
    
    xTicks <- NULL
    for(a in tpn_group){
      num <- sum(dplot$tpn == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=tpn, y = x, fill = group)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
      labs(x="TPN", y = bquote(.(metaName)(mu*mol/L)), fill = label) + 
      scale_x_discrete(labels = xTicks) + 
      ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
    #ggplotly(gp) %>% plotly::layout(boxmode = "group")
  }
  
}
############################################################################################
############################################################################################
plotBoxplotTPNRatio <- function(
  numeratorTPN, denominatorTPN, bwTPN, gaTPN, ethTPNSel, ethTPN, 
  aabcTPN, sexTPN, compareTPN
){
  if(is.null(ethTPN)){
    ethTPN = c("1", "2", "3", "4")
  }
  
  if(length(numeratorTPN) == 0){
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
    
  }  else if(length(numeratorTPN) == 1){
    numer <- meta_data[, as.integer(numeratorTPN)]
    numerName <- analytes_all[as.integer(numeratorTPN)]
  } else {
    numer <- meta_data[, as.integer(numeratorTPN)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorTPN))], collapse = " + ")
  }
  
  
  if(length(denominatorTPN) == 0){
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
    
  }  else if(length(denominatorTPN) == 1){
    denom <- meta_data[, as.integer(denominatorTPN)]
    denomName <- analytes_all[as.integer(denominatorTPN)]
  } else {
    denom <- meta_data[, as.integer(denominatorTPN)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorTPN))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  
  if(length(bwTPN) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwTPN)]
  }
  
  if(length(gaTPN) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaTPN)]
  }
  
  if(ethTPNSel == "1"){
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethTPN)]
    }
  } else {
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethTPN)]
    }
  }
  
  
  if(length(aabcTPN) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcTPN)]
  }
  
  
  if(length(sexTPN) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexTPN)]
  }
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & flag_tpn[rep(TRUE, nrow(meta_data))]!='NA'
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  if(length(numeratorTPN) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorTPN) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    tpn = flag_tpn[idxSel],
    stringsAsFactors = FALSE
  )
  
  if(compareTPN=="1"){
    xTicks <- NULL
    for(a in tpn_group){
      num <- sum(dplot$tpn == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=tpn, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "24-48"]), color = "#E18727FF") + 
      labs(x="TPN", y = ratioName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  } else {
    if(compareTPN=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareTPN=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareTPN=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareTPN=="6"&ethTPNSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareTPN=="6"&ethTPNSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareTPN=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female","NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else {
      print("ERROR")
    }
    
    xTicks <- NULL
    for(a in tpn_group){
      num <- sum(dplot$tpn == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    
    gp <- ggplot(dplot) + geom_boxplot(aes(x=tpn, y = x, fill = group)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
      labs(x="TPN", y = ratioName, fill = label) + 
      scale_x_discrete(labels = xTicks) + 
      ggsci::scale_color_npg() + 
      theme_light() + theme(text = element_text(size = 12))
    
    return(gp)
  }
}





createTableTPNAnalytes <- function(
  analyteTPN, bwTPN, gaTPN, ethTPNSel, ethTPN, 
  aabcTPN,sexTPN,compareTPN){
  
  if(is.null(ethTPN)){
    ethTPN = c("1", "2", "3", "4")
  }
  
  
  if(length(analyteTPN) == 0){
    return(NULL)
  }  else if(length(analyteTPN) == 1){
    meta <- meta_data[, as.integer(analyteTPN)]
    metaName <- analytes_all[as.integer(analyteTPN)]
  } else {
    meta <- meta_data[, as.integer(analyteTPN)]
    metaName <- paste(analytes_all[sort(as.integer(analyteTPN))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  
  if(length(bwTPN) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwTPN)]
  }
  
  if(length(gaTPN) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaTPN)]
  }
  
  if(ethTPNSel == "1"){
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethTPN)]
    }
  } else {
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethTPN)]
    }
  }
  
  
  if(length(aabcTPN) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcTPN)]
  }
  
  
  if(length(sexTPN) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexTPN)]
  }
  
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & flag_tpn[rep(TRUE, nrow(meta_data))]!='NA'
  
  
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  
  if(length(analyteTPN) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    x = x[idxSel],
    tpn = flag_tpn[idxSel],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  if(compareTPN=="1"){
    colnames(dplot) <- c("x", "TPN")
    tb <- dplot %>%
      group_by(TPN) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(2, 2)
  } else {
    if(compareTPN=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareTPN=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareTPN=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareTPN=="6"&ethTPNSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareTPN=="6"&ethTPNSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareTPN=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else {
      print("ERROR")
    }
    
    colnames(dplot) <- c("x", "TPN", label)
    tb <- dplot %>%
      group_by_at(c("TPN", label)) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(3, 2)
  }
}





createTableTPNRatio <- function(
  numeratorTPN, denominatorTPN, bwTPN, gaTPN, ethTPNSel, ethTPN, 
  aabcTPN, sexTPN, compareTPN
){
  if(is.null(ethTPN)){
    ethTPN = c("1", "2", "3", "4")
  }
  
  if(length(numeratorTPN) == 0){
    return(NULL)
  }  else if(length(numeratorTPN) == 1){
    numer <- meta_data[, as.integer(numeratorTPN)]
    numerName <- analytes_all[as.integer(numeratorTPN)]
  } else {
    numer <- meta_data[, as.integer(numeratorTPN)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorTPN))], collapse = " + ")
  }
  
  
  if(length(denominatorTPN) == 0){
    return(NULL)
  }  else if(length(denominatorTPN) == 1){
    denom <- meta_data[, as.integer(denominatorTPN)]
    denomName <- analytes_all[as.integer(denominatorTPN)]
  } else {
    denom <- meta_data[, as.integer(denominatorTPN)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorTPN))], collapse = " + ")
  }
  
  
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxEth <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  
  if(length(bwTPN) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwTPN)]
  }
  
  if(length(gaTPN) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaTPN)]
  }
  
  if(ethTPNSel == "1"){
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethTPN)]
    }
  } else {
    if(length(ethTPN) > 0 && length(ethTPN) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethTPN)]
    }
  }
  
  
  if(length(aabcTPN) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcTPN)]
  }
  
  
  if(length(sexTPN) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexTPN)]
  }
  
  idxSel <- idx_include & idxBW & idxGA & idxEth & idxAabc & idxSex & flag_tpn[rep(TRUE, nrow(meta_data))]!='NA'
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  if(length(numeratorTPN) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorTPN) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    tpn = flag_tpn[idxSel],
    stringsAsFactors = FALSE
  )
  
  if(compareTPN=="1"){
    colnames(dplot) <- c("x", "TPN")
    tb <- dplot %>%
      group_by(TPN) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(2:3, 3)
  } else {
    if(compareTPN=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareTPN=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareTPN=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareTPN=="6"&ethTPNSel=='1'){
      dplot$group <- factor(ethnicity$eth_state[idxSel], levels = c("Asian", "Black", "Hispanic", "White", 
                                                                    "OtherUnknown"))
      dplot <- dplot[dplot$group != "OtherUnknown",]
      label <- "Ethnicity"
    } else if(compareTPN=="6"&ethTPNSel=='2'){
      dplot$group <- factor(ethnicity$eth_detail[idxSel], levels = c(
        "Asian East Indian", "Black", "Cambodian",         
        "Chinese", "Filipino", "Guamanian",    
        "Hawaiian", "Hispanic", "Japanese", 
        "Korean", "Laos", "Middle Eastern", 
        "Native American", "Other Southeast Asian", "Samoan", 
        "Vietnamese", "White"  
      ))
      label <- "Ethnicity"
    } else if(compareTPN=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female","NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else {
      print("ERROR")
    }
    
    colnames(dplot) <- c("x", "TPN", label)
    tb <- dplot %>%
      group_by_at(c("TPN", label)) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(3:4, 3)
  }
}


