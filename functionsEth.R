
library(ggplot2)

plotBoxplotEthAnalytes <- function(
  analyteEth, bwEth, gaEth, ethEthSel, ethEth, sexEth, 
  aabcEth,tpnEth,compareEth){
  
  if(is.null(ethEth)){
    ethEth = c("1", "2", "3", "4")
  }

  if(length(analyteEth) == 0){
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
    
  } else if(length(analyteEth) == 1){
    meta <- meta_data[, as.integer(analyteEth)]
    metaName <- analytes_all[as.integer(analyteEth)]
  } else {
    meta <- meta_data[, as.integer(analyteEth)]
    metaName <- paste(analytes_all[sort(as.integer(analyteEth))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  #idx <- rep(TRUE, nrow(meta_data))
  
  if(length(bwEth) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwEth)]
  }
  
  if(length(gaEth) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaEth)]
  }
  
  if(length(aabcEth) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcEth)]
  }
  
  
  if(length(tpnEth) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnEth)]
  }
  
  if(length(sexEth) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexEth)]
  }
  
  if (ethEthSel=='1'){
    idxEth  <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethEth)]
  }
  else{
    idxEth  <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethEth)]
  }
  
  idxSel <- idx_include & idxBW & idxGA & idxSex & idxAabc & idxTPN & idxEth
  
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyteEth) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  dplot <- data.frame(
    x = x[idxSel],
    eth = ethnicity[idxSel,as.integer(ethEthSel)],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  if(compareEth=="1"&ethEthSel=='1'){
    xTicks <- NULL
    for(a in ethnicity_group){
      num <- sum(dplot$eth == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "Female"]), color = "#E18727FF") + 
      labs(x="Enthnicity", y = metaName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    return(gp)
    }
  if(compareEth=="1"&ethEthSel=='2'){
    xTicks <- NULL
    for(a in ethnicity_group_details){
      num <- sum(dplot$eth == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "Female"]), color = "#E18727FF") + 
      labs(x="Enthnicity", y = metaName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    return(gp)}
  else {
    if(compareEth=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareEth=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareEth=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareEth=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else if(compareEth=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else {
      print("ERROR")
    }
    if (ethEthSel=='1'){
      xTicks <- NULL
      for(a in ethnicity_group){
        num <- sum(dplot$eth == a)
        if(num > 0){
          xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
        }
      }
      gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x, fill = group)) + 
        #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
        labs(x="Ethnicity", y = metaName, fill = label) + 
        scale_x_discrete(labels = xTicks) + 
        ggsci::scale_color_npg() + 
        theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      return(gp)
    }
    if (ethEthSel=='2'){
      xTicks <- NULL
      for(a in ethnicity_group_details){
        num <- sum(dplot$eth == a)
        if(num > 0){
          xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
        }
      }
      gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x, fill = group)) + 
        #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
        labs(x="Ethnicity", y = metaName, fill = label) + 
        scale_x_discrete(labels = xTicks) + 
        ggsci::scale_color_npg() + 
        theme_light() + 
        theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      return(gp)
      
      
    }
    }
}



plotBoxplotEthRatio <- function(
  numeratorEth, denominatorEth, bwEth, gaEth, ethEthSel, ethEth, sexEth, 
  aabcEth, tpnEth, compareEth
){
  if(is.null(ethEth)){
    ethEth = c("1", "2", "3", "4")
  }
  
  if(length(numeratorEth) == 0){
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
    
  }  else if(length(numeratorEth) == 1){
    numer <- meta_data[, as.integer(numeratorEth)]
    numerName <- analytes_all[as.integer(numeratorEth)]
  } else {
    numer <- meta_data[, as.integer(numeratorEth)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorEth))], collapse = " + ")
  }
  
  
  if(length(denominatorEth) == 0){
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
    
  }  else if(length(denominatorEth) == 1){
    denom <- meta_data[, as.integer(denominatorEth)]
    denomName <- analytes_all[as.integer(denominatorEth)]
  } else {
    denom <- meta_data[, as.integer(denominatorEth)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorEth))], collapse = " + ")
  }

  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  #idx <- rep(TRUE, nrow(meta_data))
  
  if(length(bwEth) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwEth)]
  }
  if(length(sexEth) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexEth)]
  }
  
  if(length(gaEth) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaEth)]
  }

  if(length(aabcEth) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcEth)]
  }
  
  
  if(length(tpnEth) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnEth)]
  }
  
  if (ethEthSel=='1'){
    idxEth  <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethEth)]
  }
  else{
    idxEth  <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethEth)]
  }
  
  idxSel <- idx_include & idxBW & idxGA & idxSex & idxAabc & idxTPN & idxEth
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn in the selected group")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  if(length(numeratorEth) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorEth) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    eth = ethnicity[idxSel,as.integer(ethEthSel)],
    stringsAsFactors = FALSE
  )

  if(compareEth=="1"&ethEthSel=='1'){
    xTicks <- NULL
    for(a in ethnicity_group){
      num <- sum(dplot$eth == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "24-48"]), color = "#E18727FF") + 
      labs(x="Ethnicity", y = ratioName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    return(gp)
  } 
  if(compareEth=="1"&ethEthSel=='2'){
    xTicks <- NULL
    for(a in ethnicity_group_details){
      num <- sum(dplot$eth == a)
      if(num > 0){
        xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
      }
    }
    gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x)) + 
      #geom_hline(yintercept = median(dplot$x[dplot$sex == "24-48"]), color = "#E18727FF") + 
      labs(x="Ethnicity", y = ratioName) + 
      scale_x_discrete(labels = xTicks) + 
      theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    return(gp)
  } 
  
  else {
    if(compareEth=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareEth=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareEth=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareEth=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else if(compareEth=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } 
    else {
      print("ERROR")
    }
    if (ethEthSel=='1'){
      xTicks <- NULL
      for(a in ethnicity_group){
        num <- sum(dplot$eth == a)
        if(num > 0){
          xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
        }
      }
      gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x, fill = group)) + 
        #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
        labs(x="Ethnicity", y = ratioName, fill = label) + 
        scale_x_discrete(labels = xTicks) + 
        ggsci::scale_color_npg() + 
        theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      return(gp)
    }
    if (ethEthSel=='2'){
      xTicks <- NULL
      for(a in ethnicity_group_details){
        num <- sum(dplot$eth == a)
        if(num > 0){
          xTicks <- c(xTicks, paste0(a, "\n(n=", num, ")"))
        }
      }
      gp <- ggplot(dplot) + geom_boxplot(aes(x=eth, y = x, fill = group)) + 
        #geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
        labs(x="Ethnicity", y = ratioName, fill = label) + 
        scale_x_discrete(labels = xTicks) + 
        ggsci::scale_color_npg() + 
        theme_light() + theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      return(gp)
    }
  }
}







createTableEthAnalytes <- function(
  analyteEth, bwEth, gaEth, ethEthSel, ethEth, sexEth, 
  aabcEth,tpnEth,compareEth){
  
  if(is.null(ethEth)){
    ethEth = c("1", "2", "3", "4")
  }
  
  if(length(analyteEth) == 0){
    return(NULL)
  } else if(length(analyteEth) == 1){
    meta <- meta_data[, as.integer(analyteEth)]
    metaName <- analytes_all[as.integer(analyteEth)]
  } else {
    meta <- meta_data[, as.integer(analyteEth)]
    metaName <- paste(analytes_all[sort(as.integer(analyteEth))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  #idx <- rep(TRUE, nrow(meta_data))
  
  if(length(bwEth) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwEth)]
  }
  
  if(length(gaEth) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaEth)]
  }
  
  if(length(aabcEth) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcEth)]
  }
  
  
  if(length(tpnEth) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnEth)]
  }
  
  if(length(sexEth) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sexEth)]
  }
  
  if (ethEthSel=='1'){
    idxEth  <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethEth)]
  }
  else{
    idxEth  <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethEth)]
  }
  
  idxSel <- idx_include & idxBW & idxGA & idxSex & idxAabc & idxTPN & idxEth
  
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  
  if(length(analyteEth) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  dplot <- data.frame(
    x = x[idxSel],
    eth = ethnicity[idxSel,as.integer(ethEthSel)],
    stringsAsFactors = FALSE
  )
  #print(summary(dplot))
  if(compareEth=="1"){
    colnames(dplot) <- c("x", "Ethnicity")
    tb <- dplot %>%
      group_by(Ethnicity) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(2, 2)
  } else {
    if(compareEth=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareEth=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareEth=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareEth=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else if(compareEth=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } else {
      print("ERROR")
    }
    
    
    colnames(dplot) <- c("x", "Ethnicity", label)
    tb <- dplot %>%
      group_by_at(c("Ethnicity", label)) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(3, 2)
  }
}





createTableEthRatio <- function(
  numeratorEth, denominatorEth, bwEth, gaEth, ethEthSel, ethEth, sexEth, 
  aabcEth, tpnEth, compareEth
){
  if(is.null(ethEth)){
    ethEth = c("1", "2", "3", "4")
  }
  
  if(length(numeratorEth) == 0){
    return(NULL)
  }  else if(length(numeratorEth) == 1){
    numer <- meta_data[, as.integer(numeratorEth)]
    numerName <- analytes_all[as.integer(numeratorEth)]
  } else {
    numer <- meta_data[, as.integer(numeratorEth)]
    numerName <- paste(analytes_all[sort(as.integer(numeratorEth))], collapse = " + ")
  }
  
  
  if(length(denominatorEth) == 0){
    return(NULL)
  }  else if(length(denominatorEth) == 1){
    denom <- meta_data[, as.integer(denominatorEth)]
    denomName <- analytes_all[as.integer(denominatorEth)]
  } else {
    denom <- meta_data[, as.integer(denominatorEth)]
    denomName <- paste(analytes_all[sort(as.integer(denominatorEth))], collapse = " + ")
  }
  
  idxBW <- rep(TRUE, nrow(meta_data))
  idxGA <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  #idx <- rep(TRUE, nrow(meta_data))
  
  if(length(bwEth) > 0){
    idxBW <- flag_bw %in% bw_group[as.integer(bwEth)]
  }
  if(length(sexEth) == 1){
    idxBW <- flag_sex %in% sex_group[as.integer(sexEth)]
  }
  
  if(length(gaEth) > 0){
    idxGA <- flag_ga %in% ga_group[as.integer(gaEth)]
  }
  
  if(length(aabcEth) > 0){
    idxAabc <- flag_aabc %in% aabc_group[as.integer(aabcEth)]
  }
  
  
  if(length(tpnEth) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpnEth)]
  }
  
  if (ethEthSel=='1'){
    idxEth  <- ethnicity$eth_state %in% ethnicity_group[as.integer(ethEth)]
  }
  else{
    idxEth  <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(ethEth)]
  }
  
  idxSel <- idx_include & idxBW & idxGA & idxSex & idxAabc & idxTPN & idxEth
  
  if(sum(idxSel) == 0){
    return(NULL)
  }
  
  if(length(numeratorEth) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominatorEth) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  dplot <- data.frame(
    x = x[idxSel]/y[idxSel],
    eth = ethnicity[idxSel,as.integer(ethEthSel)],
    stringsAsFactors = FALSE
  )
  
  if(compareEth=="1"){
    colnames(dplot) <- c("x", "Ethnicity")
    tb <- dplot %>%
      group_by(Ethnicity) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(2:3, 3)
  } 
  else {
    if(compareEth=="2"){
      dplot$group <- factor(flag_aabc[idxSel], levels = c("12-23", "24-48", "49-72", "73-168"))
      #dplot <- dplot[dplot$group != "NA",]
      label <- "Aabc"
    } else if(compareEth=="4"){
      dplot$group <- factor(flag_bw[idxSel], levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                                        "4001-5000", ">5000"))
      label <- "Birth Weight"
    } else if(compareEth=="5"){
      dplot$group <- factor(flag_ga[idxSel], levels = c("<=27", "28-36",  "37-38",  "39-40",
                                                        "41", "42", ">=43"))
      label <- "Gestational Age"
    } else if(compareEth=="6"){
      dplot$group <- factor(flag_tpn[idxSel], levels = c("NoTPN", "TPN", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "TPN"
    } else if(compareEth=="3"){
      dplot$group <- factor(flag_sex[idxSel], levels = c("Male", "Female", "NA"))
      dplot <- dplot[dplot$group != "NA",]
      label <- "Sex"
    } 
    else {
      print("ERROR")
    }
    
    colnames(dplot) <- c("x", "Ethnicity", label)
    tb <- dplot %>%
      group_by_at(c("Ethnicity", label)) %>%
      summarise(Mean = mean(x), Median = median(x))
    
    datatable(tb,  
              style = "bootstrap4",
              rownames = FALSE) %>% 
      formatRound(3:4, 3)
  }
}


df <- list()
df$numeratorEth <- c('16')
df$denominatorEth <- c('15')
df$bwEth <- c('2','3')
df$gaEth <- c('2','3')
df$ethEthSel <- c('1')
df$ethEth <- c('1','2','3','4')
df$sexEth <- c('1')
df$aabcEth <- c('1','2')
df$tpnEth <- c('1')
df$compareEth <- c('4')
  
plotBoxplotEthRatio (df$numeratorEth, df$denominatorEth, df$bwEth, df$gaEth, df$ethEthSel, df$ethEth, df$sexEth, 
  df$aabcEth, df$tpnEth, df$compareEth
)
  