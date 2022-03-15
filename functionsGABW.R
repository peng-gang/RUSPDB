library(ComplexHeatmap)
library(circlize)
library(ggsci)
library(ggpubr)

plotHeatGABWAnalytes <- function(
  analyte, ethSel, eth, sex, aabc, tpn){
  
  if(is.null(eth)){
    eth = c("1", "2", "3", "4")
  }
  
  
  if(length(analyte) == 0){
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
    
  }  else if(length(analyte) == 1){
    meta <- meta_data[, as.integer(analyte)]
    metaName <- analytes_all[as.integer(analyte)]
  } else {
    meta <- meta_data[, as.integer(analyte)]
    metaName <- paste(analytes_all[sort(as.integer(analyte))], collapse = " + ")
  }
  
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(ethSel == "1"){
    if(length(eth) > 0 && length(eth) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(eth)]
    }
  } else {
    if(length(eth) > 0 && length(eth) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(eth)]
    }
  }
  
  if(length(sex) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sex)]
  }
  
  idxAabc <- flag_aabc %in% aabc_group[as.integer(aabc)]
  
  #print(sum(idxAabc))
  
  if(length(tpn) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpn)]
  }
  
  idxSel <- idx_include & idxEth & idxSex & idxAabc & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyte) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- matrix(0, nrow = 5, ncol = 5)
  sampleSize <- matrix(0, nrow = 5, ncol = 5)
  for(i in 1:5){
    for(j in 1:5){
      dplot[i,j] <- median(x[idxSel & (flag_ga %in% ga_group[j]) & (flag_bw %in% bw_group[i])])
      sampleSize[i,j] <- sum(idxSel & (flag_ga %in% ga_group[j]) & (flag_bw %in% bw_group[i]))
    }
  }
  
  colnames(dplot) <- ga_group
  rownames(dplot) <- bw_group
  
  #print(dplot)
  #print(min(dplot, na.rm = TRUE))
  #print(max(dplot, na.rm = TRUE))
  #anaGA <- columnAnnotation(l1 = anno_lines(matrix(rnorm(50), ncol = 10, nrow = 5), which = "row"))
  #anaBW <- rowAnnotation(l2 = anno_lines(matrix(rnorm(50), ncol = 10, nrow = 5), which = "column"))
  f1 = colorRamp2(seq(min(dplot, na.rm = TRUE), max(dplot, na.rm = TRUE), length = 3), c("blue", "white", "red"),transparency = 0.2)
  
  ht <- Heatmap(
    dplot,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    
    name = metaName,
    column_title = "Gestational Age (week)",
    row_title = "Birth Weight (g)",
    column_title_side = "bottom",
    row_names_side = "left",
    column_title_gp = gpar(fontsize = 14), 
    row_title_gp = gpar(fontsize = 14),
    
    col = f1,
    column_names_rot = 45,
    rect_gp = gpar(col = "white", lwd = 2),
    width = ncol(dplot)*unit(2.5, "cm"),
    height = nrow(dplot)*unit(2.2, "cm"),
    
    cell_fun = function(j, i, x, y, width, height, fill){
      grid.text(paste0(sprintf("%.2f", dplot[i, j]), "\n(n=", sampleSize[i,j], ")"), x, y, gp = gpar(fontsize = 12))
    },
    heatmap_legend_param = list(title = metaName, 
                                title_gp = gpar(fontsize = 10), 
                                labels_gp = gpar(fontsize = 10), 
                                color_bar='continous',
                                #labels = c("low", "zero", "high"),
                                direction = "vertical",
                                at = seq(min(dplot, na.rm = TRUE),max(dplot,na.rm = TRUE),round((max(dplot, na.rm = TRUE)-min(dplot, na.rm = TRUE))/5,3)),
                                legend_height = unit(8, "cm"),
                                legend_width = unit(8, "cm")
    )
    
    #top_annotation = anaGA,
    #right_annotation = anaBW
  )
  
  return(ht)
}




plotHeatGABWRatio <- function(
  numerator, denominator, ethSel, eth, sex, aabc, tpn
){
  if(is.null(eth)){
    eth = c("1", "2", "3", "4")
  }
  
  if(length(numerator) == 0){
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
    
  }  else if(length(numerator) == 1){
    numer <- meta_data[, as.integer(numerator)]
    numerName <- analytes_all[as.integer(numerator)]
  } else {
    numer <- meta_data[, as.integer(numerator)]
    numerName <- paste(analytes_all[sort(as.integer(numerator))], collapse = " + ")
  }
  
  
  if(length(denominator) == 0){
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
    
  }  else if(length(denominator) == 1){
    denom <- meta_data[, as.integer(denominator)]
    denomName <- analytes_all[as.integer(denominator)]
  } else {
    denom <- meta_data[, as.integer(denominator)]
    denomName <- paste(analytes_all[sort(as.integer(denominator))], collapse = " + ")
  }
  
  
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(ethSel == "1"){
    if(length(eth) > 0 && length(eth) < length(ethnicity_group)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(eth)]
    }
  } else {
    if(length(eth) > 0 && length(eth) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(eth)]
    }
  }
  
  #print(eth)
  #print(sum(idxEth))
  
  if(length(sex) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sex)]
  }
  
  idxAabc <- flag_aabc %in% aabc_group[as.integer(aabc)]
  
  #print(sum(idxAabc))
  
  if(length(tpn) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpn)]
  }
  
  idxSel <- idx_include & idxEth & idxSex & idxAabc & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(numerator) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominator) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  ratio <- x/y
  
  dplot <- matrix(0, nrow = 5, ncol = 5)
  sampleSize <- matrix(0, nrow = 5, ncol = 5)
  for(i in 1:5){
    for(j in 1:5){
      dplot[i,j] <- median(ratio[idxSel & (flag_ga %in% ga_group[j]) & (flag_bw %in% bw_group[i])])
      sampleSize[i,j] <- sum(idxSel & (flag_ga %in% ga_group[j]) & (flag_bw %in% bw_group[i]))
    }
  }
  
  colnames(dplot) <- ga_group
  rownames(dplot) <- bw_group
  
  
  #anaGA <- columnAnnotation(l1 = anno_lines(matrix(rnorm(50), ncol = 10, nrow = 5), which = "row"))
  #anaBW <- rowAnnotation(l2 = anno_lines(matrix(rnorm(50), ncol = 10, nrow = 5), which = "column"))
  f1 = colorRamp2(seq(min(dplot, na.rm = TRUE), max(dplot, na.rm = TRUE), length = 3), c("blue", "white", "red"),transparency = 0.2)
  
  ht <- Heatmap(
    dplot,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    
    name = ratioName,
    column_title = "Gestational Age (week)",
    row_title = "Birth Weight (g)",
    column_title_side = "bottom",
    row_names_side = "left",
    
    col = f1,
    column_title_gp = gpar(fontsize = 14), 
    row_title_gp = gpar(fontsize = 14),
    column_names_rot = 45,
    rect_gp = gpar(col = "white", lwd = 2),
    width = ncol(dplot)*unit(2.5, "cm"),
    height = nrow(dplot)*unit(2.2, "cm"),
    
    cell_fun = function(j, i, x, y, width, height, fill){
      if(median(dplot)>=0.1){
        grid.text(paste0(sprintf("%.2f", dplot[i, j]), "\n(n=", sampleSize[i,j], ")"), x, y, gp = gpar(fontsize = 12))
      } else if (median(dplot)>=0.01) {
        grid.text(paste0(sprintf("%.3f", dplot[i, j]), "\n(n=", sampleSize[i,j], ")"), x, y, gp = gpar(fontsize = 12))
      } else if (median(dplot)>=0.001) {
        grid.text(paste0(sprintf("%.4f", dplot[i, j]), "\n(n=", sampleSize[i,j], ")"), x, y, gp = gpar(fontsize = 12))
      } else {
        grid.text(paste0(sprintf("%.2e", dplot[i, j]), "\n(n=", sampleSize[i,j], ")"), x, y, gp = gpar(fontsize = 12))
      }
    },
    heatmap_legend_param = list(title = ratioName, 
                                title_gp = gpar(fontsize = 10), 
                                labels_gp = gpar(fontsize = 10), 
                                color_bar='continous',
                                #labels = c("low", "zero", "high"),
                                direction = "vertical",
                                #at = seq(min(dplot),max(dplot),0.01),
                                legend_height = unit(8, "cm"),
                                legend_width = unit(8, "cm")
    )
    
    #top_annotation = anaGA,
    #right_annotation = anaBW
  )
  
  return(ht)
}







plotTrendGABWAnalytes <- function(
  analyte, ethSel, eth, sex, aabc, tpn){
  
  if(is.null(eth)){
    eth == "1"
  }
  
  
  if(length(analyte) == 0){
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
    
  }  else if(length(analyte) == 1){
    meta <- meta_data[, as.integer(analyte)]
    metaName <- analytes_all[as.integer(analyte)]
  } else {
    meta <- meta_data[, as.integer(analyte)]
    metaName <- paste(analytes_all[sort(as.integer(analyte))], collapse = " + ")
  }
  
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(ethSel == "1"){
    if(length(eth) > 0 && length(eth) < length(eth)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(eth)]
    }
  } else {
    if(length(eth) > 0 && length(eth) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(eth)]
    }
  }
  
  if(length(sex) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sex)]
  }
  
  idxAabc <- flag_aabc %in% aabc_group[as.integer(aabc)]
  
  #print(sum(idxAabc))
  
  if(length(tpn) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpn)]
  }
  
  idxSel <- idx_include & idxEth & idxSex & idxAabc & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(analyte) == 1){
    x <- meta
  } else {
    x <- rowSums(meta)
  }
  
  dplot <- data.frame(
    y = c(x[idxSel],x[idxSel]),
    BW = c(birth_weight[idxSel], birth_weight[idxSel]),
    GA = c(gestational_age[idxSel], gestational_age[idxSel]),
    BWG = factor(c(rep("All", sum(idxSel)), flag_bw[idxSel]), levels = c("All", bw_group)),
    GAG = factor(c(rep("All", sum(idxSel)), flag_ga[idxSel]), levels = c("All", ga_group)),
    stringsAsFactors = FALSE
  )
  
  gpGA <- ggplot(dplot[dplot$BWG %in% c("All", "2500-3000", "3001-3500", "3501-4000"),]) +
    geom_smooth(aes(x=GA, y=y, color=BWG), method = "gam", formula = y ~ s(x, bs = "cs", k=5)) +
    #geom_smooth(aes(x=GA, y=y, color=BWG)) +
    labs(x="Gestational Age (Week)", y=metaName) + 
    scale_x_continuous(limits = c(34,42), breaks = c(34, 36, 38, 40, 42)) + 
    scale_color_nejm() + 
    theme_light() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  gpBW <- ggplot(dplot[dplot$GAG %in% c("All", "37-38", "39-40", "41"), ]) + 
    geom_smooth(aes(x=BW, y=y, color=GAG), method = "gam", formula = y ~ s(x, bs = "cs")) + 
    #geom_smooth(aes(x=BW, y=y, color=GAG)) + 
    labs(x="Birth Weight (g)", y=metaName) + 
    scale_x_continuous(limits = c(2000,4500)) + 
    scale_color_nejm() + 
    theme_light() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  
  gp <- ggarrange(gpGA, gpBW, ncol = 2)
  gp
}




plotTrendGABWRatio <- function(
  numerator, denominator, ethSel, eth, sex, aabc, tpn
){
  if(is.null(eth)){
    eth == "1"
  }
  
  if(length(numerator) == 0){
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
    
  }  else if(length(numerator) == 1){
    numer <- meta_data[, as.integer(numerator)]
    numerName <- analytes_all[as.integer(numerator)]
  } else {
    numer <- meta_data[, as.integer(numerator)]
    numerName <- paste(analytes_all[sort(as.integer(numerator))], collapse = " + ")
  }
  
  
  if(length(denominator) == 0){
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
    
  }  else if(length(denominator) == 1){
    denom <- meta_data[, as.integer(denominator)]
    denomName <- analytes_all[as.integer(denominator)]
  } else {
    denom <- meta_data[, as.integer(denominator)]
    denomName <- paste(analytes_all[sort(as.integer(denominator))], collapse = " + ")
  }
  
  
  idxEth <- rep(TRUE, nrow(meta_data))
  idxSex <- rep(TRUE, nrow(meta_data))
  idxAabc <- rep(TRUE, nrow(meta_data))
  idxTPN <- rep(TRUE, nrow(meta_data))
  
  if(ethSel == "1"){
    if(length(eth) > 0 && length(eth) < length(eth)){
      idxEth <- ethnicity$eth_state %in% ethnicity_group[as.integer(eth)]
    }
  } else {
    if(length(eth) > 0 && length(eth) < length(ethnicity_group_details)){
      idxEth <- ethnicity$eth_detail %in% ethnicity_group_details[as.integer(eth)]
    }
  }
  
  if(length(sex) == 1){
    idxSex <- flag_sex %in% sex_group[as.integer(sex)]
  }
  
  idxAabc <- flag_aabc %in% aabc_group[as.integer(aabc)]
  
  #print(sum(idxAabc))
  
  if(length(tpn) == 1){
    idxTPN <- flag_tpn %in% tpn_group[as.integer(tpn)]
  }
  
  idxSel <- idx_include & idxEth & idxSex & idxAabc & idxTPN
  
  if(sum(idxSel) == 0){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "No newborn was selected")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  
  if(length(numerator) == 1){
    x <- numer
  } else {
    x <- rowSums(numer)
    numerName <- paste0("(", numerName, ")")
  }
  
  if(length(denominator) == 1){
    y <- denom
  } else {
    y <- rowSums(denom)
    denomName <- paste0("(", denomName, ")")
  }
  
  ratioName <- paste0(numerName, "/", denomName)
  
  ratio <- x/y
  
  dplot <- data.frame(
    y = c(ratio[idxSel],ratio[idxSel]),
    BW = c(birth_weight[idxSel], birth_weight[idxSel]),
    GA = c(gestational_age[idxSel], gestational_age[idxSel]),
    BWG = factor(c(rep("All", sum(idxSel)), flag_bw[idxSel]), levels = c("All", bw_group)),
    GAG = factor(c(rep("All", sum(idxSel)), flag_ga[idxSel]), levels = c("All", ga_group)),
    stringsAsFactors = FALSE
  )
  
  gpGA <- ggplot(dplot[dplot$BWG %in% c("All", "2500-3000", "3001-3500", "3501-4000"),]) + 
    geom_smooth(aes(x=GA, y=y, color=BWG), method = "gam", formula = y ~ s(x, bs = "cs", k=5)) + 
    labs(x="Gestational Age (Week)", y=ratioName) + 
    scale_x_continuous(limits = c(34,42), breaks = c(34, 36, 38, 40, 42)) + 
    scale_color_nejm() + 
    theme_light() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  gpBW <- ggplot(dplot[dplot$GAG %in% c("All", "37-38", "39-40", "41"), ]) + 
    geom_smooth(aes(x=BW, y=y, color=GAG), method = "gam", formula = y ~ s(x, bs = "cs")) + 
    labs(x="Birth Weight (g)", y=ratioName) + 
    scale_x_continuous(limits = c(2000,4500)) + 
    scale_color_nejm() + 
    theme_light() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  
  gp <- ggarrange(gpGA, gpBW, ncol = 2)
  gp
}
