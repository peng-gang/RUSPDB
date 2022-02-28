library(ComplexHeatmap)
library(circlize)

plotHeatBWGAAnalytes <- function(
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
  
  print(sum(idxAabc))
  
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
  for(i in 1:5){
    for(j in 1:5){
      dplot[i,j] <- median(x[idxSel & (flag_ga %in% ga_group[j]) & (flag_bw %in% bw_group[i])])
    }
  }
  
  colnames(dplot) <- ga_group
  rownames(dplot) <- bw_group
  
  
  #anaGA <- columnAnnotation(l1 = anno_lines(matrix(rnorm(50), ncol = 10, nrow = 5), which = "row"))
  #anaBW <- rowAnnotation(l2 = anno_lines(matrix(rnorm(50), ncol = 10, nrow = 5), which = "column"))
  ht <- Heatmap(
    dplot,
    cluster_rows = FALSE,
    cluster_columns = FALSE
    #top_annotation = anaGA,
    #right_annotation = anaBW
  )
  
  return(ht)
}


plotHeatBWGARatio <- function(
  numerator, denominator, ethSel, eth, sex, aabc, tpn
){
  return(NULL)
}