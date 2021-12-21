## functions

library(ggplot2)
library(ggsci)

makeList <- function(x){
  rlt <- list()
  for(i in 1:length(x)){
    rlt[[i]] <- i
  }
  names(rlt) <- x
  rlt
}


aacBoxPlot <- function(x, flag_aac, idx_include, idx_sel = NULL, ylab){
  if(is.null(x)){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select analytes from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  dplot <- data.frame(
    x = x,
    aac = flag_aac
  )
  
  if(is.null(idx_sel)){
    dplot <- dplot[idx_include, ]
  } else {
    dplot <- dplot[idx_include & idx_sel, ]
  }
  
  num_pre <- sum(dplot$aac == "12-23")
  num_mid <- sum(dplot$aac == "24-48")
  num_post <- sum(dplot$aac == "49-168")
  
  gp <- ggplot(dplot) + geom_boxplot(aes(x=aac, y = x)) + 
    geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
    labs(x="Age at Blood Collection (Hour)", y = ylab) + 
    scale_x_discrete(
      labels = c(
        paste0("12-23\n(n=", num_pre, ")"),
        paste0("24-48\n(n=", num_mid, ")"),
        paste0("49-168\n(n=", num_post, ")"))) + 
    theme_light() + theme(text = element_text(size = 18))
  
  gp
}


aacBoxPlot72 <- function(x, aac, idx_include, idx_sel = NULL, ylab){
  if(is.null(x)){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select analytes from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  dplot <- data.frame(
    x = x,
    aac = aac
  )
  
  if(is.null(idx_sel)){
    dplot <- dplot[idx_include & aac<=72, ]
  } else {
    dplot <- dplot[idx_include & idx_sel & aac <=72, ]
  }
  
  aacNum <- dplot$aac
  
  dplot$aac[which(aacNum >=12 & aacNum <= 23)] <- "12-23"
  dplot$aac[which(aacNum >=24 & aacNum <= 48)] <- "24-48"
  dplot$aac[which(aacNum >=49 & aacNum <= 72)] <- "49-72"
  
  num_pre <- sum(dplot$aac == "12-23")
  num_mid <- sum(dplot$aac == "24-48")
  num_post <- sum(dplot$aac == "49-72")
  
  gp <- ggplot(dplot) + geom_boxplot(aes(x=aac, y = x)) + 
    geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
    labs(x="Age at Blood Collection (Hour)", y = ylab) + 
    scale_x_discrete(
      labels = c(
        paste0("12-23\n(n=", num_pre, ")"),
        paste0("24-48\n(n=", num_mid, ")"),
        paste0("49-72\n(n=", num_post, ")"))) + 
    theme_light() + theme(text = element_text(size = 18))
  
  gp
}


aacBoxPlotCompare <- function(x, flag_aac, idxGroup, idx_include, idx_sel = NULL,
                              flag_sex, flag_bw, flag_ga, flag_race, flag_tpn, ylab){
  if(is.null(x)){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select analytes from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  dplot <- data.frame(
    x = x,
    aac = flag_aac
  )
  
  label <- NULL
  if(idxGroup == 2){
    dplot$group <- factor(flag_sex, levels = c("Male", "Female", "NA"))
    idx_include <- idx_include & (flag_sex != "NA")
    label <- "Sex"
  } else if(idxGroup == 3){
    dplot$group <- factor(flag_bw, levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                              "4001-5000", ">5000"))
    label <- "Birth Weight"
  } else if(idxGroup == 4){
    dplot$group <- factor(flag_ga, levels = c("<=27", "28-36",  "37-38",  "39-40",
                                              "41", "42", ">=43"))
    label <- "Gestational Age"
  } else if(idxGroup == 5){
    dplot$group <- factor(flag_race, levels = c("Asian", "Black", "Hispanic", "White", 
                                                "OtherUnknown"))
    idx_include <- idx_include & (flag_race != "OtherUnknown")
    label <- "Race"
  } else if(idxGroup == 6){
    dplot$group <- factor(flag_tpn, levels = c("NoTPN", "TPN", "NA"))
    idx_include <- idx_include & (flag_tpn != "NA")
    label <- "TPN"
  } else {
    "ERROR"
  }
  
  
  if(is.null(idx_sel)){
    dplot <- dplot[idx_include, ]
  } else {
    dplot <- dplot[idx_include & idx_sel, ]
  }
  
  num_pre <- sum(dplot$aac == "12-23")
  num_mid <- sum(dplot$aac == "24-48")
  num_post <- sum(dplot$aac == "49-168")
  
  gp <- ggplot(dplot) + geom_boxplot(aes(x=aac, y = x, fill = group)) + 
    geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
    labs(x="Age at Blood Collection (Hour)", y = ylab) + 
    scale_x_discrete(
      labels = c(
        paste0("12-23\n(n=", num_pre, ")"),
        paste0("24-48\n(n=", num_mid, ")"),
        paste0("49-168\n(n=", num_post, ")"))) + 
    labs(fill = label) + scale_color_npg() + 
    theme_light() + theme(text = element_text(size = 18))
  
  gp
}



aacBoxPlotCompare72 <- function(x, aac, idxGroup, idx_include, idx_sel = NULL,
                              flag_sex, flag_bw, flag_ga, flag_race, flag_tpn, ylab){
  if(is.null(x)){
    gp <- ggplot(data.frame(x=0.5, y=0.5, label = "Please select analytes from the left panel")) + 
      geom_text(aes(x=x, y=y, label=label)) + 
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) + 
      theme_void() + theme(text = element_text(size = 24))
    
    return(gp)
  }
  
  dplot <- data.frame(
    x = x,
    aac = aac
  )
  
  label <- NULL
  if(idxGroup == 2){
    dplot$group <- factor(flag_sex, levels = c("Male", "Female", "NA"))
    idx_include <- idx_include & (flag_sex != "NA")
    label <- "Sex"
  } else if(idxGroup == 3){
    dplot$group <- factor(flag_bw, levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                              "4001-5000", ">5000"))
    label <- "Birth Weight"
  } else if(idxGroup == 4){
    dplot$group <- factor(flag_ga, levels = c("<=27", "28-36",  "37-38",  "39-40",
                                              "41", "42", ">=43"))
    label <- "Gestational Age"
  } else if(idxGroup == 5){
    dplot$group <- factor(flag_race, levels = c("Asian", "Black", "Hispanic", "White", 
                                                "OtherUnknown"))
    idx_include <- idx_include & (flag_race != "OtherUnknown")
    label <- "Race"
  } else if(idxGroup == 6){
    dplot$group <- factor(flag_tpn, levels = c("NoTPN", "TPN", "NA"))
    idx_include <- idx_include & (flag_tpn != "NA")
    label <- "TPN"
  } else {
    "ERROR"
  }
  
  if(is.null(idx_sel)){
    dplot <- dplot[idx_include & aac<=72, ]
  } else {
    dplot <- dplot[idx_include & idx_sel & aac <=72, ]
  }
  
  aacNum <- dplot$aac
  
  dplot$aac[which(aacNum >=12 & aacNum <= 23)] <- "12-23"
  dplot$aac[which(aacNum >=24 & aacNum <= 48)] <- "24-48"
  dplot$aac[which(aacNum >=49 & aacNum <= 72)] <- "49-72"
  
  num_pre <- sum(dplot$aac == "12-23")
  num_mid <- sum(dplot$aac == "24-48")
  num_post <- sum(dplot$aac == "49-72")
  
  gp <- ggplot(dplot) + geom_boxplot(aes(x=aac, y = x, fill = group)) + 
    geom_hline(yintercept = median(dplot$x[dplot$aac == "24-48"]), color = "#E18727FF") + 
    labs(x="Age at Blood Collection (Hour)", y = ylab) + 
    scale_x_discrete(
      labels = c(
        paste0("12-23\n(n=", num_pre, ")"),
        paste0("24-48\n(n=", num_mid, ")"),
        paste0("49-72\n(n=", num_post, ")"))) + 
    labs(fill = label) + scale_color_npg() + 
    theme_light() + theme(text = element_text(size = 18))
  
  gp
}


aacTrend <- function(x, aac, idx_include, idx_sel = NULL, ylab){
  if(is.null(x)){
    return(NULL)
  }
  dplot <- data.frame(
    x = x,
    aac = aac
  )
  
  if(is.null(idx_sel)){
    dplot <- dplot[idx_include, ]
  } else {
    dplot <- dplot[idx_include & idx_sel, ]
  }
  
  gp <- ggplot(dplot) + geom_smooth(aes(x=aac, y = x), method = "gam", formula = y ~ s(x, bs = "cs")) + 
    labs(x = paste0("Age at Blood Collection (Hour)\n(n=", nrow(dplot), ")"), y = ylab) + 
    scale_x_continuous(breaks = seq(0, 168, 24), minor_breaks = NULL) + 
    theme_light() + theme(text = element_text(size = 18))
  
  gp
}



aacTrendCompare <- function(x, aac, idxGroup, idx_include, idx_sel = NULL,  
                            flag_sex, flag_bw, flag_ga, flag_race, flag_tpn,
                            ylab){
  if(is.null(x)){
    return(NULL)
  }
  
  dplot <- data.frame(
    x = x,
    aac = aac
  )
  
  label <- NULL
  if(idxGroup == 2){
    dplot$group <- factor(flag_sex, levels = c("Male", "Female", "NA"))
    idx_include <- idx_include & (flag_sex != "NA")
    label <- "Sex"
  } else if(idxGroup == 3){
    dplot$group <- factor(flag_bw, levels = c("<1000", "1000-2499", "2500-3000", "3001-3500", "3501-4000",
                                             "4001-5000", ">5000"))
    label <- "Birth Weight"
  } else if(idxGroup == 4){
    dplot$group <- factor(flag_ga, levels = c("<=27", "28-36",  "37-38",  "39-40",
                                              "41", "42", ">=43"))
    label <- "Gestational Age"
  } else if(idxGroup == 5){
    dplot$group <- factor(flag_race, levels = c("Asian", "Black", "Hispanic", "White", 
                                                "OtherUnknown"))
    idx_include <- idx_include & (flag_race != "OtherUnknown")
    label <- "Race"
  } else if(idxGroup == 6){
    dplot$group <- factor(flag_tpn, levels = c("NoTPN", "TPN", "NA"))
    idx_include <- idx_include & (flag_tpn != "NA")
    label <- "TPN"
  } else {
    "ERROR"
  }
  
  
  if(is.null(idx_sel)){
    dplot <- dplot[idx_include, ]
  } else {
    dplot <- dplot[idx_include & idx_sel, ]
  }
  
  gp <- ggplot(dplot) + geom_smooth(aes(x=aac, y = x, color = group, fill = group), method = "gam", formula = y ~ s(x, bs = "cs")) + 
    labs(x = paste0("Age at Blood Collection (Hour)\n(n=", nrow(dplot), ")"), y = ylab) + 
    scale_x_continuous(breaks = seq(0, 168, 24), minor_breaks = NULL) + 
    labs(color = label, fill = label) + scale_color_npg() + 
    theme_light() + theme(text = element_text(size = 18))
  
  gp
}
