# My Functions for MSSE analysis

mf_plotANA <- function(dataset, datajitter = NA, day, time = "per1", graph, color = ""){
  library(dplyr)
  library(ggplot2)
  
  if(color == "mono"){
    SED_color = "white"
    LIE_color = "grey85"
    LIE_ANA_color = "grey30"
  }
  else {
    SED_color = "grey85"
    LIE_color = "cornflowerblue"
    LIE_ANA_color = "orange"
  }
  if(graph == "FC"){
    graphname = "ANAFC"
    sumFC <- dataset %>%
      group_by(Group, Time) %>%
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "LIE_Sal")) %>%
      mutate(Group = relevel(Group, ref = "SED_Sal"))
    
    g <- ggplot(sumFC, aes(x = Time, y = mean, group = Group, fill = Group)) +
      geom_line(size = 0.8) +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = 0.2) +
      geom_point(size = 5, shape = 21) +
      labs(title = "Fear Conditioning") +
      labs(x = "Time (min)", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_fill_manual(values = c(SED_Sal = SED_color, LIE_Sal = LIE_color, LIE_ANA = LIE_ANA_color)) +
      scale_x_continuous(limits = c(1, 6), breaks = c(1, 2, 3, 4, 5, 6)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            # legend.position = c(.27, .8),
            legend.key = element_blank(), 
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            axis.text= element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 20))
    plot(g)
    # ggsave(filename = "Result/ANA???|???Ωø?ΩΩ??Ωø?ΩΩ??Ωø?ΩΩ??Ωø?ΩΩt??.png", width = 3.5, height = 3, dpi = 300)
  }
  else if(graph == "bar"){
    sumExb <- dataset %>% 
      group_by(Group) %>% 
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "LIE_Sal")) %>% 
      mutate(Group = relevel(Group, ref = "SED_Sal"))
    
    if(day == 1){
      titlename = "Extinction Day 1"
      graphname = "ANAEx1bar"
      name = "ANA_Extinction_Day1"
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "ANAEx2bar"
      name = "ANA_Extiction_Day2"
    }  
    g <- ggplot(sumExb, aes(x = Group, y = mean, fill = Group, color = Group)) +
      geom_bar(stat = 'identity', position ='dodge', width = .7, colour = "black") +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = .2, color = "black") +
      geom_jitter(data = datajitter, aes(x = Group, y = Freezing),
                  height = 0, width = 0.1, size = 3, alpha = 0.7,
                  fill = "white", color = "black", shape = 23) +
      labs(title = titlename,x = "", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0, 0),limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_discrete(limits = c("SED_Sal", "LIE_Sal", "LIE_ANA")) +
      scale_fill_manual(values = c(SED_Sal = SED_color, LIE_Sal = LIE_color, LIE_ANA = LIE_ANA_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 22, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 20, colour = "black"),
            axis.text.y = element_text(size = 18, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_blank()) 
    
    plot(g)
    # ggsave(filename = paste0("Result/","ANA", name, ".png"), width = 3.5, height = 3.5, dpi = 300)
  }
  else if(graph == "line"){
    sumEx <- dataset %>%
      group_by(Group, Time) %>% 
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "LIE_Sal")) %>%
      mutate(Group = relevel(Group, ref = "SED_Sal"))
    xlabel = "Time (min)"
    if (day == 1){
      titlename = "Extinction Day 1"
      graphname = "ANAEx1"
      name = "ANA_Extinction_Day1"
      if(time == "per5"){
        xlabel = "Time (per 5 min)"
        graphname = "ANAEx1per5"
        name = "ANA_Extinction_Day1_per5min"
      }
      else if(time == "per3"){
        xlabel = "Time (per 3 min)"
        graphname = "ANAEx1per3"
        name = "ANA_Extinction_Day1_per3min"
      }
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "ANAEx2"
      name = "ANA_Extinction_Day2"
      if (time == "per5"){
        xlabel = "Time (per 5 min)"
        graphname = "ANAEx2per5"
        name = "ANA_Extinction_Day2_per5min"
      }
      else if (time == "per3"){
        xlabel = "Time (per 3 min)"
        graphname = "ANAEx2per3"
        name = "ANA_Extinction_Day2_per3min"
      }
    }
    g <- ggplot(sumEx, aes(x = Time, y = mean, group = Group, fill = Group)) +
      geom_line(size = .8) +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = 0.2) +
      geom_point(size = 5, shape = 21) +
      labs(title = titlename) +
      labs(x = xlabel, y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_fill_manual(values = c(SED_Sal = SED_color, LIE_Sal = LIE_color, LIE_ANA = LIE_ANA_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            # legend.title = element_blank(),
            # legend.text = element_text(size = 16),
            axis.text = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 20))
    
    if (time == "per5"){
      g <- g +
        scale_x_discrete(limits = c("5", "10", "15"))
    }
    else if (time == "per3"){
      g <- g +
        scale_x_discrete(limits = c("3", "6", "9", "12", "15"))
    }
    else {
      g <- g +
        scale_x_continuous(limits = c(1, 15), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
        theme(axis.text.x = element_text(size = 11, color = "black"))
    }
    plot(g)
    # ggsave(filename = paste0("Result/", "ANA", name, ".png"), width = 3.5, height = 3, dpi = 300)
  }
  else if(graph == "box"){
    if(day == 1){
      titlename = "Extinction Day 1"
      graphname = "ANAEx1box"
      name = "ANA_Extinction_Day1_Boxplot"
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "ANAEx2box"
      name = "ANA_Extinction_Day2_Boxplot"
    }  
    g <- ggplot(data = dataset, aes(x = Group, y = Freezing, fill = Group)) +
      stat_boxplot(geom = "errorbar", width = .2) +
      geom_boxplot(color = "black", width = .7, outlier.colour = NA) +
      stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 4, fill = "white") +
      geom_jitter(data = dataset, aes(x = Group, y = Freezing),
                  height = 0, width = 0.1, size = 2.5, alpha = 0.7,
                  fill = "white", color = "black", shape = 21) +
      labs(title = titlename,x = "", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0, 0),limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_discrete(limits = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                       labels = c("SED\n_Sal","LIE\n_Sal","LIE\n_ANA")) +
      scale_fill_manual(values = c(SED_Sal = SED_color, LIE_Sal = LIE_color, LIE_ANA = LIE_ANA_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 20, colour = "black"),
            axis.text.y = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20)) 
    
    plot(g)
  }
  assign(str_c("G", "", sep = graphname), g, envir = .GlobalEnv)
}
mf_plotcue <- function(data, datajitter = NA, day, per = "per", graph = "line", color = ""){
  if(color == "mono"){
    SED_color = "white"
    LIE_color = "grey85"
    MOE_color = "grey30"
  }
  else {
    SED_color = "grey85"
    LIE_color = "skyblue"
    MOE_color = "lightgreen"
  }
  if (graph == "line"){
    sumdata <- data %>% 
      group_by(Group, Tone) %>% 
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>% 
      mutate(Group = as.factor(Group)) %>% 
      mutate(Group = relevel(Group, ref = "LIE")) %>% 
      mutate(Group = relevel(Group, ref = "SED"))
    
    if (day == 1){
      titlename = "Extinction Day 1"
      graphname = "cueEx1"
      xlabel = "Tone"
      name = "Cue_Extinction_Day_1"
      if (per == "3tone"){
        graphname = "cueEx1per3"
        xlabel = "Tone (3-tone intervals)"
        name = "Cue_Extinction_Day_1_3-tone_intervals"
      }
      else if (per == "4tone"){
        graphname = "cueEx1per4"
        xlabel = "Tone (4-tone intervals)"
        name = "Cue_Extinction_Day_1_4-tone_intervals"
      }
    }
    else if (day == 2) {
      titlename = "Extinction Day 2"
      graphname = "cueEx2"
      xlabel = "Tone"
      name = "Cue_Extinction_Day_2"
      if (per == "3tone"){
        graphname = "cueEx2per3"
        xlabel = "Tone (3-tone intervals)"
        name = "Cue_Extinction_Day_2_3-tone_intervals"
      }
      else if (per == "4tone"){
        graphname = "cueEx2per4"
        xlabel = "Tone (4-tone intervals)"
        name = "Cue_Extinction_Day_2_4-tone_intervals"
      }
    }
    else if (day == "FC"){
      titlename = "Fear Conditioning"
      graphname = "cueFC"
      xlabel = "Tone"
      name = "Cue_Fear_Conditioning"
    }
    g <- ggplot(sumdata, aes(x = Tone, y = mean, group = Group, fill = Group)) +
      geom_line(size = .8) +
      geom_errorbar(aes(ymax = mean + se,
                        ymin = mean - se),
                    width = .2) +
      geom_point(size = 5, shape = 21) +
      labs(title = titlename) +
      labs(x = xlabel, y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color)) +
      scale_x_discrete(limits = c("pre","1","2","3","4","5","6","7","8","9","10","11","12")) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            # legend.key = element_blank(), 
            legend.title = element_blank(),
            # legend.text = element_text(size = 16),
            axis.text = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 20))
    if(per == "3tone"){
      g <- g +
        scale_x_discrete(limits = c("3", "6", "9", "12"))
    }
    else if(per == "4tone"){
      g <- g +
        scale_x_discrete(limits = c("4", "8", "12"))
    }
    if(day == "FC"){
      g <- g +
        theme(legend.position = c(.2, .8),
              legend.text = element_text(size = 18),
              legend.key = element_blank()) +
        scale_x_discrete(limits = c("pre","1","2","3"))
    }
    plot(g)
    
  }
  else if (graph == "bar"){
    sumExb <- data %>% 
      group_by(Group) %>% 
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "LIE")) %>% 
      mutate(Group = relevel(Group, ref = "SED"))
    if(day == 1){
      titlename = "Extinction Day 1"
      graphname = "cueEx1bar"
      name = "Cue_Extinction_Day_1_Barplot"
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "cueEx2bar"
      name = "Cue_Extinction_Day_2_Barplot"
    }  
    g <- ggplot(sumExb, aes(x = Group, y = mean, fill = Group)) +
      geom_bar(stat = 'identity', position ='dodge', width = .7, colour = "black") +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = .2, color = "black") +
      geom_jitter(data = datajitter, aes(x = Group, y = Freezing),
                  height = 0, width = .1, size = 3, alpha = .7,
                  fill = "white", color = "black", shape = 21) +
      labs(title = titlename, x = "", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0, 0),limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 22, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 20, colour = "black"),
            axis.text.y = element_text(size = 18, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20)) 
    plot(g)
    tiff(filename = paste0("Result/",color,"_", name, ".tiff"), width = 4 * 900, height = 4 * 900, res = 900)
    plot(g)
    dev.off()
  }
  else if (graph == "box"){
    if(day == 1){
      titlename = "Extinction Day 1"
      graphname = "Ex1box"
      name = "Cue_Extinction_Day_1_Boxplot"
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "Ex2box"
      name = "Cue_Extinction_Day_2_Boxplot"
    }  
    g <- ggplot(data = data, aes(x = Group, y = Freezing, fill = Group)) +
      stat_boxplot(geom = "errorbar", width = .2) +
      geom_boxplot(color = "black", width = .7, outlier.colour = NA) +
      stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 4, fill = "white") +
      geom_jitter(data = data, aes(x = Group, y = Freezing),
                  height = 0, width = 0.1, size = 2.5, alpha = 0.7,
                  fill = "white", color = "black", shape = 21) +
      labs(title = titlename,x = "", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0, 0),limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_discrete(limits = c("SED", "LIE", "MOE")) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 22, colour = "black"),
            axis.text.y = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_blank()) 
    plot(g)
  }
  assign(str_c("G", "cue", sep = graphname), g, envir = .GlobalEnv)
}
mf_smpplot <- function(dataset, datajit, titlename, design, 
                       muscle = FALSE, exp = "exp", graph = "bar", color = ""){
  sumdata <- dataset %>% 
    group_by(Group) %>% 
    summarise(mean = mean(val),
              se = sd(val)/sqrt(n()-1)) %>% 
    mutate(Group = as.factor(Group))
  dataset <- dataset %>% 
    mutate(Group = as.factor(Group))
  
  if (exp == "ANA"){
    sumdata <- sumdata %>%
      mutate(Group = relevel(Group, ref = "LIE_Sal")) %>%
      mutate(Group = relevel(Group, ref = "SED_Sal"))
    dataset <- dataset %>% 
      mutate(Group = relevel(Group, ref = "LIE_Sal")) %>%
      mutate(Group = relevel(Group, ref = "SED_Sal"))
    
  }
  else if ((exp == "cue")|(exp == "exp")){
    sumdata <- sumdata %>%
      mutate(Group = relevel(Group, ref = "LIE")) %>%
      mutate(Group = relevel(Group, ref = "SED"))
    
    dataset <- dataset %>% 
      mutate(Group = relevel(Group, ref = "LIE")) %>%
      mutate(Group = relevel(Group, ref = "SED"))
  }
  
  if(muscle == TRUE){
    ylabel = "(mg/100g)"
  }
  else {
    ylabel = paste(titlename, "/ É¿-actin", "\n(% of SED + Saline)")
    if (titlename == "pCREB"){
      ylabel = paste(titlename, "/ tCREB", "\n(% of SED + Saline)")
    }
  }
  
  if (graph == "bar") {
    g <- ggplot(sumdata, aes(x = Group , y = mean, fill = Group)) +
      geom_bar(stat = 'identity', position ='dodge', width = .7, colour = "black") +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = .2, color = "black") +
      labs(title = titlename, x = "", y = ylabel) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 22, colour = "black"),
            axis.text.y = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 22)) 
  }
  else if (graph == "box"){
    g <- ggplot(dataset, aes(x = Group, y = val, fill = Group)) +
      stat_boxplot(geom = "errorbar", width = .2) +
      geom_boxplot(color = "black", width = .7, outlier.colour = NA) +
      stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 4, fill = "white") +
      geom_jitter(data = dataset, aes(x = Group, y = val),
                  height = 0, width = 0.1, size = 2.5, alpha = 0.7,
                  fill = "white", color = "black", shape = 21) +
      labs(title = titlename,x = "", y = ylabel) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 20, colour = "black"),
            axis.text.y = element_text(size = 18, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20)) 
    plot(g)
  }
  
  if(color == "mono"){
    SED_color = "white"
    LIE_color = "grey85"
    MOE_color = "grey30"
    ANA_color = "grey30"
  }
  else{
    SED_color = "grey85"
    LIE_color = "skyblue"
    MOE_color = "lightgreen"
    ANA_color = "orange"
  }
  if (exp == "ANA"){
    if(graph == "bar"){
      g <- g +
        geom_jitter(data = datajit, aes(x = Group, y = val),
                    height = 0, width = 0.1, size = 3, alpha = .7,
                    fill = "white", color = "black", shape = 23) +
        scale_fill_manual(values = c("SED_Sal" = SED_color, "LIE_Sal" = LIE_color, "LIE_ANA" = ANA_color)) +
        theme(axis.title.x = element_text(angle = 45, hjust = 1))
      name = "ANA"
    }
    else if(graph == "box"){
      g <- g +
        scale_x_discrete(labels = c("SED\n_Sal","LIE\n_Sal","LIE\n_ANA")) +
        scale_fill_manual(values = c("SED_Sal" = SED_color, "LIE_Sal" = LIE_color, "LIE_ANA" = ANA_color)) 
    }
    name = "ANA"
  }
  else if (exp == "cue"){
    g <- g +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color))
    name = "cue"
  }
  else if (exp == "exp"){
    g <- g +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color))
    name = ""
  }
  plot(g)
  # ggsave(filename = paste0("Result/", name, titlename, ".png"), width = 3.5, height = 3.5, dpi = 300)
  assign(paste0("G",titlename, design), g, envir = .GlobalEnv)
}
mf_dataset <- function(exp, path, type = "xls"){
  library(dplyr)
  library(stringr)
  library(readxl)
  
  if (str_detect(exp, pattern = "per5")) {
    namestmp <- list.files(path = path,
                           full.names = F,
                           pattern = paste0("\\.", type, "$")) %>% 
      gsub(paste0(".", type), "",.)
    names <- paste0("n", namestmp, "per5")
  } 
  else if (str_detect(exp, pattern = "per3")) {
    namestmp <- list.files(path = path,
                           full.names = F,
                           pattern = paste0("\\.", type, "$")) %>% 
      gsub(paste0(".", type), "",.)
    names <- paste0("n", namestmp, "per3")
  } 
  else if (str_detect(exp, pattern = "jitter")) {
    namestmp <- list.files(path = path,
                           full.names = F,
                           pattern = paste0("\\.", type, "$")) %>% 
      gsub(paste0(".", type), "",.)
    names <- paste0("n", namestmp, "jitter")
  }
  else {
    namestmp <- list.files(path = path,
                           full.names = F,
                           pattern = paste0("\\.", type, "$")) %>% 
      gsub(paste0(".", type), "",.)
    names <- paste0("n", namestmp)
  }
  paths <- list.files(path = path,
                      full.names = T,
                      pattern = paste0(".", type, "$"))
  dataset <- function(dataset){
    name <- gsub("n", "", names[i]) %>% 
      gsub(exp, "",.)
    sliceFC <- c(3:8)
    timeFC <- c(1:6)
    sliceEx <- c(3:17)
    timeEx <- c(1:15)
    per5 = FALSE
    if (exp == "FC"){
      slicerow <- sliceFC
      timerow <- timeFC
    } 
    else if ((exp == "Ex1")||(exp =="Ex2")){
      slicerow <- sliceEx
      timerow <- timeEx
    }
    else {
      per5 = TRUE
    }
    if (per5 == FALSE){
      tmp <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(slicerow) %>%
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(Freezing = Interval...5 * 5/3) %>%
        dplyr::select(Freezing) %>%
        mutate(Time = timerow) %>%
        mutate(No = c(No = name)) 
    } 
    else if (str_detect(exp, pattern = "jitter")){
      tmp <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(3:17) %>%
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(Freezing = Interval...5 * 5/3) %>%
        mutate(Freezing = as.numeric(Freezing)) %>%
        summarise(mean = mean(Freezing)) %>%
        mutate(No = c(No = name)) 
    }
    else if (str_detect(exp, pattern = "per5")){
      A <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(3:7) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      
      B <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(8:12) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      
      C <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(13:17) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      
      tmp <- rbind(A,B,C) %>%
        mutate(Time = c("5", "10", "15")) %>% 
        mutate(No = c(No = name)) #%>%
      # mutate(No = as.numeric(No))
    }
    else if (str_detect(exp, pattern = "per3")){
      A <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(3:5) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      B <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(6:8) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      C <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(9:11) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      D <- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(12:14) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      E<- read_excel(dataset) %>%
        dplyr::select(5) %>%
        dplyr::slice(15:17) %>% 
        mutate(Interval...5 = as.numeric(Interval...5)) %>%
        mutate(val = Interval...5 * 5/3) %>%
        summarise(Freezing = mean(val))
      
      tmp <- rbind(A,B,C,D,E) %>%
        mutate(Time = c("3","6","9","12","15")) %>% 
        mutate(No = c(No = name)) #%>%
      # mutate(No = as.numeric(No))
    }
  }
  # plots <- function(datasets){
  #   datasets <- datasets %>% 
  #     mutate(color = "color")
  #   g <- ggplot(datasets, aes(x = Time, y = Freezing, color = color)) +
  #     geom_point(size = 5) + 
  #     geom_line(size = 1) +
  #     labs(title = gsub("n", "", names[i])) +
  #     labs(x = "Time (min)", y = "Freezing time (%)") +
  #     scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  #     scale_x_continuous(limits = c(1, 15), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  #     theme_classic(base_family = "TNR") +
  #     theme(legend.position = 'none',
  #           axis.text.x = element_text(size = 11, colour = "black"),
  #           axis.text.y = element_text(size = 14, colour = "black"),
  #           axis.line.x = element_line(colour = "black"),
  #           axis.line.y = element_line(colour = "black"),
  #           axis.title = element_text(size = 16))
  # }
  # if ((exp == "Ex1")||(exp == "Ex2")){
  #   for (i in 1:length(paths)) {
  #     assign(names[i], plots(dataset(paths[i])),envir = .GlobalEnv)
  #   }
  # }
  binddata <- data.frame()
  for(i in 1:length(paths)){
    binddata <- rbind(binddata, assign(names[i], dataset(paths[i]), envir = .GlobalEnv))
  }
  
  
  binddata <- binddata %>%
    mutate(Group = if_else(str_detect(No, pattern = "SED"),"SED",
                           if_else(str_detect(No, pattern = "LIE"),"LIE","MOE")))
  
  assign(str_c("data", "", sep = exp), binddata, envir = .GlobalEnv)
}
mf_plotsave <- function(dataset, datajitter = NA, day, time = "per1", graph, color = ""){
  library(dplyr)
  library(ggplot2)
  if(color == "mono"){
    SED_color = "white"
    LIE_color = "grey85"
    MOE_color = "grey30"
  }
  else{
    SED_color = "grey85"
    LIE_color = "skyblue"
    MOE_color = "lightgreen"
  }
  if(graph == "FC"){
    graphname = "FC"
    sumFC <- dataset %>%
      group_by(Group, Time) %>%
      summarise(meanFreezing = mean(Freezing), 
                seFreezing = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "SED"))
    
    g <- ggplot(sumFC, aes(x = Time, y = meanFreezing, group = Group, fill = Group)) +
      geom_line(linewidth = .8) +
      geom_errorbar(aes(ymin = meanFreezing - seFreezing,
                        ymax = meanFreezing + seFreezing),
                    width = 0.2) +
      geom_point(size = 5, shape = 21) +  
      labs(title = "Fear Conditioning") +
      labs(x = "Time (min)", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_continuous(limits = c(1, 6), breaks = c(1, 2, 3, 4, 5, 6)) +
      # scale_fill_discrete(limits = c("SED", "LIE", "MOE")) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE  = MOE_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = c(.2, .8),
            legend.key = element_blank(), 
            legend.title = element_blank(),
            legend.text = element_text(size = 18),
            axis.text = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 20))
    plot(g)
    # ggsave(filename = "Result/.png", width = 3.5, height = 3, dpi = 300)
  }
  else if(graph == "bar"){
    sumExb <- dataset %>% 
      group_by(Group) %>% 
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "SED"))
    if(day == 1){
      titlename = "Extinction Day 1"
      graphname = "Ex1bar"
      name = "Extinction_Day1_bar"
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "Ex2bar"
      name = "Extinction_Day2_bar"
    }  
    g <- ggplot(sumExb, aes(x = Group, y = mean, fill = Group)) +
      geom_bar(stat = 'identity', position ='dodge', width = .7, colour = "black") +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = 0.2) +
      geom_jitter(data = datajitter, aes(x = Group, y = mean),
                  height = 0, width = 0.1, size = 3, alpha = 0.7,
                  fill = "white", color = "black", shape = 21) +
      labs(title = titlename, x = "", y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0, 0),limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 22, hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(size = 20, colour = "black"),
            axis.text.y = element_text(size = 18, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.y = element_text(size = 20),
            axis.title.x = element_blank()) 
    
    plot(g)
    # ggsave(filename = str_c("Result/",".png",sep = name),
    # width = 3.5, height = 3.5, dpi = 300)
  }
  else if(graph == "line"){
    sumEx <- dataset %>%
      group_by(Group, Time) %>% 
      summarise(mean = mean(Freezing),
                se = sd(Freezing)/sqrt(n()-1)) %>%
      mutate(Group = as.factor(Group)) %>%
      mutate(Group = relevel(Group, ref = "SED"))
    xlabel = "Time (min)"
    if (day == 1){
      titlename = "Extinction Day 1"
      graphname = "Ex1"
      name = "Extinction_Day_1"
      if(time == "per5"){
        xlabel = "Time (per 5 min)"
        graphname = "Ex1per5"
        name = "Extinction_Day1_per5min"
      }
      else if(time == "per3"){
        xlabel = "Time (per 3 min)"
        graphname = "Ex1per3"
        name = "Extinction_Day1_per3min"
      }
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "Ex2"
      name = "Extinction_Day2"
      if (time == "per5"){
        xlabel = "Time (per 5 min)"
        graphname = "Ex2per5"
        name = "Extinction_Day2_per5min"
      }
      else if (time == "per3"){
        xlabel = "Time (per 3 min)"
        graphname = "Ex2per3"
        name = "Extinction_Day2_per3min"
      }
    }
    g <- ggplot(sumEx, aes(x = Time, y = mean,group = Group, fill = Group)) +
      geom_line(linewidth = .8) +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = 0.2) +
      geom_point(size = 5, shape = 21) +
      labs(title = titlename) +
      labs(x = xlabel, y = "Freezing time (%)") +
      scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            # legend.key = element_blank(), 
            legend.title = element_blank(),
            # legend.text = element_text(size = 16),
            axis.text = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 20))
    
    if (time == "per5"){
      g <- g +
        scale_x_discrete(limits = c("5", "10", "15"))
    }
    else if (time == "per3"){
      g <- g +
        scale_x_discrete(limits = c("3", "6", "9", "12", "15"))
    }
    else {
      g <- g +
        scale_x_continuous(limits = c(1, 15), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
        theme(axis.text.x = element_text(size = 11, color = "black"))
    }
    plot(g)
    # ggsave(filename = str_c("Result/",".png", sep = name), width = 3.5, height = 3, dpi = 300)
  }
  else if(graph == "box"){
    dataset <- dataset %>% 
      group_by(Group, No) %>% 
      summarise(Freezing = mean(Freezing))
    if(day == 1){
      titlename = "Extinction Day 1"
      graphname = "Ex1box"
    }
    else if (day == 2){
      titlename = "Extinction Day 2"
      graphname = "Ex2box"
    } 
    g <- ggplot(data = dataset, aes(x = Group, y = Freezing, fill = Group)) +
      stat_boxplot(geom = "errorbar", width = .2) +
      geom_boxplot(color = "black", width = .7, outlier.colour = NA) +
      stat_summary(fun = "mean", geom = "point", shape = 23, size = 4, fill = "white") +
      geom_jitter(data = dataset, aes(x = Group, y = Freezing),
                  height = 0, width = 0.1, size = 2.5, alpha = 0.7,
                  fill = "white", color = "black", shape = 21) +
      labs(title = titlename,x = "", y = "Freezing Time (%)") +
      scale_y_continuous(expand = c(0, 0),limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_discrete(limits = c("SED", "LIE", "MOE")) +
      scale_fill_manual(values = c(SED = SED_color, LIE = LIE_color, MOE = MOE_color)) +
      theme_classic(base_family = "TNR") +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.text = element_text(size = 20, colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20)) 
    
    plot(g)
    
  }
  assign(str_c("G", "", sep = graphname), g, envir = .GlobalEnv)
}
