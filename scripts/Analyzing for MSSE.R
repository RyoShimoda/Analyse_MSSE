# Analyzing for MSSE

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(psych)
library(readxl)
library(exactRankTests)
# For Levene Test
library(car)
library(effsize)

# Windows Font
windowsFonts("TNR" = windowsFont("Times New Roman"))

# Downloads 
source("scripts/anovakun_489.txt")
source("scripts/AUTOGRAPH.txt")
source("scripts/MyFunctions.txt")

# Experiment 1 =======================================================================================
  # Contextual Fear Conditioning ----
    # Create data sets ----
    mf_dataset(exp = "FC", path = ".\\FC")
    
    # Statistical Analysis ----
    sumFC <- dataFC %>% 
      tidyr::spread(key = Time, value = Freezing) %>% 
      mutate(No = str_replace(No, Group, "")) %>% 
      mutate(No = as.numeric(No)) %>% 
      dplyr::arrange(No)
    
    sink(file = "Results/Contextual_Fear_Conditioning_Analysis.txt", split = T)
    anovakun(dataset = sumFC[-1], "AsB",
             Group = c("SED", "LIE", "MOE"),
             Time = c("1","2","3","4","5","6"),
             hf = T, peta = T)
    
    sink()
    # Line plot ----
    mf_plotsave(dataset = dataFC, graph = "FC", color = "mono")
    gFC <- GFC +
      annotate("segment", x = 3, xend = 3, y = 50, yend = 30, color = "black", size = 1.2, 
               arrow = arrow(length = unit(.4,"cm"))) +
      annotate("segment", x = 4, xend = 4, y = 70, yend = 50, color = "black", size = 1.2, 
               arrow = arrow(length = unit(.4,"cm"))) +
      annotate("segment", x = 5, xend = 5, y = 90, yend = 70, color = "black", size = 1.2, 
               arrow = arrow(length = unit(.4,"cm"))) +
      annotate("segment", x = 4.1, xend = 4.1, y = 15, yend = 3, color = "black", size = 1.2,
               arrow = arrow(length = unit(.25, "cm"))) +
      annotate("text", x = 5.25, y = 10, label = ": Foot shock", size = 5.5, family = "TNR")
    
    gFC
    tiff(filename = "Results/Contextual_Fear_Conditioning.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
    gFC
    dev.off()
    
  # Contextual Fear Extinction ----
    # Extinction day 1 ----
      # Create data sets ----
      explist_day1 <- list("Ex1", "Ex1per3", "Ex1jitter")
      for (j in explist_day1) {
        mf_dataset(exp = j, path = ".\\Ex1")
      }

      # Statistical Analysis ----
      sumEx1per3 <- dataEx1per3 %>% 
        tidyr::spread(key = Time, value = Freezing) %>% 
        select(1:2, str_sort(names(.)[3:7], numeric = TRUE)) %>%
        mutate(No = str_replace(No, paste0("Ex1", Group, "per3"), "")) %>% 
        mutate(No = as.numeric(No)) %>% 
        dplyr::arrange(No)
      
      sumEx1 <- dataEx1 %>% 
        group_by(No, Group) %>% 
        summarise(Freezing = mean(Freezing)) %>% 
        mutate(No = str_replace(No, Group, "")) %>% 
        mutate(No = as.numeric(No)) %>% 
        dplyr::arrange(No)
      
      
      sink(file = "Results/Contextual_Fear_Extinction_day1_Analysis.txt", split = T)
      anovakun(dataset = sumEx1per3[-1],
               design = "AsB",
               Group = c("SED", "LIE", "MOE"),
               Time = c("3", "6", "9", "12", "15"),
               hf = T,
               peta = T)
      
      # Shapiro-Wilk test
      cat("\n== Shapiro-Wilk Test for Boxplot ==\n")
      shapiro.test(sumEx1$Freezing)

      # Levene's test
      cat("\n== Levene's Test for Boxplot ==\n")
      leveneTest(Freezing ~ Group, data = sumEx1)
      
      # ANOVA
      cat("\n== ANOVA == \n")
      anovakun(dataset = sumEx1[-1],
               design = "As",
               Group = c("SED", "LIE", "MOE"),
               peta = T)
      sink()

      # Line plot ----
      mf_plotsave(dataEx1per3, day = 1, graph = "line", time = "per3", color = "mono")
      
      gEx1per3 <- GEx1per3 + 
        annotate("text", x = 3, y = 44, label = "$", size = 4) +
        annotate("text", x = 3, y = 35, label = "#", size = 4) +
        annotate("text", x = 4, y = 29, label = "$", size = 4) +
        annotate("text", x = 4, y = 20, label = "#", size = 4) +
        annotate("text", x = 5, y = 19, label = "$", size = 4) +
        annotate("text", x = 5, y = 10, label = "#", size = 4)
      gEx1per3
      tiff(filename = "Results/Contextual_Extinction_Day1_per3min.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
      gEx1per3
      dev.off()

      # Box plot ----
      mf_plotsave(dataset = dataEx1, day = 1, graph = "box", color = "mono")
      gEx1box <- GEx1box +
        scale_y_continuous(expand = c(0,0), limits = c(0,128), breaks = seq(0, 100, by = 20)) +
        geom_segment(aes(x = 0.4, xend = 0.4, y = 0, yend = 100), color = "black", linewidth = 1) +
        annotate("path", x = c(1,1,2,2), y = c(102, 106, 106, 100)) +
        annotate("text", x = 1.5, y = 108, label = "*", size = 10) +
        annotate("path", x = c(1,1,3,3), y = c(113, 117, 117, 100)) +
        annotate("text", x = 2, y = 119, label = "*", size = 10) +
        theme(axis.line.y = element_blank())
      
      gEx1box
      tiff(filename = "Results/Contectual_Extinction_Day1_box.tiff", width = 4 * 900, height = 4 * 900, res = 900)
      gEx1box
      dev.off()

    # Extinction day 2 ----
      # Create data sets ----
      explist_day2 <- list("Ex2", "Ex2per3", "Ex2jitter")
      for (k in explist_day2) {
        mf_dataset(exp = k, path = ".\\Ex2")
      }
      # Statistical Analysis ----
      sumEx2per3 <- dataEx2per3 %>% 
        spread(key = Time, value = Freezing) %>% 
        select(1:2, str_sort(names(.)[3:7], numeric = TRUE)) %>%
        mutate(No = str_replace(No, paste0("Ex2", Group, "per3"), "")) %>% 
        mutate(No = as.numeric(No)) %>% 
        dplyr::arrange(No)
      
      sumEx2 <- dataEx2 %>% 
        group_by(No, Group) %>% 
        summarise(Freezing = mean(Freezing)) %>% 
        mutate(No = str_replace(No, Group, "")) %>% 
        mutate(No = as.numeric(No)) %>% 
        dplyr::arrange(No)
      
      sink(file = "Results/Contextual_Extinction_day2_Analisis.txt", split = T)
      anovakun(dataset = sumEx2per3[-1],
               design = "AsB",
               Group = c("SED", "LIE", "MOE"),
               Time = c("3", "6", "9", "12", "15"),
               peta = T)
      
      # Shapiro-Wilk test
      cat("\n== Shapiro-Wilk Test for Boxplot ==\n")
      shapiro.test(sumEx2$Freezing)
      
      # Levene's test
      cat("\n== Levene's Test for Boxplot ==\n")
      leveneTest(Freezing ~ Group, data = sumEx2)
      
      # ANOVA
      cat("\n== ANOVA ==\n")
      anovakun(dataset = sumEx2[-1],
               design = "As",
               Group = c("SED", "LIE", "MOE"),
               peta = T)
      
      sink()

      # Line plot ----
      mf_plotsave(dataEx2per3, day = 2, time = "per3", graph = "line", color = "mono")
      
      gEx2per3 <- GEx2per3 +
        annotate("text", x = 1, y = 14, label = "$" ,size = 4) +
        annotate("text", x = 1, y = 5, label = "#", size = 4) +
        annotate("text", x = 2, y = 20, label = "$" ,size = 4) +
        annotate("text", x = 2, y = 11, label = "#", size = 4) +
        annotate("text", x = 3, y = 21, label = "$" ,size = 4) +
        annotate("text", x = 3, y = 12, label = "#", size = 4)
      gEx2per3
      
      tiff(filename = "Results/Contextual_Extinction_Day2_per3.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
      gEx2per3
      dev.off()
      
      # Box plot ----
      mf_plotsave(dataEx2, day = 2, graph = "box", color = "mono")
      
      gEx2box <- GEx2box +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 128), breaks = seq(0,100, by = 20)) +
        geom_segment(aes(x = 0.4, xend = 0.4, y = 0, yend = 100), color = "black", linewidth = 1) +
        annotate("path", x = c(1,1,2,2), y = c(100, 106, 106, 80)) +
        annotate("text", x = 1.5, y = 108, label = "*", size = 10) +
        annotate("path", x = c(1,1,3,3), y = c(111, 117, 117, 80)) +
        annotate("text", x = 2, y = 119, label = "*", size = 10) +
        theme(axis.line.y = element_blank())
      gEx2box
      
      tiff(filename = "Results/Contextual_Fear_Extinction_Day2_box.tiff", width = 4 * 900, height = 4 * 900, res = 900)
      gEx2box
      dev.off()
      
# Experiment 2 ========================================================================================
  # Auditory Fear Conditioning ----
    # Create data sets ----
      cueFc <- read.csv("cue_FC.csv") %>% 
        gather(key = Group, val = Freezing, -1) %>% 
        mutate(Group = rep(c(1, 2, 3), each = 32)) %>%
        mutate(Group = dplyr::recode(Group, "1" = "SED", "2" = "LIE", "3" = "MOE")) %>%
        drop_na(Freezing) %>% 
        rename("Tone" = 1)
      
    # Statistical Analysis ----
      sumcueFC <- cueFc %>% 
        mutate(No = rep(c(1:23), each = 4)) %>% 
        tidyr::spread(key = Tone, value = Freezing) %>% 
        dplyr::arrange(No) %>% 
        dplyr::select(1,6,3,4,5)
      
      sink(file = "Results/Auditory_Fear_Conditioning_Analysis.txt", split = T)
      anovakun(dataset = sumcueFC,
               design = "AsB",
               Group = c("SED", "LIE", "MOE"),
               Tone = c("pre", "1", "2", "3"),
               hf = T,
               peta = T)
      
      sink()
      
    # Line plot ----
      mf_plotcue(data = cueFc, day = "FC", color = "mono")
      
      tiff(filename = "Results/Auditory_Fear_Conditioning.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
      GcueFCcue
      dev.off()
      
      
  # Auditory Fear Extinction ----
    # Extinction day 1 ----
      # Create data sets ----
        cueEx1 <- read.csv("cue_Ex1.csv") %>% 
          gather(key = Group, val = Freezing, na.rm = TRUE) %>% 
          mutate(Group = dplyr::recode(Group, "MIE" = "LIE"))
      
        cueEx1_per3 <- read.csv("cue_Ex1_tone.csv") %>%
          dplyr::slice(-1) %>% 
          gather(key = Group, val = Freezing, -1, na.rm = TRUE) %>% 
          mutate(row = row_number()) %>% 
          mutate(Group = if_else(row <= 96, true = "SED", false = if_else(row <= 180, true = "LIE", false = "MOE"))) %>% 
          dplyr::select(2,1,3) %>% 
          rename("Tone" = "tone")
        
      # Statistical Analysis ----
        sumcueEx1per3 <- cueEx1_per3 %>% 
          mutate(Tone = rep(c(3,6,9,12), each = 3, times = 23)) %>%
          mutate(No = rep(c(1:23), each = 12)) %>% 
          group_by(No,Tone,Group) %>% 
          summarise(Freezing = mean(Freezing)) %>% 
          spread(key = Tone, value = Freezing) 
        
        sink(file = "Results/Auditory_Fear_Extinction_Day1_Analysis.txt", split = T)
        anovakun(dataset = sumcueEx1per3[-1],
                 design = "AsB",
                 Group = c("SED", "LIE", "MOE"),
                 Tone = c("3", "6", "9", "12"),
                 hf  =T,
                 peta = T)
        
        # Shapiro-Wilk test
        cat("\n== Shapiro-Wilk Test for Boxplot ==\n")
        shapiro.test(cueEx1$Freezing)
        
        # Kruskal-Wallis test
        cat("\n== Kruskal-Wallis Test for Boxplot ==\n")
        kruskal.test(cueEx1$Freezing ~ cueEx1$Group)
        
        sink()
        
      # Line plot ----
        mf_plotcue(data = cueEx1_per3, day = 1, per = "3tone", color = "mono")
        
        modGcueEx1per3 <- GcueEx1per3cue +
          theme(axis.line.y = element_blank()) +
          annotate("segment", x = .3, xend = .3, y = 0, yend = 100.3, size = 1 ) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,105), breaks = seq(0,100, by = 20))
        modGcueEx1per3
        
        tiff(filename = "Results/Auditory_Extinction_Day1_per3tone.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
        modGcueEx1per3
        dev.off()
        
      # Box plot ----
        mf_plotcue(data = cueEx1, day = 1, graph = "box", color = "mono")
        
        modGcueEx1 <- GEx1boxcue +
          theme(axis.line.y = element_blank()) +
          annotate("segment", x = .3, xend = .3, y = 0, yend = 100.3, size = 1 ) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,105), breaks = seq(0,100, by = 20))
        modGcueEx1
        
        tiff(filename = "Results/Auditory_Extinction_Day1_boxplot.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
        modGcueEx1
        dev.off()
        
    # Extinction Day 2 ----
      # Create data sets ----
        cueEx2 <- read.csv("cue_Ex2.csv") %>% 
          gather(key = Group, val = Freezing, na.rm = TRUE) %>% 
          mutate(Group = dplyr::recode(Group, "MIE" = "LIE"))
        
        cueEx2_per3 <- read.csv("cue_Ex2_tone.csv") %>%
          dplyr::slice(-1) %>% 
          gather(key = Group, val = Freezing, -1, na.rm = TRUE) %>% 
          mutate(row = row_number()) %>% 
          mutate(Group = if_else(row <= 96, true = "SED", false = if_else(row <= 180, true = "LIE", false = "MOE"))) %>% 
          dplyr::select(2,1,3) %>% 
          rename("Tone" = "tone")
        
      # Statistical Analysis ----
        sumcueEx2per3 <- cueEx2_per3 %>% 
          mutate(Tone = rep(c(3,6,9,12), each = 3, times = 23)) %>%
          mutate(No = rep(c(1:23), each = 12)) %>% 
          group_by(No,Tone,Group) %>% 
          summarise(Freezing = mean(Freezing)) %>% 
          spread(key = Tone, value = Freezing) 
        
        sink(file = "Results/Auditory_Fear_Extinction_Day2_Analysis.txt", split = T)
        anovakun(dataset = sumcueEx2per3[-1],
                 design = "AsB",
                 Group = c("SED", "LIE", "MOE"),
                 Tone = c("3", "6", "9", "12"),
                 hf  =T,
                 peta = T)
        
        # Shapiro-Wilk test
        cat("\n== Shapiro-Wilk Test for Boxplot ==\n")
        shapiro.test(cueEx1$Freezing)
      
        # Kruskal-Wallis test
        cat("\n== Kruskal-Wallis Test for Boxplot ==\n")
        kruskal.test(cueEx2$Freezing ~ cueEx2$Group)
        
        sink()
        
      # Line plot ----
        mf_plotcue(data = cueEx2_per3, day = 2, per = "3tone", color = "mono")
        
        modGcueEx2per3 <- GcueEx2per3cue +
          theme(axis.line.y = element_blank()) +
          annotate("segment", x = .3, xend = .3, y = 0, yend = 100.3, size = 1 ) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,105), breaks = seq(0,100, by = 20))
        modGcueEx2per3
        
        tiff(filename = "Results/Auditory_Extinction_Day2_per3tone.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
        modGcueEx2per3
        dev.off()
        
      # Box plot ----
        mf_plotcue(data = cueEx2, day = 2, graph = "box", color = "mono")
        
        modGcueEx2 <- GEx2boxcue +
          theme(axis.line.y = element_blank()) +
          annotate("segment", x = .3, xend = .3, y = 0, yend = 100.3, size = 1 ) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,105), breaks = seq(0,100, by = 20))
        modGcueEx2
        
        tiff(filename = "Results/Auditory_Extinction_Day2_boxplot.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
        modGcueEx2
        dev.off()
        
# Experiment 3 =========================================================================================
  # Contextual Fear Conditioning ----
    # Create data sets ----
        ANAFC <- read.csv("ANA_FC.csv") %>% 
          gather(key = Group, val = Freezing, -1) %>% 
          rename("Time" = 1) %>% 
          mutate(row = row_number()) %>% 
          mutate(Group = if_else(row <= 42, true = "SED_Sal", false = if_else(row <= 84, true = "LIE_Sal", "LIE_ANA"))) %>% 
          dplyr::select(-4)
        
    # Statistical Analysis ----
        sumANAFC <- ANAFC %>% 
          mutate(No = rep(c(1:21), each = 6)) %>% 
          tidyr::spread(key = Time, value = Freezing) %>% 
          dplyr::arrange(No)
        
        sink(file = "ANA_Fear_Conditioning_Analysis.txt", split = T)
        anovakun(dataset = sumANAFC[-2],
                 design = "AsB",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 Time = c("1", "2", "3", "4", "5", "6"),
                 hf = T,
                 peta = T)
        sink()
    # Line plot ----
        mf_plotANA(dataset = ANAFC, graph = "FC", color = "mono")
        
        modGANAFC <- GANAFC +
          annotate("segment", x = 3, xend = 3, y = 45, yend = 30, color = "black", size = 1.2, 
                   arrow = arrow(length = unit(.35,"cm"))) +
          annotate("segment", x = 4, xend = 4, y = 90, yend = 75, color = "black", size = 1.2, 
                   arrow = arrow(length = unit(.35,"cm"))) +
          annotate("segment", x = 5, xend = 5, y = 100, yend = 88, color = "black", size = 1.2, 
                   arrow = arrow(length = unit(.35,"cm"))) +
          annotate("segment", x = 4.25, xend = 4.25, y = 13, yend = 3, color = "black", size = 1,
                   arrow = arrow(length = unit(.25, "cm"))) +
          annotate("text", x = 5.3, y = 8, label = ": Foot shock", size = 4.5, family = "TNR")
        modGANAFC
        
        tiff(filename = "Results/ANA_Contextual_Fear_Conditioning.tiff", width = 4 * 900, height = 3.5 * 900, res = 900)
        modGANAFC
        dev.off()
        
  # Contextual Fear Extinction ----
    # Extinction day 1 ----
      # Create data sets ----
        ANAEx1 <- read.csv("ANA_Ex1.csv") %>%
          gather(key = Group, val = Freezing) %>% 
          mutate(Group = dplyr::recode(Group, "SED...Sal" = "SED_Sal", 
                                              "MIE...Sal" = "LIE_Sal", 
                                              "MIE...ANA" = "LIE_ANA"))
        
        ANAEx1per3 <- read.csv(".\\Datasets\\ANA_Ex1_1min.csv") %>% 
          gather(key = Group, val = Freezing, -1) %>% 
          mutate(row = row_number()) %>% 
          mutate(Group = if_else(row <= 105, "SED_Sal", if_else(row <= 210, "LIE_Sal", "LIE_ANA"))) %>% 
          dplyr::select(-4) %>%
          rename(Time = 1) %>%  
          mutate(Time = rep(c(1:5), each = 3, times = 21))
        
      # Statistical Analysis ----
        sumANAEx1per3 <- ANAEx1per3 %>% 
          mutate(Time = rep(c(3,6,9,12,15),each = 3, times = 21)) %>%
          mutate(No = rep(c(1:21), each = 15)) %>% 
          group_by(Group, Time, No) %>% 
          summarise(Freezing = mean(Freezing)) %>% 
          spread(key = Time, val = Freezing,-1) %>%
          arrange(No)
        
        sink(file = "Results/ANA_Contextual_Fear_Extinction_day1_Analysis.txt", split = T)
        
        cat("\n== For Lineplot ==\n")
        cat("\n== ANOVA ==\n")
        anovakun(dataset = sumANAEx1per3[-2],
                 design = "AsB",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 Time = c("3", "6", "9", "12", "15"),
                 hf = T,
                 peta = T)
        
        cat("\n== For Boxplot ==\n")
        # Levene's test
        cat("\n== Levene's Test for Boxplot ==\n")
        leveneTest(Freezing ~ Group, data = ANAEx1)
        
        # ANOVA
        cat("\n== ANOVA ==\n")
        anovakun(dataset = ANAEx1,
                 design = "As",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 peta = T)
        sink()
        
      # Box plot ----
        mf_plotANA(dataset = ANAEx1, day = 1, graph = "box", color = "mono")
        
        modGANAEx1box <- GANAEx1box +
          scale_y_continuous(expand = c(0,0), limits = c(0,125), breaks = seq(0,100, by  = 20)) +
          geom_segment(aes(x = 0.4, xend = 0.4, y = 0, yend = 100), color = "black", linewidth = 1) +
          annotate("path", x = c(1, 1, 1.9, 1.9), y = c(104, 108, 108, 104)) +
          annotate("path", x = c(2.1, 2.1, 3, 3), y = c(104, 108, 108, 104)) +
          annotate("text", x = 1.45, y = 110, label  = "*", size = 10) +
          annotate("text", x = 2.55, y = 110, label  = "*", size = 10) +
          theme(axis.line.y = element_blank())
        modGANAEx1box    
        
        tiff(filename = "Results/ANA_Contectual_Extinction_Day1_box.tiff", 
             width = 4 * 900, height = 4 * 900, res = 900)
        modGANAEx1box
        dev.off()
        
    # Extinction day 2 ----
      # Create data sets ----
        ANAEx2 <- read.csv("ANA_Ex2.csv") %>%
          gather(key = Group, val = Freezing) %>% 
          mutate(Group = dplyr::recode(Group, "SED...Sal" = "SED_Sal", 
                                       "MIE...Sal" = "LIE_Sal", 
                                       "MIE...ANA" = "LIE_ANA"))
        
        ANAEx2per3 <- read.csv(".\\Datasets\\ANA_Ex2_1min.csv") %>% 
          gather(key = Group, val = Freezing, -1) %>% 
          mutate(row = row_number()) %>% 
          mutate(Group = if_else(row <= 105, "SED_Sal", if_else(row <= 210, "LIE_Sal", "LIE_ANA"))) %>% 
          dplyr::select(-4) %>%
          rename(Time = 1) %>%  
          mutate(Time = rep(c(1:5), each = 3, times = 21))
        
      # Statistical Analysis ----
        sumANAEx2per3 <- ANAEx2per3 %>% 
          mutate(Time = rep(c(3,6,9,12,15),each = 3, times = 21)) %>%
          mutate(No = rep(c(1:21), each = 15)) %>% 
          group_by(Group, Time, No) %>% 
          summarise(Freezing = mean(Freezing)) %>% 
          spread(key = Time, val = Freezing,-1) %>%
          arrange(No)
        
        sink(file = "ANA_Contextual_Fear_Extinction_day2_Analysis.txt", split = T)
        
        cat("\n== For Lineplot ==\n")
        cat("\n== ANOVA ==\n")
        anovakun(dataset = sumANAEx2per3[-2],
                 design = "AsB",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 Time = c("3", "6", "9", "12", "15"),
                 hf = T,
                 peta = T)
        
        cat("\n== For Boxplot ==\n")
        # Levene's test
        cat("\n== Levene's Test for Boxplot ==\n")
        leveneTest(Freezing ~ Group, data = ANAEx2)
        
        # ANOVA
        cat("\n== ANOVA ==\n")
        anovakun(dataset = ANAEx2,
                 design = "As",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 peta = T)
        sink()
      # Box plot ----
        mf_plotANA(dataset = ANAEx2, day = 2, graph = "box", color = "mono")
        
        modGANAEx2box <- GANAEx2box +
          scale_y_continuous(expand = c(0,0), limits = c(0,125), breaks = seq(0,100, by = 20)) +
          geom_segment(aes(x = 0.4, xend = 0.4, y = 0, yend = 100), color = "black", linewidth = 1) +
          annotate("path", x = c(1, 1, 1.9, 1.9), y = c(104, 108, 108, 100)) +
          annotate("text", x = 1.45, y = 110, label  = "*", size = 10) +
          theme(axis.line.y = element_blank())
        modGANAEx2box
        
        tiff(filename = "Results/ANA_Contectual_Extinction_Day2_box.tiff", 
             width = 4 * 900, height = 4 * 900, res = 900)
        modGANAEx2box
        dev.off()
        
# Experiment 4 =========================================================================================
  # Create data sets ----
        ANABDNF <- read.csv("ANA_BDNF.csv") %>% 
          gather(key = Group, val = val)
        
        ANATrkB <- read.csv("ANA_TrkB.csv") %>%
          gather(key = Group, val = val)
        
        ANApCREB <- read.csv("ANA_pCREB.csv") %>% 
          gather(key = Group, val = val)
        
  # Statistical Analysis ----
        sink(file = "Results/Western_Blotting_Analysis.txt", split = T)
        
        # Shapiro-Wilk test
        cat("== Shapiro-Wilk Test for Boxplot ==\n")
        cat("\n-- BDNF --\n")
        shapiro.test(ANABDNF$val)
        
        cat("-- TrkB --\n")
        shapiro.test(ANATrkB$val)
        
        cat("-- pCREB/tCREB --\n")
        shapiro.test(ANApCREB$val)
        
        # Levene's test
        cat("\n== Levene's Test for Boxplot ==\n")
        cat("\n-- BDNF --\n")
        leveneTest(val ~ Group, data = ANABDNF)
        
        cat("\n-- TrkB --\n")
        leveneTest(val ~ Group, data = ANATrkB)
        
        cat("\n-- pCREB/tCREB --\n")
        leveneTest(val ~ Group, data = ANApCREB)
        
        # ANOVA
        cat("\n== ANOVA ==\n")
        cat("\n-- BDNF --\n")
        anovakun(dataset = ANABDNF, 
                 design = "As",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 peta = T)
        
        cat("\n-- TrkB --\n")
        anovakun(dataset = ANATrkB,
                 design = "As",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 peta = T)
        
        cat("\n-- pCREB/tCREB --\n")
        anovakun(dataset = ANApCREB,
                 design = "As",
                 Group = c("SED_Sal", "LIE_Sal", "LIE_ANA"),
                 peta = T)
        sink()
        
  # Box plot ----
        # BDNF
        mf_smpplot(dataset = ANABDNF, titlename = "BDNF",
                   design = "ANA", exp = "ANA", graph = "box",
                   color = "mono")
        
        modGANABDNF <- GBDNFANA +
          scale_y_continuous(expand = c(0, 0), limits = c(0,250)) +
          annotate("path", x = c(1,1,1.9,1.9), y = c(200, 240, 240, 220)) +
          annotate("text", x = 1.45, y = 242, label = "*", size = 10) +
          annotate("path", x = c(2.1,2.1,3,3), y = c(220, 240, 240, 220)) +
          annotate("text", x = 2.55, y = 242, label = "*", size = 10)
        modGANABDNF
        
        tiff(filename = "Results/ANA_BDNF_box.tiff", 
             width = 4 * 900, height = 4 * 900, res = 900)
        modGANABDNF
        dev.off()
        
        # TrkB
        mf_smpplot(dataset = ANATrkB, titlename = "TrkB",
                   design = "ANA", exp = "ANA", graph = "box",
                   color = "mono")
        
        modGANATrkB <- GTrkBANA +
          scale_y_continuous(expand = c(0, 0), limits = c(0,250)) +
          annotate("path", x = c(1,1,1.9,1.9), y = c(200, 240, 240, 220)) +
          annotate("text", x = 1.45, y = 242, label = "*", size = 10) +
          annotate("path", x = c(2.1,2.1,3,3), y = c(220, 240, 240, 220)) +
          annotate("text", x = 2.55, y = 242, label = "*", size = 10)
        modGANATrkB
        
        tiff(filename = "Results/ANA_TrkB_box.tiff", 
             width = 4 * 900, height = 4 * 900, res = 900)
        modGANATrkB
        dev.off()
        
        # pCREB
        mf_smpplot(dataset = ANApCREB, titlename = "pCREB",
                   design = "ANA", exp = "ANA", graph = "box",
                   color = "mono")
        
        modGANApCREB <- GpCREBANA +
          scale_y_continuous(expand = c(0, 0), limits = c(0,250))
        modGANApCREB
        
        tiff(filename = "Results/ANA_pCREB_box.tiff", 
             width = 4 * 900, height = 4 * 900, res = 900)
        modGANApCREB
        dev.off()

# Supplemental Experiment =============================================================================
  # Contextual Fear Conditioning ----
    # Create data sets & Plots ----
          mf_autograph(folder_name = "ANAsedentary", experiment_type = "FC", 
                        path = "./ANAsedentary/FC", color = "mono")
          
          modsupGFC <- GFC + 
            scale_fill_manual(labels = c("SED" = "SED + Vehicle", "ANA-12" = "SED + ANA-12"),
                              values = c(SED = "white", `ANA-12` = "black"))
          modsupGFC
          
          mf_FC_annotation(folder_name = "ANAsedentary", graph = modsupGFC, 
                                y_first_arrow = 20, y_second_arrow = 40, y_third_arrow3 = 70, 
                                legend_posision_y = 75, plot_number = "_MONO", color = "mono", 
                                plot_width = 3.75, plot_height = 3.25) 
          
    # Statistical Analysis ----
          sink(file = "Results/Supplemental_Contextual_Fear_Conditioning_Analysis.txt", split = T)
          anovakun(dataset = sumFC[-1], "AsB",
                   Group = c("SED + ANA-12", "SED + Vehicle"),
                   Time = TimeFC,
                   hf = T,
                   peta = T)
          sink()
          
  # Contextual Fear Extinction ----
    # Extinction day 1 ----
      # Create data sets & Plots ----
          mf_autograph(folder_name = "ANAsedentary", experiment_type = "Ex1", 
                           path = "./ANAsedentary/Ex1", legend_position_Ex = "none",
                           color = "mono", jitter_fill_color = "white", 
                           axis_text_size = 20, axis_title_size = 20,
                           save_lineplot_width = 3.75, save_plot_height = 3.25,
                           save_plot_width = 3.25)
          
          modsupGEx1box <- GEx1box +
            scale_x_discrete(labels = c("SED" = "SED\nVehicle", "ANA-12" = "SED\nANA-12")) +
            scale_fill_manual(values = c(SED = "white", `ANA-12` = "grey30")) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 105), breaks = seq(0,100, by = 20)) +
            theme(axis.text.x = element_text(size = 18),
                  axis.line.y = element_blank()) +
            annotate("segment", x = .4, xend = .4, y = 0, yend = 100.3, size = 1)
          modsupGEx1box
          
          tiff(filename = "Results/Supplement_Contectual_Extinction_Day1_box.tiff", width = 4 * 900, height = 4 * 900, res = 900)
          modsupGEx1box
          dev.off()
          
      # Statistical Analysis ----
          supsumEx1 <- dataEx1 %>% 
            group_by(No,Group) %>% 
            summarise(Freezing = mean(Freezing))
          
          sink(file = "Results/Supplemental_Contextual_Fear_Extinction_Day1_Analysis.txt", split = T)
          anovakun(dataset = sumEx1per3[-1], "AsB",
                   Group = c("SED + ANA-12", "SED + Vehicle"),
                   Time = Timeper3,
                   hf = T,
                   peta = T)
          
          # Shapiro-Wilk test
          cat("\n== Shapiro-Wilk Test for Boxplot ==\n")
          shapiro.test(supsumEx1$Freezing)
          
          # Levene's test
          cat("\n== Levene's Test for Boxplot ==\n")
          leveneTest(Freezing ~ Group, data = supsumEx1)
          
          # ANOVA
          cat("\n== ANOVA == \n")
          anovakun(dataset = supsumEx1[-1],
                   design = "As",
                   Group = c("SED + ANA-12", "SED + Vehicle"),
                   peta = T)
          sink()

    # Extinction day 2 ----
      # Create data sets & Plots ----
          mf_autograph(folder_name = "ANAsedentary", experiment_type = "Ex2", 
                           path = "./ANAsedentary/Ex2", legend_position_Ex = "none",
                           color = "mono", jitter_fill_color = "white",
                           axis_text_size = 20, axis_title_size = 20,
                           save_lineplot_width = 3.75, save_plot_height = 3.25,
                           save_plot_width = 3.25)
          modsupGEx2box <- GEx2box +
            scale_x_discrete(labels = c("SED" = "SED\nVehicle", "ANA-12" = "SED\nANA-12")) +
            scale_fill_manual(values = c(SED = "white", `ANA-12` = "grey30")) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 105), breaks = seq(0, 100, by = 20)) +
            theme(axis.text.x = element_text(size = 18),
                  axis.line.y = element_blank()) +
            annotate("segment", x = .4, xend = .4, y = 0, yend = 100.3, size = 1)
          modsupGEx2box
          
          tiff(filename = "Results/Supplement_Contectual_Extinction_Day2_box.tiff", width = 4 * 900, height = 4 * 900, res = 900)
          modsupGEx2box
          dev.off()
          
      # Statistical Analysis ----
          supsumEx2 <- dataEx2 %>% 
            group_by(No,Group) %>% 
            summarise(Freezing = mean(Freezing))
          
          sink(file = "Results/Supplemental_Contextual_Fear_Extinction_Day2_Analysis.txt", split = T)
          anovakun(dataset = sumEx2per3[-1], "AsB",
                   Group = c("SED + ANA-12", "SED + Vehicle"),
                   Time = Timeper3,
                   peta = T)
          
          # Shapiro-Wilk test
          cat("\n== Shapiro-Wilk Test for Boxplot ==\n")
          shapiro.test(supsumEx2$Freezing)
          
          # Levene's test
          cat("\n== Levene's Test for Boxplot ==\n")
          leveneTest(Freezing ~ Group, data = supsumEx2)
          
          # ANOVA
          cat("\n== ANOVA == \n")
          anovakun(dataset = supsumEx2[-1],
                   design = "As",
                   Group = c("SED + ANA-12", "SED + Vehicle"),
                   peta = T)
          sink()
