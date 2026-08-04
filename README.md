## Accelerated Fear Extinction by Regular Light-Intensity Exercise: A Possible Role of Hippocampal BDNF-TrkB Signaling

This repository contains the R code and related materials for the data analysis presented in the following paper:

> RYO SHIMODA; YUKI AMAYA; MASAHIRO OKAMOTO; SHINGO SOYA; MARIKO SOYA; HIKARU KOIZUMI; KENGO NAKAMURA; TAICHI HIRAGA; FERENC TORMA; HIDEAKI SOYA (2024).
> Accelerated Fear Extinction by Regular Light-Intensity Exercise: A Possible Role of Hippocampal BDNF-TrkB Signaling.
> Medicine & Science in Sports & Exercise, 56(2):p 221-229, February 2024. DOI: 10.1249/mss.0000000000003312

---

## Repository Structure
```
├── README.md          # This file
├── scripts/           # R scripts for analysis and function text files.
│   ├── Analyzing for MSSE.R
│   ├── MyFunctions.R
│   ├── anovakun_489.txt
│   ├── AUTOGRAPH.txt
│   └── MyFunctions.txt
├── Results/           # Directory for generated figures and analysing text files.
│   ├── Plots/
│   │   ├── ANA_BDNF_box.tiff
│   │   └── [15 more .tiff files]
│   ├── ANA_Contextual_Fear_Conditioning_Analysis.txt
│   └── [9 more .text files]
└── Supplemental/      # Directory for generated figures and analysing text files in Supplemental experiments.
    └── Results/
        ├── Plots/
        │    ├── MONO/
        │        ├── Extinction Day 1 barplotSM.png
        │        └── [15 more .png files]
        ├── Supplemental_Contextual_Fear_Conditioning_Analysis.txt
        ├── Supplemental_Contextual_Fear_Extinction_Day1_Analysis.txt
        └── Supplemental_Contextual_Fear_Extinction_Day2_Analysis.txt
```

##

## System Requirements & Prerequisites

The analysis was developed and tested under the following environment:

- **OS:** Windows 11
- **R Version:** 4.6.1
- **RStudio Version:** 2026.07.1

## Required R Packages

```R
install.packages(c("dplyr", "ggplot2", "tidyr", "stringr", "psych", "readxl", "exactRankTests", "car", "effsize"))
```

## Contact
For any questions regarding the paper, data analysis, or to report a bug, please open an Issue in this repository or contact:

- Contact Person: Ryo Shimoda
- Email: `ryo.shimoda.tech@gmail.com`
