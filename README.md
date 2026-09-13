## 概要
このリポジトリには、国際学術誌Medicine & Science in Sports & Exerciseに掲載された論文のデータ分析コードと結果が格納されています。  
この研究は、4週間の低強度運動が恐怖記憶の消去学習を促進するメカニズムに、海馬の脳由来神経栄養因子（BDNF）が関与していることを示唆しています。心的外傷後ストレス障害（PTSD）への運動療法の可能性や、そのメカニズム解明に迫った研究です。  
実験データは等分散性の検定（Levene検定）や球面性検定（Mendoza）を行った後、繰り返しのある二元配置分散分析や繰り返しのない一元配置分散分析を行い、Shafferの方法を用いて事後検定を行いました。

- 研究概要  
まず、ラットに軽微な電気ショックによる恐怖条件付けを行い、恐怖記憶（トラウマ記憶）を学習させます。その後、安静群、低強度運動群、中強度運動群に分け、４週間の運動、あるいは安静を実施したのち、恐怖記憶の消去学習（恐怖症状を緩和していく実験、PTSDの治療に用いられる暴露療法に類似）を実施すると、安静群に対し、両運動群で消去学習が促進され、恐怖症状が緩和しました。実験を低強度運動群に絞り、BDNF拮抗薬を用いてBDNFの作用を阻害すると、低強度運動の効果が消失したことから、４週間の低強度運動による消去学習促進効果にはBDNFが作用していることが明らかとなりました。


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

- Contact Person: [Ryo Shimoda (ORCID)](https://orcid.org/0000-0002-9123-0703)
- Email: `shimoda.ryo.su@alumni.tsukuba.ac.jp`
