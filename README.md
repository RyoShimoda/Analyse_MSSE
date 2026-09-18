## 概要
このリポジトリには、国際学術誌Medicine & Science in Sports & Exerciseに掲載された論文のデータ分析コードと結果が格納されています。  
この研究は、4週間の低強度運動が恐怖記憶の消去学習を促進するメカニズムに、海馬の脳由来神経栄養因子（BDNF）が関与していることを示唆しています。心的外傷後ストレス障害（PTSD）への運動療法の可能性や、そのメカニズム解明に迫った研究です。  
実験データは等分散性の検定（Levene検定）や球面性検定（Mendoza）を行った後、繰り返しのある二元配置分散分析や繰り返しのない一元配置分散分析を行い、Shafferの方法を用いて事後検定を行いました。

- 研究概要（[筑波大学プレスリリース](https://www.tsukuba.ac.jp/journal/medicine-health/20240829140000.html)から抜粋)
ストレスによって誘発される代表的な神経疾患の一つが心的外傷後ストレス障害（PTSD: Post Traumatic Stress Disorder）です。近年、運動がPTSDの予防や治療に有効だとする報告が散見されるようになりました。その神経分子基盤の一つの仮説として、BDNF（脳由来神経栄養因子）があります。BDNFは恐怖記憶の消去に重要な因子とされ、習慣的な運動によって脳内で発言が高まることが知られています。
そこで本研究では、独自に開発した動物用のトレッドミル運動モデルを活用し、習慣的な運動が恐怖記憶の消去に効果的か、そしてその背景としてBDNFの関与があるかを検証しました。
実験ではまず、ラットチャンバー（箱）の中に入れて軽微な電気刺激を与え、恐怖を記憶させました。続いて、ラットを箱から取り出し、低強度の運動トレーニングを実施した後、再びラットを箱の中に入れてその行動を観察し、運動トレーニングを実施していないラットと比較しました。  
ラットは恐怖を覚えていると立ちすくみ行動を示します。最初はどのラットも立ちすくみ行動を示しましたが、習慣的に運動を行ったラットは、徐々に活発に行動するようになりました。このことは、習慣的な運動が恐怖記憶の消去を促進したことを意味します。さらに、低強度運動をしたラットにBDNFの作用を阻害する薬を投与すると、運動の効果は消失したことから、低強度運動による恐怖記憶の消去は、BDNFシグナリングが関与することが分かりました。
以上のことから、強いストレスで形成されるPTSDの精神症状は、後に低強度運動を継続して行い、海馬のBDNF作用が高まることで軽減できる可能性が示唆されました。  
PTSD患者はうつ症状を併発していることが多く、運動継続率が低いことが指摘されています。運動継続性を担保しやすい低強度運動でも恐怖記憶消去に対し有効であるとする本研究の知見は、新たな運動を基盤とした治療・予防プログラムの開発につながる可能性があります。


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
