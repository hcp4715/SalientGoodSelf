# Notebook for perceptual salience of positive-self

Authors: Hu Chuan-Peng, Kaiping Peng, Jie Sui

## Purpose

This repository is an on-going notebook that analyze the results from a series of experiments on perceptual salience of positive self.

The scripts were made public to increasing the transparency and reproducibility of this study.

The cleaned data will be uploaded after I submitting this manuscript.

## Links:

OSF: <https://osf.io/83dyj/>

## Scripts & their functions:

This repo included scripts from raw data pre-processing to manuscript generating.

### Folder structure

```         
root_dir
│   README.md
│   Initial.r
│   Initial_suppl_simple.r
│   Load_save_data.r
│   AllData.RData
│   Data4manu.RData
│   Exp_info_all.csv
│   general_method.rmd
│   Notebook_Pos_Self_Salience_DDM_APA.rmd
│   Notebook_Pos_Self_Salience_DDM_APA.pdf
│   Suppl_Materials_individual_Exp.rmd
│   Suppl_Materials_individual_Exp.pdf
│   endnote.bib
│   r-reference.bib
│
└───exp1a
│   │   rawdata_behav_exp1a_201404_export_2019.csv
│   │   rawdata_behav_exp1a_201704_export_2019.csv
│   
└───exp1b
│   │   rawdata_behav_exp1b_201410_export_2019.csv
│   │   rawdata_behav_exp1b_201705_export_2019.csv
│
└───exp1c
│   │   rawdata_behav_exp1c_export_2019.csv
│
└───exp2
│   │   rawdata_behav_exp2_201405_export_2019.csv
│
└───exp3a
│   │   rawdata_behav_exp3a_2014_export_2019.csv
│
└───exp3b
│   │   rawdata_behav_exp3b_201704_export_2019.csv
│
└───exp4a
│   │   rawdata_behav_exp4a_2015_export_2019.csv
│   │   rawdata_behav_exp4a_2017_export_2019.csv
│
└───exp4b
│   │   rawdata_behav_exp4b_2015_export_2019.csv
│   │   rawdata_behav_exp4b_2017_export_2019.csv
│
└───exp5_specificity
│   │   rawdata_behav_exp5_2016_export_2019.csv
│
└───exp6a_erp1
│   │   rawdata_ERP_exp6a_201412_export_2019.csv
│
└───exp6b_erp2
│   │   rawdata_erp_exp6b_d1_2016_export_2019.csv
│
└───exp7 
│   │   rawdata_behav_exp7a_2016.csv  # data from Hu et al 2020, collabra: psychology
│   │   rawdata_behav_exp7b_2018.csv  # data from Hu et al 2020, collabra: psychology
│
└───Scale_data
    │   FADGS_dataset4_1_clean.csv    # data from Liu et al 2020, J Open Psych Data
```

### Key scripts

`Initial.r`: The r script for initialization, using in `Notebook_Pos_Self_Salience_DDM_APA.rmd`.

`Initial_suppl_simple`: The r script for initialization, using in `Suppl_Materials_individual_Exp.rmd`.

`Load_save_data.r`: I used this script to read raw data (`.csv` files) of each experiment.

`Notebook_Pos_Self_Salience_DDM_APA.rmd` is the main r markdown file that include the latest analysis and results.

`Notebook_Pos_Self_Salience_DDM_APA.pdf` is the output from the above-mentioned r markdown file that presenting the latest results.

`Suppl_Materials_individual_Exp.rmd` is the supplementary RMarkdown file for individual studies.

`Suppl_Materials_individual_Exp.pdf` is the output of the above supplementary RMarkdown file.
