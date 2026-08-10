# 👁️ Eye-Tracking Data Quality

[![OSF Preregistration](https://img.shields.io/badge/OSF-Preregistration-blue)](https://osf.io/en4cu/overview)

This repository contains the R analyses for reviewing eye-tracking data quality prior to the main data collection of the study: **Visual Attentional Markers Of Understanding Interpersonal Connectedness In Three-Year-Old Toddlers and Non-Human Great Apes (Chimpanzees, Bonobos, and Orangutans)**.

## 📌 Background & Motivation

Specifically, this repository compares the eye-tracking data quality when using an individual's own 2-point calibration versus a 5-point calibration obtained from a same-species conspecific (or in few cases, an individual’s own 5-point calibration). This comparison was conducted because Schmidt et al. (in prep.) ([Link to Preregistration](https://osf.io/u3p8d/overview)) found that using a 5-point calibration from a conspecific yields substantially better accuracy than using an individual's own 2-point calibration. 

The R scripts uploaded in this repository evaluate the eye-tracking data quality of the conspecifics (or in few cases own) 5-point calibrations. The baseline 2-point calibration values used for comparison were obtained from previous data collections by our research group.

## 📊 Summary of Results

The comparison showed an overall improvement in accuracy across all tested species when using the 5-point conspecific calibration. (Note that the higher the value, the greater the offset, that is, the poorer the accuracy.)

* **Orangutans:**
  * Mean 2-point: `3.37°`
  * Mean 5-point: `2.53°`
  * **Average Improvement: 0.84°**

* **Bonobos:**
  * Mean 2-point: `3.15°`
  * Mean 5-point: `2.92°`
  * **Average Improvement: 0.23°**

* **Chimpanzees (Group B):**
  * Mean 2-point: `4.14°`
  * Mean 5-point: `3.14°`
  * **Average Improvement: 1.00°**

## 💡 Conclusion

Across all species, we observed an average improvement in data quality when utilizing a 5-point calibration from a conspecific (or in few cases, an own 5-point calibration) compared to the individual's own 2-point calibration. Based on these findings, we decided to use the reviewed 5-point  calibrations for the data collection of the main research project.

## 📁 Data Structure & Usage

The raw data is too large for GitHub and is hosted externally on OSF. To run the analysis locally, please download the raw data files from OSF: **[Insert OSF Link to Data Here]**

Once downloaded, place the raw `.tsv` files into the `data/` directory matching the following structure:

```text
et_vamis/
├── data/
│   ├── bchimps/
│   │   └── main_data.tsv
│   ├── bonobos2/
│   │   └── main_data.tsv
│   └── orangs/
│       └── main_data.tsv
├── popflake_analysis.R
└── README.md
