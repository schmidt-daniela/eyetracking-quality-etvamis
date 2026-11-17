# 👁️ Eye-Tracking Data Quality

[![OSF Preregistration](https://img.shields.io/badge/OSF-Preregistration-blue)](https://osf.io/en4cu/overview)

This repository contains the R analyses for reviewing eye-tracking data quality prior to the main data collection of the study: **Visual Attentional Markers Of Understanding Interpersonal Connectedness In Three-Year-Old Toddlers and Non-Human Great Apes (Chimpanzees, Bonobos, and Orangutans)**.

## 📌 Background & Motivation

Specifically, this repository compares eye-tracking data quality when using an individual's own 2-point calibration versus a 5-point calibration obtained from a same-species conspecific (or in few cases, an individual’s own 5-point calibration). This comparison was conducted because Schmidt et al. (in prep.) ([Link to Preregistration](https://osf.io/u3p8d/overview)) found that using a 5-point calibration from a conspecific yields substantially better accuracy than using an individual's own 2-point calibration. 

The R scripts uploaded in this repository evaluate the eye-tracking data quality of the conspecifics (or in few cases own) 5-point calibrations. The accuracy values derived using an individual's 2-point calibration were obtained from previous data collections by our research group. They are not uploaded in this repository.

## 📊 Summary of Results

### Accuracy (2 vs 5 points)

The descriptive comparison showed consistent improvements in accuracy (lower error/distance) across all tested species when using the 5-point calibration.

* **Chimpanzees (Group A):**
  * Mean 2-point: `3.809`
  * Mean 5-point: `3.532`
  * **Average Improvement: `0.278`** (SD: `1.118`)
* **Chimpanzees (Group B):**
  * Mean 2-point: `3.815`
  * Mean 5-point: `3.145`
  * **Average Improvement: `0.670`** (SD: `0.919`)
* **Chimpanzees:**
  * Mean 2-point: `3.811`
  * Mean 5-point: `3.395`
  * **Average Improvement: `0.416`** (SD: `1.040`)
* **Bonobos:**
  * Mean 2-point: `3.155`
  * Mean 5-point: `2.917`
  * **Average Improvement: `0.237`** (SD: `1.223`)
* **Orangutans:**
  * Mean 2-point: `3.365`
  * Mean 5-point: `2.529`
  * **Average Improvement: `0.836`** (SD: `1.845`)

---

### Precision (2 vs 5 points)

#### RMS

The descriptive comparison showed minimal changes in precision (RMS) across the tested species when using the 5-point calibration.

* **Chimpanzees (Group A):**
  * Mean 2-point: `0.500`
  * Mean 5-point: `0.497`
  * **Average Improvement: `0.003`** (SD: `0.399`)
* **Chimpanzees (Group B):**
  * Mean 2-point: `0.436`
  * Mean 5-point: `0.396`
  * **Average Improvement: `0.040`** (SD: `0.385`)
* **Chimpanzees:**
  * Mean 2-point: `0.477`
  * Mean 5-point: `0.461`
  * **Average Improvement: `0.016`** (SD: `0.382`)
* **Bonobos:**
  * Mean 2-point: `0.363`
  * Mean 5-point: `0.429`
  * **Average Improvement: `-0.066`** (SD: `0.220`)
* **Orangutans:**
  * Mean 2-point: `0.217`
  * Mean 5-point: `0.315`
  * **Average Improvement: `-0.097`** (SD: `0.250`)

#### SD

The descriptive comparison showed minimal changes in precision (SD) across the tested species when using the 5-point calibration.

* **Chimpanzees (Group A):**
  * Mean 2-point: `0.321`
  * Mean 5-point: `0.334`
  * **Average Improvement: `-0.013`** (SD: `0.328`)
* **Chimpanzees (Group B):**
  * Mean 2-point: `0.315`
  * Mean 5-point: `0.285`
  * **Average Improvement: `0.030`** (SD: `0.355`)
* **Chimpanzees:**
  * Mean 2-point: `0.318`
  * Mean 5-point: `0.316`
  * **Average Improvement: `0.002`** (SD: `0.327`)
* **Bonobos:**
  * Mean 2-point: `0.249`
  * Mean 5-point: `0.296`
  * **Average Improvement: `-0.047`** (SD: `0.173`)
* **Orangutans:**
  * Mean 2-point: `0.143`
  * Mean 5-point: `0.198`
  * **Average Improvement: `-0.054`** (SD: `0.169`)

---

### Robustness (2 vs 5 points)

Note that here negative values indicate an improvement in robustness and better data quality.

#### MS

The descriptive comparison showed mixed results in robustness (ms) across the tested species when using the 5-point calibration.

* **Chimpanzees (Group A):**
  * Mean 2-point: `364.033 ms`
  * Mean 5-point: `395.749 ms`
  * **Average Improvement: `-31.716 ms`** (SD: `647.284 ms`)
* **Chimpanzees (Group B):**
  * Mean 2-point: `841.617 ms`
  * Mean 5-point: `752.508 ms`
  * **Average Improvement: `89.109 ms`** (SD: `400.971 ms`)
* **Chimpanzees:**
  * Mean 2-point: `491.389 ms`
  * Mean 5-point: `438.361 ms`
  * **Average Improvement: `53.027 ms`** (SD: `589.410 ms`)
* **Bonobos:**
  * Mean 2-point: `917.083 ms`
  * Mean 5-point: `341.836 ms`
  * **Average Improvement: `575.247 ms`** (SD: `970.618 ms`)
* **Orangutans:**
  * Mean 2-point: `698.281 ms`
  * Mean 5-point: `702.499 ms`
  * **Average Improvement: `-4.218 ms`** (SD: `868.977 ms`)

#### PROP

The descriptive comparison showed mixed results in robustness (proportion) across the tested species when using the 5-point calibration.

* **Chimpanzees (Group A):**
  * Mean 2-point: `0.027`
  * Mean 5-point: `0.409`
  * **Average Improvement: `-0.383`** (SD: `0.399`)
* **Chimpanzees (Group B):**
  * Mean 2-point: `0.061`
  * Mean 5-point: `0.026`
  * **Average Improvement: `0.035`** (SD: `0.024`)
* **Chimpanzees:**
  * Mean 2-point: `0.036`
  * Mean 5-point: `0.454`
  * **Average Improvement: `-0.418`** (SD: `0.363`)
* **Bonobos:**
  * Mean 2-point: `0.016`
  * Mean 5-point: `0.006`
  * **Average Improvement: `0.010`** (SD: `0.016`)
* **Orangutans:**
  * Mean 2-point: `0.026`
  * Mean 5-point: `0.026`
  * **Average Improvement: `0.000`** (SD: `0.032`)

## 💡 Conclusion

Across all species, we observed, descriptively, an improvement in accuracy (and no meaningful differences in robustness and precision overall) when utilizing a 5-point calibration from a conspecific compared to the individual's own 2-point calibration. Based on these findings, we decided to use the reviewed 5-point calibrations for the data collection of the main research project.

## 📁 Data Structure & Usage

As the raw data is too large for GitHub, we host it externally on OSF. To run the analysis locally, please download the raw data files from OSF: **[Insert OSF Link to Data Here]**

Once downloaded, place the raw `.tsv` files into the `data/` directory matching the following structure:

```text
et_vamis/
├── data/                    # (Must be created locally / downloaded from OSF)
│   ├── data_apes_5popflakes/
│   │   └── b_chimps
│   │   └── bonobos
│   │   └── bonobos2
│   │   └── orangs
│   ├── data_apes_7popflakes/
│   │   └── bonobos
│   │   └── orangs
│   └── sumdata_apes_5popflakes/
│   │   └── b_chimps
│   │   └── bonobos
│   │   └── bonobos2
│   │   └── orangs
├── img/                     # Plot outputs and generated images
├── R/                       # Helper R scripts and functions
├── et_vamis_popflakes.Rproj
├── apes_5popflakes_dataquality.R # Popflakes presented at 5 positions before data collection in apes
├── apes_7popflakes_dataquality.R # Popflakes presented at the 7 AOI positions during data collection in apes
├── human_7popflakes_dataquality.R # Popflakes presented at the 7 AOI positions during data collection in humans
└── README.md
