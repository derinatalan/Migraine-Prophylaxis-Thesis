# Treatments of Migraine Prophylaxis
This repository contains the screening documents, data files, and analysis scripts of the Bachelor thesis: "Optimizing Migraine Treatment Through Non-invasive Brain Stimulation: A Systematic Review of NIBS Applications and Pharmaceuticals"

## Abstract
Migraine is a common disabling disorder, and the need for effective prophylactic treatment is unmet, with low efficacy, tolerability, and accessibility. Current preventive pharmaceuticals often result in poor adherence due to intolerable side effects and high costs. Neuromodulation has recently emerged as an alternative, but its efficacy has not been assessed against pharmaceuticals. This review comparatively assessed the efficacy of pharmaceuticals and neuromodulation in migraine prophylaxis.

Randomised controlled trials between 2020-2025 were searched on PubMed, Scopus, Embase, Web of Science, and Cochrane Library. After quality and risk of bias assessment, a random-effects meta-analysis was conducted for pooled effect sizes on migraine frequency (MMD), severity, intensity, acute medication days (AMD), and adverse events (AEs).

35 trials were included, and all interventions significantly reduced MMD. Transcranial direct current stimulation (tDCS) had consistently high reductions in all parameters, particularly multifocal tDCS across MMD and AMD. Within pharmaceuticals, CGRP-mAbs had the highest consistent efficacy with milder AEs. The first-line treatment topiramate, had similar or lower efficacy compared to CGRP-mAbs and neuromodulation. The findings suggest that neuromodulation is as effective, or sometimes more effective, than pharmaceuticals in migraine prophylaxis with fewer AEs. Protocol standardisation, long-term outcomes and larger sample sizes are recommended for future research.

## Repository Structure
### _Screening Documents_
Contains tools and completed forms used during quality screening and risk of bias assessment:

- `Template - CEBM RCT Quality Screening.pdf` – Template for quality assessment (CEBM).
- `Quality_Screening_Data.csv` – Final quality assessment data.
- `Template - Risk of Bias Tool.docx` – Template for Cochrane RoB2 tool.
- `RoB_data.csv` – Final risk of bias assessments for all included studies.

### _Data Files_
Contains datasets used in the meta-analysis:
- `raw_data.csv` – Raw data from Covidence.
- `mmd_data.csv` – Monthly Migraine Days (MMD).
- `amd_data.csv` – Acute Medication Days (AMD).
- `mhd_data.csv` – Monthly Headache Days (MHD).
- `ae_data.csv` – Adverse Events.
- `response_data.csv` – Response rates (>50% reduction in MMD).
- `protocol_data.csv` – Metadata on study design and treatment protocols.

### _Analysis Scripts_
These scripts perform all statistical analyses related to the outcomes:
- `mmd_data_analysis.R` – Analysis of Monthly Migraine Days.
- `amd_data_analysis.R` – Analysis of Acute Medication Days.
- `mhd_data_analysis.R` – Analysis of Monthly Headache Days.
- `response_data_analysis.R` – Analysis of response rates.
- `correlational_data_analysis.R` – Correlation analysis and extra statistical analyses.

  ## Notes
- All scripts are written in **R** and require libraries such as `metafor`, `dplyr`, and `ggplot2`.
- Please note to change the working directory and file locations accordingly.
- This repository supports full transparency and reproducibility of the data analysis pipeline.
- Refer to the paper for methodology, inclusion criteria, and interpretation of results.
- This repository is released under the MIT License. See `LICENSE` file for details.
