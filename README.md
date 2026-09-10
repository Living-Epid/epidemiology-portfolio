# Living-Epid | Epidemiology Portfolio

Welcome to my professional Epidemiology Portfolio.

This repository documents my journey from epidemiology fundamentals to advanced public health analytics through reproducible analyses, real-world case studies, and applied statistical methods.

## Repository Structure

- 📘 Foundations
- 📊 Biostatistics
- 🦠 Epidemiology
- 🌍 Public Health
- 🗺️ Geospatial Analytics
- 📈 Shiny Dashboards
- 💻 R Toolbox
- 🧪 Case Studies
- 🎓 Capstone Projects

## Tools

- R
- RStudio
- Shiny
- ArcGIS
- QGIS
- SQL
- Git & GitHub

## 📈 Shiny Dashboards (03_Epi Dashboards)

Six interactive R Shiny dashboards built as an applied epidemiology / field
epidemiology (FETP) training series, following WHO/CDC surveillance and
outbreak investigation conventions. All use simulated data.

| # | Project | Live Demo | Focus |
|---|---------|-----------|-------|
| 1 | Cholera Outbreak Line List Explorer | [Live](https://taiwoadegbola-cholera-outbreak-dashboard.share.connect.posit.cloud) | Descriptive epi, reactive filtering, attack rates |
| 2 | Measles Epi Curve & Time Series Dashboard | [Live](https://taiwoadegbola-measles-outbreak-surveillance-dashboard.share.connect.posit.cloud) | MMWR epi weeks, rolling averages, vaccine effectiveness |
| 3 | Cholera Surveillance Map | [Live](https://taiwoadegbola-cholera-surveillance-map.share.connect.posit.cloud) | Spatial epi, leaflet clustering, choropleth attack rate |
| 4 | Outbreak Investigation: 2x2 Analysis | [Live](https://taiwoadegbola-gastroenteritis-2x2-analysis.share.connect.posit.cloud) | Odds ratio/relative risk, case-control analytics |
| 5 | Real-Time AWD Surveillance Dashboard | *(not currently deployed — see note below)* | SQLite database, `reactivePoll()` auto-refresh, alerting |
| 6 | Typhoid Outbreak Situation Report (Capstone) | [Live](https://taiwoadegbola-typhoid-outbreak-situation-report.share.connect.posit.cloud) | Full sitrep integrating descriptive, spatial, and analytic epi |

**Note on Project 5:** its core feature — live database auto-refresh via
`reactivePoll()` — can't be meaningfully demonstrated through a static public
deployment, since triggering new case reports requires running a separate
script (`simulate_new_reports.R`) alongside the live app. The full working
code, including a tested live-update demonstration, is available in its
project folder. It was unpublished from the hosting platform's free tier to
make room for the capstone, but remains fully functional locally.

**Dashboard tech stack:** bslib, plotly, leaflet, DT/reactable, gt, epitools,
DBI/RSQLite, lubridate/MMWRweek, dplyr

## Mission

To build a reproducible portfolio demonstrating competence in epidemiology, biostatistics, surveillance, spatial epidemiology, and public health analytics through real-world projects.

---

**Author:** Taiwo Adegbola

GitHub: https://github.com/Living-Epid