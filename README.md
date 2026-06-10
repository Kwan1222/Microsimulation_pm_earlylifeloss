# Microsimulation of Early-Life Losses Attributable to Air Pollution

## Overview

This project presents a micro-simulation model designed to quantify early-life losses (including infertility, stillbirth, and infant mortality) attributable to ambient PM<sub>2.5</sub> pollution exposure. 
The model integrates exposure-response functions (ERFs) derived from three epidemiological studies to assess health impacts across different early-life stages. 
R code and data files are provided to transparently document the entire simulation workflow.

## Data Files

- **Data_for_simulation_withoutID.RData**: Simulation population derived from Demographic and Health Survey (DHS) data, with individual identifiers removed and containing only variables necessary for the micro-simulation analysis.
  
- **Infertrate_ERF_risk_assessment.RData**: Exposure-response function table quantifying the relationship between PM<sub>2.5</sub> concentration and 12-month infertility rates.
  
- **ERFs_4_types_for_stillbirth_and_PM25.RData**: Maternal age-specific exposure-response functions linking PM<sub>2.5</sub> exposure to stillbirth risk.
  
- **ERF_pregnancyexp_infant_mortality.RData**: Exposure-response function table modeling the association between pregnancy-period PM<sub>2.5</sub> exposure and infant mortality.
  
- **Simulation_result_overall.RData**: Aggregated micro-simulation results and key findings from the analysis.

## Code Structure

This repository includes R scripts for:

- Running the complete micro-simulation process
- Processing and organizing simulation outputs
- Generating publication-ready figures and summary statistics


## Author

**Mingkun Tong**  
PKU-IIASA International Postdoctoral Fellow  
Email: tongmk@pku.edu.cn

---

*Last updated: June 10, 2026*