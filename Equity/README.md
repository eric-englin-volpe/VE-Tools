# Equity Metrics for VisionEval Models

This repository demonstrates how to generate four possible equity indices at census tract levels, and join them to VisionEval model outputs. The example model used is for a case study in northern Virginia; data files for that case study are not provided in this repository. However, the methods used can be applied to any VisionEval model with explicit geographies for Bzones, namely where Bzones can be mapped to combinations or subsets of census tracts.

1. The geographies used in this case study are Traffic Analysis Zones (TAZs). Spatial matching is done in `download_Census_data.R` to align census tracts with the Bzone geographies used. The Census script relies on users having signed up for a free API key.

2. Each equity measure is calculated in a separate R script. Currently the four used are `PBOT_EquityMatrix.R`, `Vtrans_EEA.R`, `MWCOG_EEA.R`, `CDC_SVI.R`. The latter does not rely on the downloaded raw data from Census, but rather uses the CDC-generated equity theme indices which have already been pre-calculated at a census tract level. Each script produces a stand-alone table at the Bzone level and also produces figures for visualization of the results.

3. Model runs are assumed to have been previosuly completed. `VERSPM_Scenarios_Extract_For_Equity.R` extracts household and Marea level metrics to join with the equity values. 

4. Finally, the values from VERSPM runs and the equity indices are joined and analyzed in `VERSPM_Equity_Analysis.R`.

