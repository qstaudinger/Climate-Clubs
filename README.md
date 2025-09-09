# Climate Clubs in Retrospect and Prospect: Evidence from Updated Data

This project replicates the results from
Nordhaus (2015): "Climate Clubs: Overcoming Free-riding in International Climate Policy"
and extends them with an updated dataset for the year 2023.

There are two main scripts:

01_Preprocessing.R – data preparation

02_Replication.R – replication and scenario runs

1. 01_Preprocessing.R
Purpose

This script prepares trade, emissions, population, and GDP data and aggregates them into 15 world regions. The output is a consolidated dataset used for modeling.

Steps

a) Trade data

Import from TradeData.xlsx.

Remove aggregate or invalid codes.

Map countries to 15 regions (e.g. EU, China, LatAm, SSA).

Construct a bilateral export matrix between regions (in million USD).

b) GHG emissions

Import from GHG.csv (EDGAR).

Aggregate to the 15 regions.

c) Population data

Import from Population.xls (World Bank).

Convert to millions of people, aggregate by region.

d) GDP data

Import from GDP.xls (World Bank).

Convert to billions of USD, aggregate by region.

e) Merge

Combine trade, population, GDP, and emissions for each region.

Add McKinsey/Nordhaus alpha values.

Export the final dataset: Data/Input_Updated.xlsx.

2. 02_Replication.R
Purpose

This script runs the actual replication exercise. It compares the original Nordhaus dataset with the updated 2023 dataset from step 1.

Steps

a) Data preparation

Load Input_Original.xlsx and Input_Updated.xlsx.

Compute unscaled SCC weights, initialize random start values for membership and welfare.

Add alpha and beta parameters from Nordhaus.

Compute optimal tariffs for each region.

b) Scenario function (run_scenario)

Models the dynamics of climate club formation over multiple iterations.

Key steps:

Test membership decisions.

Compute tariff rates, terms of trade, welfare and efficiency gains.

Account for trade, mitigation, damages, and abatement costs.

Compare new vs. baseline allocation (Pareto test).

Output: final membership status, welfare, global CO₂ price, total gains.

c) Scenario runs

Parameters:

Tariff rates from 0 to 0.1 (step size 0.01).

SCC = 12.5, 25, 50, 100 USD/tCO₂.

Simulations run for both datasets (Original and Updated, 20 iterations each).

Results exported as Excel files:

Results/results_original.xlsx

Results/results_updated.xlsx

d) Graphs

Number of participating regions by SCC and tariff rate.

Global average carbon price.

Net economic gains.

Figures exported to Graphs/.

3. Directory structure
Data/
  Rawdata/               # Raw data (TradeData.xlsx, GHG.csv, Population.xls, GDP.xls)
  Input_Original.xlsx    # Original Nordhaus dataset
  Input_Updated.xlsx     # Updated dataset (2023)
Results/
  results_original.xlsx  # Simulation results with original data
  results_updated.xlsx   # Simulation results with updated data
Graphs/
  *.png                  # Figures from the simulations

4. Notes

Both scripts require dplyr, tidyr, readxl, writexl, ggplot2 and related packages.

Input files must be placed in the Data/ directory.

The preprocessing script must be run before the replication script to generate Input_Updated.xlsx.

Results depend on the random initialization of membership status (reproducibility only with a fixed random seed).
