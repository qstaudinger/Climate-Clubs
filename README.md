# Nordhaus Climate Clubs – Replication

Replication of  
**Nordhaus (2015), “Climate Clubs: Overcoming Free-riding in International Climate Policy”**  
with an additional update using 2023 data.

Main scripts:
- `01_Preprocessing.R` – data preparation
- `02_Replication.R` – replication and scenario runs

---

## Requirements

- R ≥ 4.2
- R packages:
  - `dplyr`, `tidyr`, `stringr`, `purrr`
  - `readr`, `readxl`, `writexl`
  - `ggplot2`

Install in R:

```r
install.packages(c(
  "dplyr","tidyr","stringr","purrr",
  "readr","readxl","writexl","ggplot2"
))
```

## Data inputs

Place the following files under Data/Rawdata/:
- `TradeData.xlsx`
- `GHG.csv` (EDGAR)
- `Population.xls` (World Bank)
- `GDP.xls` (World Bank)
Also place the original Nordhaus dataset as:
- `Data/Input_Original.xlsx`
Running the preprocessing script will generate:
- `Data/Input_Updated.xlsx` (2023 update)


What the scripts do
01_Preprocessing.R
- Cleans trade data and constructs a 15-region export matrix (million USD).
- Aggregates GHG, population (millions), and GDP (billion USD).
- Maps countries into 15 regions:
  - Brazil, Japan, EU, SSA, Canada, US, LatAm, ROW, SEAsia, Mideast, Russia, India, Safrica, China, Eurasia
- Adds McKinsey/Nordhaus alpha values.
- Writes final dataset `Data/Input_Updated.xlsx`.

02_Replication.R
- Loads `Input_Original.xlsx` and `Input_Updated.xlsx`.
- Adds alpha/beta parameters and computes optimal tariffs.
- Runs run_scenario() across:
  - Tariff rates: 0 … 0.10 (step 0.01)
  - SCC: 12.5, 25, 50, 100 USD/tCO₂
  - 20 iterations per combination
- Outputs:
  - Participation by region
  - Global average carbon price
  - Net economic gains

 ## Outputs

**Excel tables**
- `Results/results_original.xlsx`
- `Results/results_updated.xlsx`
  
**Figures**
- `Graphs/number_regions_original.png`
- `Graphs/carbon_price_original.png`
- `Graphs/net_gain_original.png`
- `Graphs/number_regions_updated.png`
- `Graphs/carbon_price_updated.png`
- `Graphs/net_gain_updated.png`

## Citation
Nordhaus, W. D. (2015).

Climate Clubs: Overcoming Free-riding in International Climate Policy.

American Economic Review, 105(4), 1339–1370.

https://doi.org/10.1257/aer.15000001
