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
Data inputs

Place the following files under Data/Rawdata/:

TradeData.xlsx

GHG.csv (EDGAR)

Population.xls (World Bank)

GDP.xls (World Bank)

Also place the original Nordhaus dataset as
Data/Input_Original.xlsx.

Running the preprocessing script will generate
Data/Input_Updated.xlsx (2023 update).

How to run

From project root:

# 1) Build updated input (creates Data/Input_Updated.xlsx)
source("01_Preprocessing.R")

# 2) Run replication and scenarios (writes Results/* and Graphs/*)
source("02_Replication.R")


Or via command line:

Rscript 01_Preprocessing.R
Rscript 02_Replication.R

What the scripts do
01_Preprocessing.R

Cleans trade data and constructs a 15-region export matrix (million USD).

Aggregates GHG, population (millions), and GDP (billion USD).

Maps countries into 15 regions:

Brazil, Japan, EU, SSA, Canada, US,
LatAm, ROW, SEAsia, Mideast, Russia,
India, Safrica, China, Eurasia


Adds McKinsey/Nordhaus alpha values.

Writes final dataset Data/Input_Updated.xlsx.

02_Replication.R

Loads Input_Original.xlsx and Input_Updated.xlsx.

Adds alpha/beta parameters and computes optimal tariffs.

Runs run_scenario() across:

Tariff rates: 0 … 0.1 (step 0.01)

SCC: 12.5, 25, 50, 100 USD/tCO₂

20 iterations per combination.

Outputs:

Participation by region

Global average carbon price

Net economic gains

Outputs

Excel tables

Results/results_original.xlsx

Results/results_updated.xlsx

Figures

Graphs/number_regions_original.png

Graphs/carbon_price_original.png

Graphs/net_gain_original.png

Graphs/number_regions_updated.png

Graphs/carbon_price_updated.png

Graphs/net_gain_updated.png

Directory layout
Data/
  Rawdata/
    TradeData.xlsx
    GHG.csv
    Population.xls
    GDP.xls
  Input_Original.xlsx
  Input_Updated.xlsx
Results/
  results_original.xlsx
  results_updated.xlsx
Graphs/
  *.png
01_Preprocessing.R
02_Replication.R


Add to .gitignore if you don’t want to track outputs:

Data/Input_Updated.xlsx
Results/
Graphs/
.DS_Store

Reproducibility

Results depend on random initialization of membership.
Add a seed at the top of 02_Replication.R:

set.seed(2025)

Known issues & fixes

Parameter call in scenarios
Replace countries = Input$Country with the matching dataset:

# For original data
results_original <- purrr::pmap_dfr(
  parameter,
  ~ run_scenario(tariff = ..1, SCC = ..2,
                 Input = Input_Original,
                 countries = Input_Original$Country,
                 n_iter = 20)
)

# For updated data
results_updated <- purrr::pmap_dfr(
  parameter,
  ~ run_scenario(tariff = ..1, SCC = ..2,
                 Input = Input_Updated,
                 countries = Input_Updated$Country,
                 n_iter = 20)
)


Plot object names
Use valid R names (no .png in object names):

net_gain_original <- ggplot(...) + ...
ggsave("Graphs/net_gain_original.png", plot = net_gain_original, width = 8, height = 5, dpi = 300)


Same for updated plots.

Undefined plots when saving
Ensure you save the correct object:

ggsave("Graphs/number_regions_updated.png",
       plot = number_regions_updated, width = 8, height = 5, dpi = 300)

Citation

Nordhaus, W. D. (2015).
Climate Clubs: Overcoming Free-riding in International Climate Policy.
American Economic Review, 105(4), 1339–1370.
https://doi.org/10.1257/aer.15000001
