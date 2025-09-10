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
