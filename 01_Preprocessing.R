################################################################################
# Replication Nordhaus (2015)
# Climate Clubs: Overcoming Free-riding in International Climate Policy
# 01: In this Data I prepare the data for the 2023 import
################################################################################

# Load Packages
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readr)
library(writexl)

################################################################################
# 1. Trade Data
################################################################################

# Import Trade Data
TradeData <- read_excel("Data/Rawdata/TradeData.xlsx") %>%
  select(reporterCode, reporterISO, reporterDesc, flowDesc, partnerCode, partnerISO, partnerDesc, primaryValue)

# Country Order
regions_order <- c(
  "Brazil","Japan","EU","SSA","Canada","US","LatAm","ROW",
  "SEAsia","Mideast","Russia","India","Safrica","China","Eurasia"
)

## Lists of countries (ISO3) for each region
# Single Countries:
iso_brazil  <- c("BRA")
iso_japan   <- c("JPN")
iso_canada  <- c("CAN")
iso_us      <- c("USA")
iso_india   <- c("IND")
iso_russia  <- c("RUS")
iso_safrica <- c("ZAF")
iso_china   <- c("CHN")   

# EU-27:
iso_eu <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC",
  "HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK",
  "SVN","ESP","SWE"
)

# South-East-Asia (ASEAN + Timor-Leste):
iso_seasia <- c("BRN","KHM","IDN","LAO","MYS","MMR","PHL","SGP","THA","VNM","TLS")

# Middle East
iso_mideast <- c("SAU","ARE","QAT","KWT","OMN","BHR","IRN","IRQ","ISR","JOR","LBN","SYR","YEM","PSE")

# Eurasia
iso_eurasia <- c("UKR","BLR","KAZ","UZB","TKM","KGZ","TJK","ARM","GEO","AZE","MDA")

# Sub-Sahara
iso_ssa <- c(
  "AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD","COM","COD","COG","CIV",
  "DJI","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB","KEN","LSO","LBR",
  "MDG","MWI","MLI","MRT","MUS","MOZ","NAM","NER","NGA","RWA","STP","SEN","SYC",
  "SLE","SOM","SSD","SDN","TZA","TGO","UGA","ZMB","ZWE"
)
iso_ssa <- setdiff(iso_ssa, iso_safrica) # Sicherheitshalber

# Latin America
iso_latam <- c(
  # Südamerika
  "ARG","BOL","CHL","COL","ECU","GUY","PRY","PER","SUR","URY","VEN",
  # Mexiko
  "MEX",
  # Zentralamerika
  "BLZ","CRI","SLV","GTM","HND","NIC","PAN",
  # Karibik (souveräne Staaten)
  "ATG","BHS","BRB","CUB","DMA","DOM","GRD","HTI","JAM","KNA","LCA","VCT","TTO"
)
iso_latam <- setdiff(iso_latam, iso_brazil)

## Mapping-Funktion ISO3 -> Region
iso_to_region <- function(iso3) {
  iso3 <- toupper(iso3)
  case_when(
    iso3 %in% iso_brazil  ~ "Brazil",
    iso3 %in% iso_japan   ~ "Japan",
    iso3 %in% iso_eu      ~ "EU",
    iso3 %in% iso_canada  ~ "Canada",
    iso3 %in% iso_us      ~ "US",
    iso3 %in% iso_latam   ~ "LatAm",
    iso3 %in% iso_seasia  ~ "SEAsia",
    iso3 %in% iso_mideast ~ "Mideast",
    iso3 %in% iso_russia  ~ "Russia",
    iso3 %in% iso_india   ~ "India",
    iso3 %in% iso_safrica ~ "Safrica",
    iso3 %in% iso_china   ~ "China",
    iso3 %in% iso_eurasia ~ "Eurasia",
    iso3 %in% iso_ssa     ~ "SSA",
    TRUE                  ~ "ROW"
  )
}

## Process Data
drop_iso <- c("W00","ALL","XXA","XXB","XXC","Z99")
df_clean <- TradeData %>%
  mutate(
    reporterISO = toupper(reporterISO),
    partnerISO  = toupper(partnerISO)
  ) %>%
  filter(!(reporterISO %in% drop_iso | partnerISO %in% drop_iso))

## Define Export/Import Flow
flows_norm <- df_clean %>%
  mutate(
    flowDesc = stringr::str_to_title(flowDesc),  # "Import"/"Export"
    exporterISO = if_else(flowDesc == "Export", reporterISO, partnerISO),
    importerISO = if_else(flowDesc == "Export", partnerISO,  reporterISO)
  ) %>%
  select(flowDesc, exporterISO, importerISO, primaryValue)

## Estimate Russia by Proxy
exports_raw <- flows_norm %>%
  filter(flowDesc == "Export")

# Has Russia own export reports?
russia_has_exports <- any(exports_raw$exporterISO == "RUS")

# If no, take other reports as proxy
if (!russia_has_exports) {
  russia_proxy <- flows_norm %>%
    filter(flowDesc == "Import", exporterISO == "RUS") %>%
    mutate(flowDesc = "Export") 
  exports_use <- bind_rows(
    exports_raw %>% filter(exporterISO != "RUS"),
    russia_proxy
  )
} else {
  exports_use <- exports_raw
}

## Assign regions
exports_regions <- exports_use %>%
  transmute(
    exporter_region = iso_to_region(exporterISO),
    importer_region = iso_to_region(importerISO),
    value = as.numeric(primaryValue)
  ) %>%
  filter(!is.na(value))

## Construct Export Matrix
export_mat_df <- exports_regions %>%
  group_by(exporter_region, importer_region) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(exporter_region = regions_order,
           importer_region = regions_order,
           fill = list(value = 0)) %>%
  mutate(
    exporter_region = factor(exporter_region, levels = regions_order),
    importer_region = factor(importer_region, levels = regions_order)
  ) %>%
  arrange(exporter_region, importer_region)

export_mat <- export_mat_df %>%
  tidyr::pivot_wider(names_from = importer_region, values_from = value) %>%
  arrange(exporter_region)

# Diagonale is 0
m <- as.matrix(export_mat[,-1])
diag(m) <- 0

trade_matrix_exports <- cbind(exporter = regions_order, as.data.frame(m, check.names = FALSE))
rownames(trade_matrix_exports) <- NULL

## Scale
trade_matrix_exports <- trade_matrix_exports %>%
  mutate(across(-exporter, ~ .x / 1000000))


################################################################################
# 2. GHG Data
################################################################################
GHG <- read_delim("Data/Rawdata/GHG.csv",
                  delim = ";",
                  locale = locale(decimal_mark = ".")) %>%
  rename(Code = `EDGAR Country Code`,
         Carbon = `2023`) %>%
  select(Code, Country, Carbon) %>%
  mutate(
    # alles auf Zahl trimmen: Leerzeichen raus, Komma -> Punkt
    Carbon = str_replace_all(Carbon, "\\s", ""),    
    Carbon = str_replace_all(Carbon, ",", "."),
    Carbon = as.numeric(Carbon)
  )

#Auf 15 Regionen abbilden und aggregieren
ghg_regions <- GHG %>%
  mutate(
    Code   = toupper(Code),
    Carbon = as.numeric(Carbon)
  ) %>%
  filter(!(Code %in% drop_iso)) %>%              # Aggregate wie "W00", "ALL" etc. raus
  mutate(region = iso_to_region(Code)) %>%       # ISO3 -> Region
  group_by(region) %>%
  summarise(Carbon = sum(Carbon, na.rm = TRUE), .groups = "drop") %>%
  complete(region = regions_order, fill = list(Carbon = 0)) %>%  # fehlende Regionen auffüllen
  mutate(region = factor(region, levels = regions_order)) %>%
  arrange(region)

################################################################################
# 3. Population Data
################################################################################

Population <- read_excel("Data/Rawdata/Population.xls") %>%
  rename(Country = `Data Source`,
         Code = `World Development Indicators`,
         Population = `...68`) %>%
  select(Code, Country, Population)%>% 
  slice(-1, -2, -3)

pop_regions <- Population %>%
  mutate(
    Code = toupper(Code),
    # robuste Zahlkonvertierung: Leerzeichen raus, Komma -> Punkt
    Population = Population |> as.character() |> 
      str_replace_all("\\s", "") |> 
      str_replace_all(",", ".") |> 
      as.numeric()
  ) %>%
  filter(!(Code %in% drop_iso)) %>%          # Aggregate wie W00, ALL, …
  mutate(region = iso_to_region(Code)) %>%   # ISO3 -> Region
  group_by(region) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  complete(region = regions_order, fill = list(Population = 0)) %>%  # fehlende Regionen auffüllen
  mutate(region = factor(region, levels = regions_order)) %>%
  arrange(region) %>%
  mutate(Population=Population/1000000)

################################################################################
# 4. GDP Data
################################################################################

GDP <- read_excel("Data/Rawdata/GDP.xls") %>%
  rename(Country = `Data Source`,
         Code = `World Development Indicators`,
         GDP = `...68`) %>%
  select(Code, Country, GDP)%>% 
  slice(-1, -2, -3)

gdp_regions <- GDP %>%
  mutate(
    Code = toupper(Code),
    GDP = GDP |> as.character() |> 
      str_replace_all("\\s", "") |> 
      str_replace_all(",", ".") |> 
      as.numeric()
  ) %>%
  filter(!(Code %in% drop_iso)) %>%          # Aggregate wie W00, ALL, …
  mutate(region = iso_to_region(Code)) %>%   # ISO3 -> Region
  group_by(region) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE), .groups = "drop") %>%
  complete(region = regions_order, fill = list(GDP = 0)) %>%  # fehlende Regionen auffüllen
  mutate(region = factor(region, levels = regions_order)) %>%
  arrange(region) %>%
  mutate(GDP=GDP/1000000000)

################################################################################
# 5. Merge Data
################################################################################

# Use alpha from Nordhaus / McKinsey
mckinsey_alpha <- c(
  0.00716,
  0.03014,
  0.02299,
  0.00701,
  0.04195,
  0.03251,
  0.02032,
  0.01252,
  0.01989,
  0.05073,
  0.05691,
  0.03597,
  0.04949,
  0.05977,
  0.05882
)

# Merge all data sets
Input_Updated <- trade_matrix_exports %>%
  rename(region = exporter) %>%
  left_join(pop_regions, by = "region") %>%
  left_join(gdp_regions, by = "region") %>%
  left_join(ghg_regions, by = "region") %>%
  mutate(McKinsey_alpha = mckinsey_alpha)%>%
  rename(Country=region)

# Export
write_xlsx(Input_Updated, "Data/Input_Updated.xlsx")


