################################################################################
# Replication Nordhaus (2015)
# Climate Clubs: Overcoming Free-riding in International Climate Policy
# Replication for both data sets
################################################################################


################################################################################
# 1. Preparing Data
################################################################################

# 1.1. Load packages
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readxl)
library(ggplot2)
library(writexl)

# 1.2. Preprocessing data for Original Data set
Input_Original <- read_excel("Data/Input_Original.xlsx")
head(Input_Original)

Input_Original <- Input_Original %>%
  mutate(SCC_unscaled=GDP/sum(GDP)) %>%
  mutate(start_status=sample(c(0, 1), size = n(), replace = TRUE))%>%
  mutate(start_welfare = NA_real_)

  # Add alpha and beta from Nordmann
  vars <- c(paste0("alpha_", Input_Original$Country),
            paste0("beta_",  Input_Original$Country))
  vals <- scan(text = "0,682 0,41 0,611 0,548811824 0,684531442 0,733 0,626481595 0,591294405 0,640675679 0,642277081 0,654438965 0,585 0,622645325 0,581 0,614254211 0,7578 0,4271 0,5555 0,5177 0,6458 0,6212 0,5910 0,5578 0,6044 0,6059 0,6174 0,9750 0,5874 0,6181 0,5795",
               dec = ",", quiet = TRUE)
  Input_Original[vars] <- lapply(as.list(vals), rep, times = nrow(Input_Original))
  
  
  # Add optimal Tariff 
  optimal_tariff <- setNames(
    lapply(Input_Original$Country, function(cty) {
      Input_Original[[paste0("alpha_", cty)]] /(2* Input_Original[[paste0("beta_", cty)]])
    }),
    paste0("optimal_tariff_", Input_Original$Country))
  Input_Original <- Input_Original %>% mutate(!!!optimal_tariff)
  

  
# 1.3 Preprocessing data for Updated Data set
  Input_Updated <- read_excel("Data/Input_Updated.xlsx")
  head(Input_Updated)
  

  Input_Updated <- Input_Updated %>%
    mutate(SCC_unscaled=GDP/sum(GDP)) %>%
    mutate(start_status=sample(c(0, 1), size = n(), replace = TRUE))%>%
    mutate(start_welfare = NA_real_)
  
  # Add alpha and beta from Nordmann
  vars <- c(paste0("alpha_", Input_Updated$Country),
            paste0("beta_",  Input_Updated$Country))
  vals <- scan(text = "0,682 0,41 0,611 0,548811824 0,684531442 0,733 0,626481595 0,591294405 0,640675679 0,642277081 0,654438965 0,585 0,622645325 0,581 0,614254211 0,7578 0,4271 0,5555 0,5177 0,6458 0,6212 0,5910 0,5578 0,6044 0,6059 0,6174 0,9750 0,5874 0,6181 0,5795",
               dec = ",", quiet = TRUE)
  Input_Updated[vars] <- lapply(as.list(vals), rep, times = nrow(Input_Updated))
  
  
  # Add optimal Tariff 
  optimal_tariff <- setNames(
    lapply(Input_Updated$Country, function(cty) {
      Input_Updated[[paste0("alpha_", cty)]] /(2* Input_Updated[[paste0("beta_", cty)]])
    }),
    paste0("optimal_tariff_", Input_Updated$Country))
  Input_Updated <- Input_Updated %>% mutate(!!!optimal_tariff)

# 1.4. Delete
rm(vals, vars, optimal_tariff)

################################################################################
# 2. Function Design
################################################################################

run_scenario <- function(tariff, SCC, Input, countries, alpha_scaling = 0.837, n_iter) {
  df_state <- Input
  
  for (it in seq_len(n_iter)) {
    df <- df_state %>%
      # 1) Test-Regime
      mutate(
        random_factor = sample(c(0, 1), size = n(), replace = TRUE),
        test_status = if_else(start_status + random_factor == 2L, 0L, start_status + random_factor),
        SCC_scaled  = SCC_unscaled * SCC
      )
    
    # 2) Tariff Rate: Spalten TariffRate_<country>
    test_status_vec <- df$test_status
    TariffRate_cols <- setNames(
      lapply(seq_along(Input$Country), function(i) {
        const_i <- test_status_vec[i]
        ifelse(test_status_vec == 0 & const_i == 1, tariff, 0)
      }),
      paste0("TariffRate_", Input$Country)
    )
    df <- df %>% mutate(!!!TariffRate_cols)
    
    # 3) Terms of Trade (ToT): Tot_<country>
    tot_cols <- setNames(
      lapply(Input$Country, function(cty) {
        df[[paste0("TariffRate_", cty)]] * df[[paste0("alpha_", cty)]]
      }),
      paste0("Tot_", Input$Country)
    )
    df <- df %>% mutate(!!!tot_cols)
    
    # 4) ToT Efficiency: Tot_Eff_<country>
    Tot_Eff_cols <- setNames(
      lapply(Input$Country, function(cty) {
        -1 * (df[[paste0("TariffRate_", cty)]]^2) * df[[paste0("beta_", cty)]]
      }),
      paste0("Tot_Eff_", Input$Country)
    )
    df <- df %>% mutate(!!!Tot_Eff_cols)
    
    # 5) Welfare Gain: WFGain_<country>
    wfgain_cols <- setNames(
      lapply(Input$Country, function(cty) {
        df[[paste0("Tot_", cty)]] * df[[paste0("Import_", cty)]]
      }),
      paste0("WFGain_", Input$Country)
    )
    df <- df %>% mutate(!!!wfgain_cols)
    
    # 6) Efficiency Gain: Eff_Gain_<country>
    wf_gain_eff_cols <- setNames(
      lapply(Input$Country, function(cty) {
        df[[paste0("Tot_Eff_", cty)]] * df[[paste0("Import_", cty)]]
      }),
      paste0("Eff_Gain_", Input$Country)
    )
    df <- df %>% mutate(!!!wf_gain_eff_cols)
    
    # 7) Loss: Loss_<country>
    src_names <- paste0("WFGain_", Input$Country)
    W <- do.call(cbind, lapply(src_names, function(nm) df[[nm]]))
    M <- -t(W)
    loss_cols <- setNames(
      lapply(seq_len(nrow(df)), function(j) as.numeric(M[, j])),
      paste0("Loss_", Input$Country)
    )
    df <- df %>% mutate(!!!loss_cols)
    
    # 8) Total Trade Impact (TTI)
    df[paste0("TTI_", Input$Country)] <- sapply(
      Input$Country,
      function(ct) rowSums(df[paste0(c("WFGain_", "Eff_Gain_", "Loss_"), ct)], na.rm = TRUE)
    )
    
    # 9) Summary Stats
    tti_cols <- grep("^TTI_", names(df), value = TRUE)
    df <- df %>%
      mutate(
        Trade_impact = if (length(tti_cols) == 0) 0 else colSums(across(any_of(tti_cols), ~ as.numeric(.)), na.rm = TRUE)
      ) %>%
      mutate(
        Corrected_alpha = alpha_scaling * McKinsey_alpha,
        co2_dollar = CO2/GDP
      )
    
    # 10) Nash, Coop, Actual
    df <- df %>%
      mutate(
        miu_nash   = (SCC_scaled * co2_dollar) / (2 * Corrected_alpha) / 1000,
        miu_coop   = (SCC * co2_dollar) / (2 * Corrected_alpha) / 1000,
        miu_actual = test_status * miu_coop + (1 - test_status) * miu_nash,
        cprice_nash   = SCC_scaled,
        cprice_coop   = SCC,
        cprice_actual = test_status * cprice_coop + (1 - test_status) * cprice_nash,
        cq_nash    = Corrected_alpha * miu_nash^2,
        cq_coop    = Corrected_alpha * miu_coop^2,
        cq_actual  = test_status * cq_coop + (1 - test_status) * cq_nash,
        abatement_nash   = GDP * cq_nash,
        abatement_coop   = GDP * cq_coop,
        abatement_actual = -GDP * cq_actual,
        emissions_actual =(1-miu_actual) * CO2,
        benefits_damage  = (sum(CO2)-sum(emissions_actual)) * SCC_scaled / 1000,
        benefits_net     = Trade_impact + benefits_damage + abatement_actual
      )
    
    # 11) Comparison / Pareto-Test
    
    # if first round (start_welfare is NA) → nutze test values
    if (all(is.na(df$start_welfare))) {
      df$start_status  <- df$test_status
      df$start_welfare <- df$benefits_net
      df$start_ctax    <- df$cprice_actual
    }
    
    # Comparison
    df <- df %>%
      mutate(
        Diff = benefits_net - start_welfare,
        Effect = random_factor * Diff,
        tariff = tariff,
        SCC=SCC
      )
    
    # Test for Pareto
    subset_eff <- df$Effect[df$random_factor == 1]
    has_candidates <- length(subset_eff) > 0 && any(!is.na(subset_eff))
    cond <- has_candidates && min(subset_eff, na.rm = TRUE) > 0
    df$final_status  <- if (cond) df$test_status  else df$start_status
    df$final_welfare <- if (cond) df$benefits_net else df$start_welfare
    df$final_ctax    <- if (cond) df$cprice_actual else df$start_ctax
    
    
    # Carry forward status to the next iteration
    df_state <- df %>%
      mutate(
        start_status  = final_status,
        start_welfare = final_welfare,
        start_ctax    = final_ctax
      )
  }
  
  # 12) Merge Results
  
  # Extraxt Status for each country
  status_cols <- setNames(
    lapply(Input$Country, function(cty) df_state$final_status[df_state$Country == cty]),
    paste0("Final_Status_", Input$Country)
  )
  
  # Extraxt Welfare for each country
  welfare_cols <- setNames(
    lapply(Input$Country, function(cty) df_state$final_welfare[df_state$Country == cty]),
    paste0("Final_Welfare_", Input$Country)
  )
  
  # Calculate Summaries for graphs
  global_price <- mean(df$cprice_actual, na.rm = TRUE)
  sum_gains    <- sum(df$benefits_net,   na.rm = TRUE)
  
  # Combine all data in one data set
  result <- data.frame(
    tariff = tariff,
    SCC = SCC,
    final_ctax = mean(df_state$final_ctax),
    status_cols,
    welfare_cols,
    Sum_Status  = sum(df_state$final_status, na.rm = TRUE),
    Sum_Welfare = sum(df_state$final_welfare, na.rm = TRUE),
    global_price = global_price,
    sum_gains=sum_gains,
    check.names = FALSE,
    row.names = NULL
  )
  
  # Reset Values
  df_state <- df_state %>%
    mutate(
      final_ctax     = NA_real_,
      final_status   = NA_integer_,
      final_welfare  = NA_real_,
      start_status   = NA_integer_,
      start_welfare  = NA_real_,
      start_ctax     = NA_real_
    )
  result
}


################################################################################
# 3. Run data
################################################################################

# 3.1. Define Paramters 
parameter <- expand.grid(
  tariff = seq(0, 0.1, length.out = 11),
  SCC    = c(12.5, 25, 50, 100)
)

# 3.2.1. Run data for original data set 
results_original <- pmap_dfr(parameter, ~ run_scenario(tariff = ..1, SCC = ..2, Input = Input_Original, countries = Input$Country, n_iter=20))
write_xlsx(results_original, "Results/results_original.xlsx")

# 3.2.2. Run data for updated data set 
results_updated <- pmap_dfr(parameter, ~ run_scenario(tariff = ..1, SCC = ..2, Input = Input_Updated, countries = Input$Country, n_iter=20))
write_xlsx(results_updated, "Results/results_updated.xlsx")

################################################################################
# 4. Graph Production
################################################################################


# 4.1.1 Number of Participating Regions by International Target Carbon Price and Tariff Rate
results_original <- results_original %>%
  mutate(
    combo = paste0("SCC_", SCC, "_t", tariff),
    combo = factor(combo, levels = combo) 
  )

number_regions_original <- ggplot(results_original, aes(x = combo, y = Sum_Status, fill = tariff)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions",
    title = "Participation by SCC and tariff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/number_regions_original.png", plot = number_regions_original, width = 8, height = 5, dpi = 300)


# 4.1.2. Globally Averaged Global Carbon Price by Target Carbon Price and Tariff Rate
carbon_price_original <- ggplot(results_original, aes(x = combo, y = global_price, fill = tariff)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions",
    title = "Participation by SCC and tariff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/carbon_price_original.png", plot = carbon_price_original, width = 8, height = 5, dpi = 300)


# 4.1.3. Net Economic Gains from Different Regimes
net_gain_original.png <- ggplot(results_original, aes(x = combo, y = sum_gains, fill = tariff)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions",
    title = "Participation by SCC and tariff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/net_gain_original.png", plot = net_gain_original.png, width = 8, height = 5, dpi = 300)


# 4.2.1 Number of Participating Regions by International Target Carbon Price and Tariff Rate
results_updated <- results_updated %>%
  mutate(
    combo = paste0("SCC_", SCC, "_t", tariff),
    combo = factor(combo, levels = combo) 
  )

ggplot(results_updated, aes(x = combo, y = Sum_Status, fill = tariff)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions",
    title = "Participation by SCC and tariff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/net_gain_updated.png", plot = net_gain_updated.png, width = 8, height = 5, dpi = 300)


# 4.2.2. Globally Averaged Global Carbon Price by Target Carbon Price and Tariff Rate
carbon_price_updated <- ggplot(results_updated, aes(x = combo, y = global_price, fill = tariff)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions",
    title = "Participation by SCC and tariff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/carbon_price_updated.png", plot = carbon_price_updated, width = 8, height = 5, dpi = 300)


# 4.2.3. Net Economic Gains from Different Regimes
net_gain_updated.png <- ggplot(results_updated, aes(x = combo, y = sum_gains, fill = tariff)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions",
    title = "Participation by SCC and tariff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/net_gain_updated.png", plot = net_gain_updated.png, width = 8, height = 5, dpi = 300)

