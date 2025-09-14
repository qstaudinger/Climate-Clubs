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
library(furrr)
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

run_scenario_parallel <- function(tariff, SCC, Input, countries,
                                  alpha_scaling = 0.837, n_iter,
                                  n_restarts = 1, seed = NULL,
                                  save_each = TRUE, save_dir = "results") {

  sanitize <- function(x) gsub("[^0-9A-Za-z._-]+", "_", as.character(x))
  if (save_each) {
    save_root <- normalizePath(save_dir, winslash = "/", mustWork = FALSE)
    dir.create(save_root, recursive = TRUE, showWarnings = FALSE)
    scen_dir <- file.path(save_root,
                          sprintf("tariff_%s__SCC_%s", sanitize(tariff), sanitize(SCC)))
    dir.create(scen_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    scen_dir <- NULL
  }
  
  # Benefit kernel: calculates everything for a given status vector
  benefits_kernel <- function(status_vec) {
    df <- Input
    df$test_status <- status_vec
    df$SCC_scaled  <- df$SCC_unscaled * SCC
    
    # TariffRate_<country>: Tariff from i to j, where i=1 (in the club), j=0 (outside)
    test_status_vec <- df$test_status
    TariffRate_cols <- setNames(
      lapply(seq_along(Input$Country), function(i) {
        const_i <- test_status_vec[i]
        ifelse(test_status_vec == 0 & const_i == 1, tariff, 0)
      }),
      paste0("TariffRate_", Input$Country)
    )
    df <- dplyr::mutate(df, !!!TariffRate_cols)
    
    # Tot_<country>
    tot_cols <- setNames(
      lapply(Input$Country, function(cty) df[[paste0("TariffRate_", cty)]] * df[[paste0("alpha_", cty)]]),
      paste0("Tot_", Input$Country)
    )
    df <- dplyr::mutate(df, !!!tot_cols)
    
    # Tot_Eff_<country>
    Tot_Eff_cols <- setNames(
      lapply(Input$Country, function(cty) -1 * (df[[paste0("TariffRate_", cty)]]^2) * df[[paste0("beta_", cty)]]),
      paste0("Tot_Eff_", Input$Country)
    )
    df <- dplyr::mutate(df, !!!Tot_Eff_cols)
    
    # WFGain_<country>
    wfgain_cols <- setNames(
      lapply(Input$Country, function(cty) df[[paste0("Tot_", cty)]] * df[[paste0("Import_", cty)]]),
      paste0("WFGain_", Input$Country)
    )
    df <- dplyr::mutate(df, !!!wfgain_cols)
    
    # Eff_Gain_<country>
    wf_gain_eff_cols <- setNames(
      lapply(Input$Country, function(cty) df[[paste0("Tot_Eff_", cty)]] * df[[paste0("Import_", cty)]]),
      paste0("Eff_Gain_", Input$Country)
    )
    df <- dplyr::mutate(df, !!!wf_gain_eff_cols)
    
    # Loss_<country> (matrix of counterparty sides)
    W <- do.call(cbind, lapply(paste0("WFGain_", Input$Country), function(nm) df[[nm]]))
    M <- -t(W)
    loss_cols <- setNames(
      lapply(seq_len(nrow(df)), function(j) as.numeric(M[, j])),
      paste0("Loss_", Input$Country)
    )
    df <- dplyr::mutate(df, !!!loss_cols)
    
    # TTI_* per donor country, then Trade_impact as row sum across all TTI columns
    df[paste0("TTI_", Input$Country)] <- sapply(
      Input$Country,
      function(ct) rowSums(df[paste0(c("WFGain_", "Eff_Gain_", "Loss_"), ct)], na.rm = TRUE)
    )
    # TTI columns
    tti_cols <- grep("^TTI_", names(df), value = TRUE)
    ti_by_col <- colSums(df[tti_cols], na.rm = TRUE)
    names(ti_by_col) <- sub("^TTI_", "", names(ti_by_col))
    df$Trade_impact <- ti_by_col[match(as.character(df$Country), names(ti_by_col))]
    
    # Prices, abatement, emissions, benefits
    df$Corrected_alpha <- alpha_scaling * df$McKinsey_alpha
    df$co2_dollar      <- df$CO2 / df$GDP
    
    df$miu_nash   <- (df$SCC_scaled * df$co2_dollar) / (2 * df$Corrected_alpha) / 1000
    df$miu_coop   <- (SCC            * df$co2_dollar) / (2 * df$Corrected_alpha) / 1000
    df$miu_actual <- df$test_status * df$miu_coop + (1 - df$test_status) * df$miu_nash
    
    df$cprice_nash   <- df$SCC_scaled
    df$cprice_coop   <- SCC
    df$cprice_actual <- df$test_status * df$cprice_coop + (1 - df$test_status) * df$cprice_nash
    
    df$cq_nash    <- df$Corrected_alpha * (df$miu_nash^2)
    df$cq_coop    <- df$Corrected_alpha * (df$miu_coop^2)
    df$cq_actual  <- df$test_status * df$cq_coop + (1 - df$test_status) * df$cq_nash
    
    df$abatement_actual <- -df$GDP * df$cq_actual
    df$emissions_actual <- (1 - df$miu_actual) * df$CO2
    
    df$benefits_damage  <- (sum(df$CO2) - sum(df$emissions_actual)) * df$SCC_scaled / 1000
    df$benefits_net     <- df$Trade_impact + df$benefits_damage + df$abatement_actual
    
    list(
      benefits_net  = df$benefits_net,
      cprice_actual = df$cprice_actual
    )
  }
  
  # Calculating a restart serially
  run_one_restart <- function(r) {
    if (!is.null(seed)) {
      set.seed(seed + r - 1L + as.integer(round(1e4 * tariff)) + as.integer(round(10 * SCC)))
      #set.seed(seed + r - 1L)
    }
    
    # Initial state
    df_state <- dplyr::mutate(
      Input,
      start_status  = sample(c(0L, 1L), size = dplyr::n(), replace = TRUE),
      start_welfare = NA_real_,
      start_ctax    = NA_real_
    )
    
    # Baseline once only
    base0 <- benefits_kernel(df_state$start_status)
    df_state$start_welfare <- base0$benefits_net
    df_state$start_ctax    <- base0$cprice_actual
    
    # Iterations
    for (it in seq_len(n_iter)) {
      random_factor <- sample(c(0L, 1L), size = nrow(df_state), replace = TRUE)
      test_status   <- dplyr::if_else(df_state$start_status + random_factor == 2L, 0L,
                                      df_state$start_status + random_factor)
      
      benef_prop <- benefits_kernel(test_status)
      
      Diff <- benef_prop$benefits_net - df_state$start_welfare
      Eff  <- ifelse(random_factor == 1L, Diff, NA_real_)
      
      has_candidates <- any(random_factor == 1L, na.rm = TRUE)
      cond <- has_candidates && min(Eff[random_factor == 1L], na.rm = TRUE) > 0
      
      # Only accept if all substitutions are slightly better (min>0)
      if (cond) {
        df_state$start_status  <- test_status
        df_state$start_welfare <- benef_prop$benefits_net
        df_state$start_ctax    <- benef_prop$cprice_actual
      }
      # otherwise the status remains unchanged
    }
    
    # Result object
    status_cols <- setNames(
      lapply(Input$Country, function(cty) df_state$start_status[df_state$Country == cty]),
      paste0("Final_Status_", Input$Country)
    )
    welfare_cols <- setNames(
      lapply(Input$Country, function(cty) df_state$start_welfare[df_state$Country == cty]),
      paste0("Final_Welfare_", Input$Country)
    )
    
    result <- data.frame(
      restart      = r,
      tariff_param = tariff,
      SCC_param    = SCC,
      final_ctax   = mean(df_state$start_ctax,   na.rm = TRUE),
      status_cols,
      welfare_cols,
      Sum_Status   = sum(df_state$start_status,  na.rm = TRUE),
      Sum_Welfare  = sum(df_state$start_welfare, na.rm = TRUE),
      global_price = mean(df_state$start_ctax,   na.rm = TRUE),
      sum_gains    = sum(df_state$start_welfare, na.rm = TRUE),
      check.names  = FALSE,
      row.names    = NULL
    )
    
    if (save_each && !is.null(scen_dir)) {
      utils::write.csv(
        result,
        file = file.path(scen_dir, sprintf("results_restart_%03d.csv", r)),
        row.names = FALSE
      )
    }
    
    result
  }
  
  # Run restarts (serially in this function)
  res_list <- lapply(seq_len(n_restarts), run_one_restart)
  if (length(res_list) == 0) stop("No results: run_one_restart did not return any results.")
  out <- dplyr::bind_rows(res_list)
  
  if (save_each && !is.null(scen_dir)) {
    utils::write.csv(
      out,
      file = file.path(scen_dir, "results__scenario_summary.csv"),
      row.names = FALSE
    )
  }
  
  out
}

################################################################################
# 3. Run data
################################################################################

# 3.1. Define Paramters 
parameter <- expand.grid(
  tariff = seq(0, 0.1, length.out = 11),
  SCC    = c(12.5, 25, 50, 100)
)

# 44 scenarios in parallel, restarts internally in series
future::plan(future::multisession, workers = min(nrow(parameter), future::availableCores() - 1))


# 3.2.1. Run data for original data set 
results_original <- furrr::future_pmap_dfr(
  parameter,  # tibble mit Spalten: tariff, SCC
  ~ run_scenario_parallel(
    tariff = ..1,
    SCC = ..2,
    Input = Input_Original,
    countries = Input_Original$Country,
    n_iter = 10000,
    n_restarts = 5,
    seed = 123,
    save_each = TRUE,
    save_dir = "Results_Original"
  ),
  .options = furrr::furrr_options(seed = 123)
)
write.csv(results_original, "Results/results_original.csv")
#write_xlsx(results_original, "Results/results_original.xlsx")

# 3.2.2. Run data for updated data set 
results_updated <- furrr::future_pmap_dfr(
  parameter,  
  ~ run_scenario_parallel(
    tariff = ..1,
    SCC = ..2,
    Input = Input_Updated,
    countries = Input_Updated$Country,
    n_iter = 10000,
    n_restarts = 5,
    seed = 123,
    save_each = TRUE,
    save_dir = "Results_Updated"
  ),
  .options = furrr::furrr_options(seed = 123)
)
write.csv(results_updated, "Results/results_updated_new.csv")
#write_xlsx(results_updated, "Results/results_updated.xlsx")

################################################################################
# 4. Graph Production
################################################################################

# 4.1.1 Number of Participating Regions by International Target Carbon Price and Tariff Rate
results_original_agg <- results_original %>%
  group_by(SCC_param, tariff_param) %>%
  summarise (
    global_price = mean(global_price, na.rm = TRUE),
    sum_gains = mean(sum_gains, na.rm = TRUE),
    Sum_Status = mean(Sum_Status, na.rm = TRUE)
  )%>%
  ungroup() %>%
  mutate(
    combo = paste0("SCC_", SCC_param, "_t", tariff_param),
    combo = factor(combo, levels = combo) 
  )

number_regions_original <- ggplot(results_original_agg, aes(x = combo, y = Sum_Status, fill = tariff_param)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC_param, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions"
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
carbon_price_original <- ggplot(results_original_agg, aes(x = combo, y = global_price, fill = tariff_param)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC_param, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = expression("Global average carbon ($t/CO "[2]*")")
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
net_gain_original.png <- ggplot(results_original_agg, aes(x = combo, y = sum_gains, fill = tariff_param)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC_param, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Net Income Gain (billions, 2011$/year)"
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
results_updated_agg <- results_updated %>%
  group_by(SCC_param, tariff_param) %>%
  summarise (
    global_price = mean(global_price, na.rm = TRUE),
    sum_gains = mean(sum_gains, na.rm = TRUE),
    Sum_Status = mean(Sum_Status, na.rm = TRUE)
  )%>%
  ungroup() %>%
  mutate(
    combo = paste0("SCC_", SCC_param, "_t", tariff_param),
    combo = factor(combo, levels = combo) 
  )

number_regions_updated <- ggplot(results_updated_agg, aes(x = combo, y = Sum_Status, fill = tariff_param)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC_param, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Number of participating regions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/number_regions_updated.png", plot = number_regions_updated, width = 8, height = 5, dpi = 300)


# 4.2.2. Globally Averaged Global Carbon Price by Target Carbon Price and Tariff Rate
carbon_price_updated <- ggplot(results_updated_agg, aes(x = combo, y = global_price, fill = tariff_param)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC_param, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = expression("Global average carbon ($t/CO "[2]*")")
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
net_gain_updated <- ggplot(results_updated_agg, aes(x = combo, y = sum_gains, fill = tariff_param)) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~SCC_param, scales = "free_x", nrow = 1) +
  scale_fill_gradient(low = "skyblue", high = "navy", name = "Tariff") +
  labs(
    x = "Target price (SCC) and tariff rate",
    y = "Net Income Gain (billions, 2011$/year)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # Tariff steht schon in Farben
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Graphs/net_gain_updated.png", plot = net_gain_updated, width = 8, height = 5, dpi = 300)


