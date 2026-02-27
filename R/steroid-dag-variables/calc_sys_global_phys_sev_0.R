## -----------------------------------------------------------------------------
## 1. APS Temperature Score
## -----------------------------------------------------------------------------

# Calculates APS temperature score component
calc_aps_temp_score <- function(
  hightemp_vsorres,
  lowtemp_vsorres
) {
  # 1. Temperature (rectal, °C) - use most extreme
  # - Get highest and lowest temp (`hightemp_vsorres` can be lower than `lowtemp_vsorres`)
  temp_high <- pmax(hightemp_vsorres, lowtemp_vsorres)
  temp_low <- pmin(hightemp_vsorres, lowtemp_vsorres)

  # - Calculate APS temperature points
  temp_points <- dplyr::case_when(
    temp_high >= 41 | temp_low < 30 ~ 4,
    temp_high >= 39 | temp_low < 32 ~ 3,
    temp_low < 34 ~ 2,
    temp_high >= 38.5 | temp_low < 36 ~ 1,
    temp_high >= 36 | temp_low < 38.5 ~ 0
  )

  return(temp_points)
}

## -----------------------------------------------------------------------------
## 2. APS Mean Arterial Pressure (MAP) Score
## -----------------------------------------------------------------------------

# Calculates APS MAP score component
calc_aps_map_score <- function(
  lowsysbp_vsorres,
  lowdbp_vsorres
) {
  # 2. Mean Arterial Pressure (mmHg)
  # - Calculate MAP from systolic and diastolic BP
  map <- (lowsysbp_vsorres + (2 * lowdbp_vsorres)) / 3

  # - Calculate APS MAP points
  map_points <- dplyr::case_when(
    map >= 160 ~ 4,
    map >= 130 ~ 3,
    map >= 110 ~ 2,
    map >= 70  ~ 0,
    map >= 50  ~ 2,
    map < 50   ~ 4
  )

  return(map_points)
}

## -----------------------------------------------------------------------------
## 3. APS Heart Rate Score
## -----------------------------------------------------------------------------

# Calculates APS heart rate score component
calc_aps_hr_score <- function(
  highhr_vsorres,
  lowhr_vsorres
) {
  # 3. Heart Rate (bpm) - use most extreme
  # - Get highest and lowest heart rate (`highhr_vsorres` can be lower than `lowhr_vsorres`)
  hr_high <- pmax(highhr_vsorres, lowhr_vsorres)
  hr_low <- pmin(highhr_vsorres, lowhr_vsorres)

  # - Calculate APS heart rate points
  hr_points <- dplyr::case_when(
    hr_high >= 180 | hr_low < 40 ~ 4,
    hr_high >= 140 | hr_low < 55 ~ 3,
    hr_high >= 110 | hr_low < 70 ~ 2,
    hr_high >= 70 | hr_low < 110 ~ 0
  )

  return(hr_points)
}

## -----------------------------------------------------------------------------
## 4. APS Respiratory Rate Score
## -----------------------------------------------------------------------------

# Calculates APS respiratory rate score component
calc_aps_rr_score <- function(
  highrr_vsorres,
  lowrr_vsorres
) {
  # 4. Respiratory Rate (breaths/min) - use worst (most extreme)
  # - Get highest and lowest respiratory rate (`highrr_vsorres` can be lower than `lowrr_vsorres`)
  rr_high <- pmax(highrr_vsorres, lowrr_vsorres)
  rr_low <- pmin(highrr_vsorres, lowrr_vsorres)

  # - Calculate APS respiratory rate points
  rr_points <- dplyr::case_when(
    rr_high >= 50 | rr_low < 6  ~ 4,
    rr_high >= 35               ~ 3,
    rr_low < 10                 ~ 2,
    rr_high >= 25 | rr_low < 12 ~ 1,
    rr_high >= 12 | rr_low < 25 ~ 0,
  )

  return(rr_points)
}

## -----------------------------------------------------------------------------
## 5. APS Oxygenation Score
## -----------------------------------------------------------------------------

# Calculates APS oxygenation score component
# Uses A-aDO2 if FiO2 >= 0.5, else uses PaO2
calc_aps_oxy_score <- function(
  resp_support_type_0,  # Calculated by calc_resp_support_type()
  resp_support_type_m1, # Calculated by calc_resp_support_type()
  resp_support_type_m2, # Calculated by calc_resp_support_type()

  daily_resp_8a_0_code,
  daily_resp_8a_m1_code,
  daily_resp_8a_m2_code,

  daily_standard_flow_8a_0,
  daily_standard_flow_8a_m1,
  daily_standard_flow_8a_m2,

  daily_hfnc_fi02_8a_0,
  daily_hfnc_fi02_8a_m1,
  daily_hfnc_fi02_8a_m2,

  daily_niv_fi02_8a_0,
  daily_niv_fi02_8a_m1,
  daily_niv_fi02_8a_m2,

  daily_imv_fio2_8a_0,
  daily_imv_fio2_8a_m1,
  daily_imv_fio2_8a_m2,

  daily_pa02_lowest_0,
  daily_pa02_lowest_m1,
  daily_pa02_lowest_m2,

  daily_paco2_lowest_0,
  daily_paco2_lowest_m1,
  daily_paco2_lowest_m2
) {
  # 5. Oxygenation: A-aDO2 if FiO2 >= 0.5, else PaO2

  # Determine FiO2 based on respiratory support type (using helper function)
  # - If FiO2 is still missing after this, it will be treated as < 0.5 for APS calculation purposes

  ## Get FiO2 for each day
  fio2_0 <- calc_fio2_from_resp_support(
    resp_support_type_0,
    daily_standard_flow_8a_0,
    daily_hfnc_fi02_8a_0,
    daily_niv_fi02_8a_0,
    daily_imv_fio2_8a_0
  )

  fio2_m1 <- calc_fio2_from_resp_support(
    resp_support_type_m1,
    daily_standard_flow_8a_m1,
    daily_hfnc_fi02_8a_m1,
    daily_niv_fi02_8a_m1,
    daily_imv_fio2_8a_m1
  )

  fio2_m2 <- calc_fio2_from_resp_support(
    resp_support_type_m2,
    daily_standard_flow_8a_m2,
    daily_hfnc_fi02_8a_m2,
    daily_niv_fi02_8a_m2,
    daily_imv_fio2_8a_m2
  )

  ## Get final FiO2 with lookback (Day 0 -> Day -1 -> Day -2)
  fio2 <- get_value_with_lookback(fio2_0, fio2_m1, fio2_m2)

  # Track which day FiO2 came from (0 = Day 0, 1 = Day -1, 2 = Day -2)
  fio2_day <- dplyr::case_when(
    !is.na(fio2_0) ~ 0,
    !is.na(fio2_m1) ~ 1,
    !is.na(fio2_m2) ~ 2
  )

  # Check if patient is on ECMO, but only if ECMO occurred on same or earlier day than FiO2
  resp_code <- get_value_with_lookback(daily_resp_8a_0_code, daily_resp_8a_m1_code, daily_resp_8a_m2_code)

  # Track which day resp_code came from (0 = Day 0, 1 = Day -1, 2 = Day -2)
  resp_code_day <- dplyr::case_when(
    !is.na(daily_resp_8a_0_code) ~ 0,
    !is.na(daily_resp_8a_m1_code) ~ 1,
    !is.na(daily_resp_8a_m2_code) ~ 2
  )

  # ECMO is only prioritized if it occurred on an earlier or same available day as FiO2
  is_ecmo <- !is.na(resp_code) & resp_code %in% c(1, 2) &
              (is.na(fio2_day) | (resp_code_day <= fio2_day))

  # Get PaO2 and PaCO2 with lookback
  pao2 <- get_value_with_lookback(daily_pa02_lowest_0, daily_pa02_lowest_m1, daily_pa02_lowest_m2)
  paco2 <- get_value_with_lookback(daily_paco2_lowest_0, daily_paco2_lowest_m1, daily_paco2_lowest_m2)

  # Calculate A-aDO2 if FiO2 >= 0.5
  # A-aDO2 = (FiO2 * (760 - 47)) - (PaCO2 / 0.8) - PaO2
  aado2 <- ifelse(!is.na(fio2) & fio2 >= 0.5 & !is.na(paco2) & !is.na(pao2),
                  (fio2 * (760 - 47)) - (paco2 / 0.8) - pao2,
                  NA)

  oxy_points <- dplyr::case_when(
    # ECMO patients (daily_resp_8a_0_code %in% c(1, 2)) get maximum severity (4 points)
    is_ecmo ~ 4,
    # A-aDO2 used if FiO2 >= 0.5
    !is.na(aado2) & aado2 >= 500 ~ 4,
    !is.na(aado2) & aado2 >= 350 ~ 3,
    !is.na(aado2) & aado2 >= 200 ~ 2,
    !is.na(aado2) & aado2 < 200 ~ 0,
    # PaO2 used if FiO2 < 0.5 or A-aDO2 not calculable
    !is.na(pao2) & pao2 > 70 ~ 0,
    !is.na(pao2) & pao2 >= 61 ~ 1,
    !is.na(pao2) & pao2 >= 55 ~ 3,
    !is.na(pao2) & pao2 < 55 ~ 4,
    # Impute normal (0 points) if measurement is missing
    .default = 0
  )

  return(oxy_points)
}

## -----------------------------------------------------------------------------
## 6. APS Arterial pH Score
## -----------------------------------------------------------------------------

# Calculates APS arterial pH score component
calc_aps_ph_score <- function(
  daily_ph_lowest_0,
  daily_ph_lowest_m1,
  daily_ph_lowest_m2
) {
  # 6. Arterial pH
  ph <- get_value_with_lookback(daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2)

  ph_points <- dplyr::case_when(
    ph >= 7.7 ~ 4,
    ph >= 7.6 ~ 3,
    ph >= 7.5 ~ 1,
    ph >= 7.33 ~ 0,
    ph >= 7.25 ~ 2,
    ph >= 7.15 ~ 3,
    ph < 7.15 ~ 4,
    # Impute normal (0 points) if measurement is missing
    .default = 0
  )

  return(ph_points)
}

## -----------------------------------------------------------------------------
## 7. APS Serum Sodium Score
## -----------------------------------------------------------------------------

# Calculates APS serum sodium score component
calc_aps_sodium_score <- function(
  daily_na_8a_0,
  daily_na_8a_m1,
  daily_na_8a_m2
) {
  # 7. Serum Sodium (mmol/L)
  sodium <- get_value_with_lookback(daily_na_8a_0, daily_na_8a_m1, daily_na_8a_m2)

  sodium_points <- dplyr::case_when(
    sodium >= 180 ~ 4,
    sodium >= 160 ~ 3,
    sodium >= 155 ~ 2,
    sodium >= 150 ~ 1,
    sodium >= 130 ~ 0,
    sodium >= 120 ~ 2,
    sodium >= 111 ~ 3,
    sodium < 111  ~ 4
  )

  return(sodium_points)
}

## -----------------------------------------------------------------------------
## 8. APS Serum Potassium Score
## -----------------------------------------------------------------------------

# Calculates APS serum potassium score component
calc_aps_potassium_score <- function(
  daily_k_8a_0,
  daily_k_8a_m1,
  daily_k_8a_m2
) {
  # 8. Serum Potassium (mmol/L)
  potassium <- get_value_with_lookback(daily_k_8a_0, daily_k_8a_m1, daily_k_8a_m2)

  k_points <- dplyr::case_when(
    potassium >= 7 ~ 4,
    potassium >= 6 ~ 3,
    potassium >= 5.5 ~ 1,
    potassium >= 3.5 ~ 0,
    potassium >= 3 ~ 1,
    potassium >= 2.5 ~ 2,
    potassium < 2.5 ~ 4
  )

  return(k_points)
}

## -----------------------------------------------------------------------------
## 9. APS Serum Creatinine Score
## -----------------------------------------------------------------------------

# Calculates APS serum creatinine score component
# Note: Points can be doubled for acute renal failure
calc_aps_creatinine_score <- function(
  daily_cr_8a_0,
  daily_cr_8a_m1,
  daily_cr_8a_m2,
  sofa_base_renal_dysnfx,
  sofa_base_renal_chronic
) {
  # 9. Serum Creatinine (mg/dL)
  creatinine <- get_value_with_lookback(daily_cr_8a_0, daily_cr_8a_m1, daily_cr_8a_m2)

  cr_points <- dplyr::case_when(

    # Check whether or not to double points
    creatinine >= 3.5 & (
      sofa_base_renal_dysnfx != "Yes" |
      (
        sofa_base_renal_dysnfx == "Yes" &
        sofa_base_renal_chronic %in% c(
          "Creatinine 1.2 - 1.9 mg/dL",
          "Creatinine 2.0 - 3.4 mg/dL"
        )
      )
    ) ~ 8,
    creatinine >= 3.5 ~ 4,

    # Check whether or not to double points
    creatinine >= 2 & (
      sofa_base_renal_dysnfx != "Yes" |
      (sofa_base_renal_dysnfx == "Yes" & sofa_base_renal_chronic == "Creatinine 1.2 - 1.9 mg/dL")
    ) ~ 6,
    creatinine >= 2 ~ 3,

    # # Check whether or not to double points
    creatinine >= 1.5 & sofa_base_renal_dysnfx != "Yes" ~ 4,
    creatinine >= 1.5 ~ 2,

    # Don't double these two
    creatinine >= 0.6 ~ 0,
    creatinine < 0.6 ~ 2
  )

  return(cr_points)
}

## -----------------------------------------------------------------------------
## 10. APS Hematocrit Score
## -----------------------------------------------------------------------------

# Calculates APS hematocrit score component
calc_aps_hct_score <- function(
  daily_hct_8a_0,
  daily_hct_8a_m1,
  daily_hct_8a_m2
) {
  # 10. Hematocrit (%)
  hematocrit <- get_value_with_lookback(daily_hct_8a_0, daily_hct_8a_m1, daily_hct_8a_m2)

  hct_points <- dplyr::case_when(
    hematocrit >= 60 ~ 4,
    hematocrit >= 50 ~ 2,
    hematocrit >= 46 ~ 1,
    hematocrit >= 30 ~ 0,
    hematocrit >= 20 ~ 2,
    hematocrit < 20 ~ 4
  )

  return(hct_points)
}

## -----------------------------------------------------------------------------
## 11. APS White Blood Cell Count Score
## -----------------------------------------------------------------------------

# Calculates APS white blood cell count score component
calc_aps_wbc_score <- function(
  daily_wbc_8a_0,
  daily_wbc_8a_m1,
  daily_wbc_8a_m2
) {
  # 11. White Blood Cell Count (1000/mm³)
  wbc <- get_value_with_lookback(daily_wbc_8a_0, daily_wbc_8a_m1, daily_wbc_8a_m2)

  wbc_points <- dplyr::case_when(
    wbc >= 40 ~ 4,
    wbc >= 20 ~ 2,
    wbc >= 15 ~ 1,
    wbc >= 3 ~ 0,
    wbc >= 1 ~ 2,
    wbc < 1 ~ 4
  )

  return(wbc_points)
}

## -----------------------------------------------------------------------------
## 12. APS Glasgow Coma Score
## -----------------------------------------------------------------------------

# Calculates APS Glasgow Coma Score component (15 - GCS)
# - Look back to find the last available value without a T
# - If no prior value available without a T, then assume a "normal" GCS (15)
#   This is consistent with Footnote C from the SOFA 2 paper.
calc_aps_gcs_score <- function(
  daily_gcs_8a_0,
  daily_gcs_8a_m1,
  daily_gcs_8a_m2
) {

  # Use helper function to get GCS with lookback
  gcs <- get_gcs_with_lookback(daily_gcs_8a_0, daily_gcs_8a_m1, daily_gcs_8a_m2)

  # Extract quantitative component of GCS
  gcs <- as.numeric(gcs)

  # Calculate points (15 - GCS)
  gcs_points <- 15 - gcs

  return(gcs_points)
}

## -----------------------------------------------------------------------------
## APS Score (Combining All Components)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Global Physiology Severity'
#
# Value: Total APS score (sum of 12 components, range 0-48+)
calc_sys_global_phys_sev_0 <- function(
  aps_temp_score,
  aps_map_score,
  aps_hr_score,
  aps_rr_score,
  aps_oxy_score,
  aps_ph_score,
  aps_sodium_score,
  aps_potassium_score,
  aps_creatinine_score,
  aps_hct_score,
  aps_wbc_score,
  aps_gcs_score
) {
  # Impute normal (0 points) for any component with NA value
  # This allows extraction of records missing components (from earlier steps) for quality improvement
  # while still calculating a complete APS score properly
  total_aps <- rowSums(
    cbind(
      dplyr::coalesce(aps_temp_score, 0),
      dplyr::coalesce(aps_map_score, 0),
      dplyr::coalesce(aps_hr_score, 0),
      dplyr::coalesce(aps_rr_score, 0),
      dplyr::coalesce(aps_oxy_score, 0),
      dplyr::coalesce(aps_ph_score, 0),
      dplyr::coalesce(aps_sodium_score, 0),
      dplyr::coalesce(aps_potassium_score, 0),
      dplyr::coalesce(aps_creatinine_score, 0),
      dplyr::coalesce(aps_hct_score, 0),
      dplyr::coalesce(aps_wbc_score, 0),
      dplyr::coalesce(aps_gcs_score, 0)
    ),
    na.rm = FALSE
  )

  return(total_aps)
}


## -----------------------------------------------------------------------------
## Helper Functions
## -----------------------------------------------------------------------------

# Helper function to calculate FiO2 from respiratory support type
#
# Value: FiO2 based on respiratory support type
# - Uses the same logic as the denominator calculation in `calc_sfratio_8a_0()`
# - to ensure consistency.
calc_fio2_from_resp_support <- function(
  resp_support_type, # Calculated by calc_resp_support_type()
  standard_flow,
  hfnc_fio2,
  niv_fio2,
  imv_fio2
) {
  dplyr::case_when(
    resp_support_type == 1 ~ 0.21,
    resp_support_type == 2 ~ 0.21 + (0.03 * standard_flow),
    resp_support_type == 3 ~ hfnc_fio2,
    resp_support_type == 4 ~ niv_fio2,
    resp_support_type %in% c(5, 6) ~ imv_fio2
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_global_phys_sev_0 (APS score) columns (one row per record_id)
wrapper_calc_sys_global_phys_sev_0 <- function(data, dictionary) {
  # Get Day 0 variables
  data_day0_vars <- data |>
    filter(event_label == 'Day 0') |>
    select(
      record_id,
      vs_perf, # branching logic
      hightemp_vsorres,
      lowtemp_vsorres,
      highhr_vsorres,
      lowhr_vsorres,
      highrr_vsorres,
      lowrr_vsorres,
      lowsysbp_vsorres,
      lowdbp_vsorres,
      sofa_unk, # branching logic
      sofa_base_renal_dysnfx,
      sofa_base_renal_chronic
    )

  # Get Daily In-Hospital Forms variables (Days 0, -1, -2)
  data_hospitalform_vars <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    select(
      record_id,

      ## - Day 0
      dailysofa_perf_0, # branching logic
      daily_resp_8a_0, # branching logic
      daily_imv_mode_8a_0,
      daily_imv_fio2_8a_0,
      daily_standard_flow_8a_0,
      daily_hfnc_fi02_8a_0,
      daily_niv_fi02_8a_0,
      daily_epap_8a_0,
      daily_pao2_occur_0, # branching logic
      daily_pa02_lowest_0,
      daily_paco2_lowest_0,
      daily_ph_lowest_0,
      daily_k_nc_0, # branching logic
      daily_k_8a_0,
      daily_na_nc_0, # branching logic
      daily_na_8a_0,
      daily_hct_nc_0, # branching logic
      daily_hct_8a_0,
      daily_wbc_nc_0, # branching logic
      daily_wbc_8a_0,
      daily_cr_nc_0, # branching logic
      daily_cr_8a_0,
      daily_gcs_8a_0,

      ## - Day -1
      dailysofa_perf_m1, # branching logic
      daily_resp_8a_m1, # branching logic
      daily_imv_mode_8a_m1,
      daily_imv_fio2_8a_m1,
      daily_standard_flow_8a_m1,
      daily_hfnc_fi02_8a_m1,
      daily_niv_fi02_8a_m1,
      daily_epap_8a_m1,
      daily_pao2_occur_m1, # branching logic
      daily_pa02_lowest_m1,
      daily_paco2_lowest_m1,
      daily_ph_lowest_m1,
      daily_k_nc_m1, # branching logic
      daily_k_8a_m1,
      daily_na_nc_m1, # branching logic
      daily_na_8a_m1,
      daily_hct_nc_m1, # branching logic
      daily_hct_8a_m1,
      daily_wbc_nc_m1, # branching logic
      daily_wbc_8a_m1,
      daily_cr_nc_m1, # branching logic
      daily_cr_8a_m1,
      daily_gcs_8a_m1,

      ## - Day -2
      dailysofa_perf_m2, # branching logic
      daily_resp_8a_m2, # branching logic
      daily_imv_mode_8a_m2,
      daily_imv_fio2_8a_m2,
      daily_standard_flow_8a_m2,
      daily_hfnc_fi02_8a_m2,
      daily_niv_fi02_8a_m2,
      daily_epap_8a_m2,
      daily_pao2_occur_m2, # branching logic
      daily_pa02_lowest_m2,
      daily_paco2_lowest_m2,
      daily_ph_lowest_m2,
      daily_k_nc_m2, # branching logic
      daily_k_8a_m2,
      daily_na_nc_m2, # branching logic
      daily_na_8a_m2,
      daily_hct_nc_m2, # branching logic
      daily_hct_8a_m2,
      daily_wbc_nc_m2, # branching logic
      daily_wbc_8a_m2,
      daily_cr_nc_m2, # branching logic
      daily_cr_8a_m2,
      daily_gcs_8a_m2
    ) |>
    # Add code mappings for respiratory support variables
    left_join(
      get_code_label_map('daily_resp_8a_0', dictionary),
      by = 'daily_resp_8a_0'
    ) |>
    left_join(
      get_code_label_map('daily_resp_8a_m1', dictionary),
      by = 'daily_resp_8a_m1'
    ) |>
    left_join(
      get_code_label_map('daily_resp_8a_m2', dictionary),
      by = 'daily_resp_8a_m2'
    )

  # Join Day 0 and Daily In-Hospital Forms data
  data_aps_vars <- data_day0_vars |>
    left_join(
      data_hospitalform_vars,
      by = 'record_id'
    )

  # Calculate respiratory support types for all 3 days
  data_aps_vars <- data_aps_vars |>
    mutate(
      # Calculate respiratory support type for day 0
      resp_support_type_0 = calc_resp_support_type(
        daily_resp_8a_code = daily_resp_8a_0_code,
        dailysofa_perf = dailysofa_perf_0,
        daily_standard_flow_8a = daily_standard_flow_8a_0,
        daily_hfnc_fi02_8a = daily_hfnc_fi02_8a_0,
        daily_niv_fi02_8a = daily_niv_fi02_8a_0,
        daily_imv_fio2_8a = daily_imv_fio2_8a_0,
        daily_epap_8a = daily_epap_8a_0
      ),
      # Calculate respiratory support type for day -1
      resp_support_type_m1 = calc_resp_support_type(
        daily_resp_8a_code = daily_resp_8a_m1_code,
        dailysofa_perf = dailysofa_perf_m1,
        daily_standard_flow_8a = daily_standard_flow_8a_m1,
        daily_hfnc_fi02_8a = daily_hfnc_fi02_8a_m1,
        daily_niv_fi02_8a = daily_niv_fi02_8a_m1,
        daily_imv_fio2_8a = daily_imv_fio2_8a_m1,
        daily_epap_8a = daily_epap_8a_m1
      ),
      # Calculate respiratory support type for day -2
      resp_support_type_m2 = calc_resp_support_type(
        daily_resp_8a_code = daily_resp_8a_m2_code,
        dailysofa_perf = dailysofa_perf_m2,
        daily_standard_flow_8a = daily_standard_flow_8a_m2,
        daily_hfnc_fi02_8a = daily_hfnc_fi02_8a_m2,
        daily_niv_fi02_8a = daily_niv_fi02_8a_m2,
        daily_imv_fio2_8a = daily_imv_fio2_8a_m2,
        daily_epap_8a = daily_epap_8a_m2
      )
    )

  # Calculate all APS component scores
  data_aps_component_vars <- data_aps_vars |>
    mutate(
      aps_temp_score = calc_aps_temp_score(
        hightemp_vsorres = hightemp_vsorres,
        lowtemp_vsorres = lowtemp_vsorres
      )
    ) |>
    mutate(
      aps_map_score = calc_aps_map_score(
        lowsysbp_vsorres = lowsysbp_vsorres,
        lowdbp_vsorres = lowdbp_vsorres
      )
    ) |>
    mutate(
      aps_hr_score = calc_aps_hr_score(
        highhr_vsorres = highhr_vsorres,
        lowhr_vsorres = lowhr_vsorres
      )
    ) |>
    mutate(
      aps_rr_score = calc_aps_rr_score(
        highrr_vsorres = highrr_vsorres,
        lowrr_vsorres = lowrr_vsorres
      )
    ) |>
    mutate(
      aps_oxy_score = calc_aps_oxy_score(
        resp_support_type_0 = resp_support_type_0,
        resp_support_type_m1 = resp_support_type_m1,
        resp_support_type_m2 = resp_support_type_m2,

        daily_resp_8a_0_code = daily_resp_8a_0_code,
        daily_resp_8a_m1_code = daily_resp_8a_m1_code,
        daily_resp_8a_m2_code = daily_resp_8a_m2_code,

        daily_standard_flow_8a_0 = daily_standard_flow_8a_0,
        daily_standard_flow_8a_m1 = daily_standard_flow_8a_m1,
        daily_standard_flow_8a_m2 = daily_standard_flow_8a_m2,

        daily_hfnc_fi02_8a_0 = daily_hfnc_fi02_8a_0,
        daily_hfnc_fi02_8a_m1 = daily_hfnc_fi02_8a_m1,
        daily_hfnc_fi02_8a_m2 = daily_hfnc_fi02_8a_m2,

        daily_niv_fi02_8a_0 = daily_niv_fi02_8a_0,
        daily_niv_fi02_8a_m1 = daily_niv_fi02_8a_m1,
        daily_niv_fi02_8a_m2 = daily_niv_fi02_8a_m2,

        daily_imv_fio2_8a_0 = daily_imv_fio2_8a_0,
        daily_imv_fio2_8a_m1 = daily_imv_fio2_8a_m1,
        daily_imv_fio2_8a_m2 = daily_imv_fio2_8a_m2,

        daily_pa02_lowest_0 = daily_pa02_lowest_0,
        daily_pa02_lowest_m1 = daily_pa02_lowest_m1,
        daily_pa02_lowest_m2 = daily_pa02_lowest_m2,

        daily_paco2_lowest_0 = daily_paco2_lowest_0,
        daily_paco2_lowest_m1 = daily_paco2_lowest_m1,
        daily_paco2_lowest_m2 = daily_paco2_lowest_m2
      )
    ) |>
    mutate(
      aps_ph_score = calc_aps_ph_score(
        daily_ph_lowest_0 = daily_ph_lowest_0,
        daily_ph_lowest_m1 = daily_ph_lowest_m1,
        daily_ph_lowest_m2 = daily_ph_lowest_m2
      )
    ) |>
    mutate(
      aps_sodium_score = calc_aps_sodium_score(
        daily_na_8a_0 = daily_na_8a_0,
        daily_na_8a_m1 = daily_na_8a_m1,
        daily_na_8a_m2 = daily_na_8a_m2
      )
    ) |>
    mutate(
      aps_potassium_score = calc_aps_potassium_score(
        daily_k_8a_0 = daily_k_8a_0,
        daily_k_8a_m1 = daily_k_8a_m1,
        daily_k_8a_m2 = daily_k_8a_m2
      )
    ) |>
    mutate(
      aps_creatinine_score = calc_aps_creatinine_score(
        daily_cr_8a_0 = daily_cr_8a_0,
        daily_cr_8a_m1 = daily_cr_8a_m1,
        daily_cr_8a_m2 = daily_cr_8a_m2,
        sofa_base_renal_dysnfx = sofa_base_renal_dysnfx,
        sofa_base_renal_chronic = sofa_base_renal_chronic
      )
    ) |>
    mutate(
      aps_hct_score = calc_aps_hct_score(
        daily_hct_8a_0 = daily_hct_8a_0,
        daily_hct_8a_m1 = daily_hct_8a_m1,
        daily_hct_8a_m2 = daily_hct_8a_m2
      )
    ) |>
    mutate(
      aps_wbc_score = calc_aps_wbc_score(
        daily_wbc_8a_0 = daily_wbc_8a_0,
        daily_wbc_8a_m1 = daily_wbc_8a_m1,
        daily_wbc_8a_m2 = daily_wbc_8a_m2
      )
    ) |>
    mutate(
      aps_gcs_score = calc_aps_gcs_score(
        daily_gcs_8a_0 = daily_gcs_8a_0,
        daily_gcs_8a_m1 = daily_gcs_8a_m1,
        daily_gcs_8a_m2 = daily_gcs_8a_m2
      )
    )

  # Calculate final APS score
  data_aps_var <- data_aps_component_vars |>
    mutate(
      sys_global_phys_sev_0 = calc_sys_global_phys_sev_0(
        aps_temp_score = aps_temp_score,
        aps_map_score = aps_map_score,
        aps_hr_score = aps_hr_score,
        aps_rr_score = aps_rr_score,
        aps_oxy_score = aps_oxy_score,
        aps_ph_score = aps_ph_score,
        aps_sodium_score = aps_sodium_score,
        aps_potassium_score = aps_potassium_score,
        aps_creatinine_score = aps_creatinine_score,
        aps_hct_score = aps_hct_score,
        aps_wbc_score = aps_wbc_score,
        aps_gcs_score = aps_gcs_score
      )
    ) |>
    select(record_id, sys_global_phys_sev_0)

  # Return data with one row per record_id
  data |>
    distinct(record_id) |>
    left_join(data_aps_var, by = 'record_id')
}
