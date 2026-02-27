## -----------------------------------------------------------------------------
## Presence of ARDS Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of ARDS Syndrome'
#
# Values:
# - 0 = Syndrome criteria not all present
# - 1 = Syndrome criteria all present
# - NA = Unknown/insufficient data
calc_sys_ards_0 <- function(
  d0_pos,
  dm1_pos,
  dm2_pos,
  daily_spo2_lowest_0,
  daily_fio2_lowest_0,
  daily_resp_lowest_0_code,
  daily_pa02_lowest_0,
  daily_fio2_lowest_pao2_0,
  daily_resp_lowest_pao2_0_code
) {

  ## Criterion #1: Bilateral opacities on or before Day 0
  criterion_1 <- (d0_pos == 1 | dm1_pos == 1 | dm2_pos == 1)
  
  ## Criterion #2 Option A: SpO2-based hypoxemia with qualifying respiratory support
  # Qualifying respiratory support codes: 1=ECMO+IMV, 3=IMV, 4=NIV, 5=HFNC
  option_a <- (
    !is.na(daily_spo2_lowest_0) & 
    !is.na(daily_fio2_lowest_0) & 
    !is.na(daily_resp_lowest_0_code) &
    daily_spo2_lowest_0 <= 97 & 
    (daily_spo2_lowest_0 / daily_fio2_lowest_0) <= 315 & 
    daily_resp_lowest_0_code %in% c(1, 3, 4, 5)
  )
  
  ## Criterion #2 Option B: PaO2-based hypoxemia with qualifying respiratory support
  option_b <- (
    !is.na(daily_pa02_lowest_0) & 
    !is.na(daily_fio2_lowest_pao2_0) & 
    !is.na(daily_resp_lowest_pao2_0_code) &
    (daily_pa02_lowest_0 / daily_fio2_lowest_pao2_0) <= 300 & 
    daily_resp_lowest_pao2_0_code %in% c(1, 3, 4, 5)
  )
  
  ## Criterion #2: Either Option A or Option B is met
  criterion_2 <- (option_a | option_b)
  
  ## Determine ARDS status
  dplyr::case_when(
    # Both criteria met -> ARDS present
    criterion_1 & criterion_2 ~ 1,
    # Either criterion definitively not met -> ARDS not present
    (!is.na(criterion_1) & !criterion_1) | (!is.na(criterion_2) & !criterion_2) ~ 0,
    # Otherwise -> Unknown
    TRUE ~ NA_real_
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_ards_0 columns (one row per record_id)
wrapper_calc_sys_ards_0 <- function(data, dictionary) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Get bilateral opacity data from Syndrome Adjudication event
      data |>
        filter(event_label == 'Syndrome Adjudication') |>
        select(record_id, d0_pos, dm1_pos, dm2_pos),
      by = 'record_id'
    ) |>
    
    left_join(
      # Get respiratory data from Daily Assessment Day 0
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        left_join(
          get_code_label_map('daily_resp_lowest_0', dictionary),
          by = 'daily_resp_lowest_0'
        ) |>
        left_join(
          get_code_label_map('daily_resp_lowest_pao2_0', dictionary),
          by = 'daily_resp_lowest_pao2_0'
        ) |>
        select(
          record_id,
          daily_spo2_lowest_0,
          daily_fio2_lowest_0,
          daily_resp_lowest_0_code,
          daily_pa02_lowest_0,
          daily_fio2_lowest_pao2_0,
          daily_resp_lowest_pao2_0_code
        ),
      by = 'record_id'
    ) |>
    
    # Calculate sys_ards_0 after all joins are complete
    mutate(sys_ards_0 = calc_sys_ards_0(
      d0_pos = d0_pos,
      dm1_pos = dm1_pos,
      dm2_pos = dm2_pos,
      daily_spo2_lowest_0 = daily_spo2_lowest_0,
      daily_fio2_lowest_0 = daily_fio2_lowest_0,
      daily_resp_lowest_0_code = daily_resp_lowest_0_code,
      daily_pa02_lowest_0 = daily_pa02_lowest_0,
      daily_fio2_lowest_pao2_0 = daily_fio2_lowest_pao2_0,
      daily_resp_lowest_pao2_0_code = daily_resp_lowest_pao2_0_code
    )) |>
    select(record_id, sys_ards_0)
}
