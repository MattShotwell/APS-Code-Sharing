## -----------------------------------------------------------------------------
## SCAP Criterion: Respiratory Rate (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Respiratory Rate'
#
# Values:
# - 0 = No (Respiratory rate <= 30)
# - 1 = Yes (Respiratory rate > 30)
calc_str_scap_rr_0 <- function(
  daily_resp_8a_0_code,
  daily_resp_8a_m1_code,
  daily_resp_8a_m2_code,

  daily_hfnc_rr_8a_0,
  daily_hfnc_rr_8a_m1,
  daily_hfnc_rr_8a_m2,

  daily_niv_rr_8a_0,
  daily_niv_rr_8a_m1,
  daily_niv_rr_8a_m2,

  daily_standard_rr_8a_0,
  daily_standard_rr_8a_m1,
  daily_standard_rr_8a_m2,

  daily_resp_rate_8a_0,
  daily_resp_rate_8a_m1,
  daily_resp_rate_8a_m2,

  highrr_vsorres
  ) {

  ## Helper function to get RR based on respiratory support type
  get_rr_for_support <- function(resp_code, imv_rr, niv_rr, hfnc_rr, standard_rr, high_rr) {
    dplyr::case_when(
      resp_code == 1 ~ imv_rr,       # ECMO and IMV
      resp_code == 2 ~ high_rr,      # ECMO without IMV
      resp_code == 3 ~ imv_rr,       # IMV without ECMO
      resp_code == 4 ~ niv_rr,       # Non-invasive ventilation
      resp_code == 5 ~ hfnc_rr,      # High-flow nasal oxygen (HFNC)
      resp_code == 6 ~ standard_rr,  # Standard flow supplemental oxygen
      resp_code == 7 ~ high_rr,      # No respiratory support (room air)
      resp_code == 99 ~ high_rr      # Unknown
    )
  }

  ## Get RR for each day
  rr_0 <- get_rr_for_support(
    daily_resp_8a_0_code,
    daily_resp_rate_8a_0,
    daily_niv_rr_8a_0,
    daily_hfnc_rr_8a_0,
    daily_standard_rr_8a_0,
    highrr_vsorres
  )

  rr_m1 <- get_rr_for_support(
    daily_resp_8a_m1_code,
    daily_resp_rate_8a_m1,
    daily_niv_rr_8a_m1,
    daily_hfnc_rr_8a_m1,
    daily_standard_rr_8a_m1,
    highrr_vsorres
  )

  rr_m2 <- get_rr_for_support(
    daily_resp_8a_m2_code,
    daily_resp_rate_8a_m2,
    daily_niv_rr_8a_m2,
    daily_hfnc_rr_8a_m2,
    daily_standard_rr_8a_m2,
    highrr_vsorres
  )

  ## Get final RR with lookback (Day 0 -> Day -1 -> Day -2)
  rr <- get_value_with_lookback(rr_0, rr_m1, rr_m2)

  ## Apply SCAP criterion
  dplyr::case_when(
    rr <= 30 ~ 0,
    rr > 30 ~ 1
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_scap_rr_0 columns (one row per record_id)
wrapper_calc_str_scap_rr_0 <- function(data, dictionary) {
  data_with_highrr_vsorres <- data |>
    filter(event_label == 'Day 0') |>
    select(
      record_id,
      highrr_vsorres
    )

  data_with_scap_rr <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    left_join(
      get_code_label_map("daily_resp_8a_0", dictionary),
      by = "daily_resp_8a_0"
    ) |>
    left_join(
      get_code_label_map("daily_resp_8a_m1", dictionary),
      by = "daily_resp_8a_m1"
    ) |>
    left_join(
      get_code_label_map("daily_resp_8a_m2", dictionary),
      by = "daily_resp_8a_m2"
    ) |>
    select(
      record_id,

      daily_resp_8a_0_code,
      daily_resp_8a_m1_code,
      daily_resp_8a_m2_code,

      daily_hfnc_rr_8a_0,
      daily_hfnc_rr_8a_m1,
      daily_hfnc_rr_8a_m2,

      daily_niv_rr_8a_0,
      daily_niv_rr_8a_m1,
      daily_niv_rr_8a_m2,

      daily_standard_rr_8a_0,
      daily_standard_rr_8a_m1,
      daily_standard_rr_8a_m2,

      daily_resp_rate_8a_0,
      daily_resp_rate_8a_m1,
      daily_resp_rate_8a_m2
    ) |>
    left_join(
      data_with_highrr_vsorres,
      by = "record_id"
    ) |>
    mutate(
      str_scap_rr_0 = calc_str_scap_rr_0(
        daily_resp_8a_0_code,
        daily_resp_8a_m1_code,
        daily_resp_8a_m2_code,

        daily_hfnc_rr_8a_0,
        daily_hfnc_rr_8a_m1,
        daily_hfnc_rr_8a_m2,

        daily_niv_rr_8a_0,
        daily_niv_rr_8a_m1,
        daily_niv_rr_8a_m2,

        daily_standard_rr_8a_0,
        daily_standard_rr_8a_m1,
        daily_standard_rr_8a_m2,

        daily_resp_rate_8a_0,
        daily_resp_rate_8a_m1,
        daily_resp_rate_8a_m2,

        highrr_vsorres
      )
    ) |>
    select(record_id, str_scap_rr_0)

  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>
    left_join(data_with_scap_rr, by = 'record_id')
}
