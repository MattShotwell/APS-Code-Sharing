## -----------------------------------------------------------------------------
## Respiratory Failure Severity (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Respiratory Failure Severity: Respiratory Support Type' for Day 0
#
# Values:
# - 1 = No respiratory support
# - 2 = Standard flow oxygen
# - 3 = High-flow nasal cannula (HFNC)
# - 4 = Non-invasive ventilation (NIV)
# - 5 = Invasive mechanical ventilation (IMV) with PEEP < 12 cm H2O, no ECMO
# - 6 = IMV with PEEP ≥ 12 cm H2O, or ECMO
# - 99 = Unknown
calc_resp_support_type <- function(
  daily_resp_8a_code,
  dailysofa_perf,
  daily_standard_flow_8a,
  daily_hfnc_fi02_8a,
  daily_niv_fi02_8a,
  daily_imv_fio2_8a,
  daily_epap_8a
) {

  dplyr::case_when(
    # ECMO
    daily_resp_8a_code %in% c(1, 2) ~ 6,
    # IMV with PEEP >= 12 (same code as ECMO)
    daily_resp_8a_code == 3 & !is.na(daily_imv_fio2_8a) & daily_epap_8a >= 12 ~ 6,
    # IMV with PEEP < 12
    daily_resp_8a_code == 3 & !is.na(daily_imv_fio2_8a) & daily_epap_8a < 12 ~ 5,
    # NIV
    (daily_resp_8a_code == 4) & !is.na(daily_niv_fi02_8a) ~ 4,
    # HFNC
    (daily_resp_8a_code == 5) & !is.na(daily_hfnc_fi02_8a) ~ 3,
    # Standard flow
    (daily_resp_8a_code == 6) & !is.na(daily_standard_flow_8a) ~ 2,
    # No respiratory support on day 0 or not applicable
    (daily_resp_8a_code == 7 | dailysofa_perf == 'Not Available') ~ 1,
    # Unknown
    daily_resp_8a_code == 99 ~ 99
  )
}

# Calculate SYSTEMATIC DAG 'Respiratory Failure Severity: S/F Ratio' for Day 0
#
# Value: SpO2/FiO2 ratio calculated as:
# - Resp Support Type 1 (no support): SpO2 / 0.21
# - Resp Support Type 2 (standard flow): SpO2 / (0.21 + 0.03 × O2 flow rate)
# - Resp Support Types 3-6 (HFNC/NIV/IMV): SpO2 / FiO2
calc_sfratio_8a_0 <- function(
  resp_support_type_0, # Calculated by calc_resp_support_type()
  daily_spo2_8a_0,
  daily_standard_flow_8a_0,
  daily_hfnc_fi02_8a_0,
  daily_niv_fi02_8a_0,
  daily_imv_fio2_8a_0
) {

  dplyr::case_when(
    resp_support_type_0 == 1 ~ daily_spo2_8a_0 / 0.21,
    resp_support_type_0 == 2 ~ daily_spo2_8a_0 / (0.21 + (0.03 * daily_standard_flow_8a_0)),
    resp_support_type_0 == 3 ~ daily_spo2_8a_0 / daily_hfnc_fi02_8a_0,
    resp_support_type_0 == 4 ~ daily_spo2_8a_0 / daily_niv_fi02_8a_0,
    resp_support_type_0 %in% c(5, 6) & !is.na(daily_imv_fio2_8a_0) ~ daily_spo2_8a_0 / daily_imv_fio2_8a_0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id, resp_support_type_0, and sfratio_8a_0 columns (one row per record_id)
wrapper_calc_sys_resp_failure_sev_0 <- function(data, dictionary) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate resp_support_type and sfratio_8a_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        left_join(
          get_code_label_map('daily_resp_8a_0', dictionary),
          by = 'daily_resp_8a_0'
        ) |>
        mutate(
          resp_support_type_0 = calc_resp_support_type(
            daily_resp_8a_code = daily_resp_8a_0_code,
            dailysofa_perf = dailysofa_perf_0,
            daily_standard_flow_8a = daily_standard_flow_8a_0,
            daily_hfnc_fi02_8a = daily_hfnc_fi02_8a_0,
            daily_niv_fi02_8a = daily_niv_fi02_8a_0,
            daily_imv_fio2_8a = daily_imv_fio2_8a_0,
            daily_epap_8a = daily_epap_8a_0
          ),
          sfratio_8a_0 = calc_sfratio_8a_0(
            resp_support_type_0 = resp_support_type_0,
            daily_spo2_8a_0 = daily_spo2_8a_0,
            daily_standard_flow_8a_0 = daily_standard_flow_8a_0,
            daily_hfnc_fi02_8a_0 = daily_hfnc_fi02_8a_0,
            daily_niv_fi02_8a_0 = daily_niv_fi02_8a_0,
            daily_imv_fio2_8a_0 = daily_imv_fio2_8a_0
          )
        ) |>
        select(record_id, resp_support_type_0, sfratio_8a_0),
      by = 'record_id'
    )
}
