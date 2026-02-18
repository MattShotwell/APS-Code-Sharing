## -----------------------------------------------------------------------------
## Septic Shock (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Septic Shock'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_sys_septic_shock_0 <- function(
  sys_sepsis_0,
  daily_vasopressors_0___0,
  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_phen_dose_8a_0_mcg,
  daily_phen_dos_8a_0_mcgkg,
  daily_vaso_dose_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dos_8a_0_mcgkg,
  daily_ang2_8a_0_mcg,
  daily_ang2_8a_0_mcgkg,
  m_weight_kg
) {

  ## Calculate norepinephrine equivalents
  norepi_equiv <- calc_norepi_equivalents(
    daily_ne_dose_8a_0_mcg = daily_ne_dose_8a_0_mcg,
    daily_ne_dose_8a_0_mcgkg = daily_ne_dose_8a_0_mcgkg,
    daily_epi_dose_8a_0_mcg = daily_epi_dose_8a_0_mcg,
    daily_epi_dose_8a_0_mcgkg = daily_epi_dose_8a_0_mcgkg,
    daily_phen_dose_8a_0_mcg = daily_phen_dose_8a_0_mcg,
    daily_phen_dos_8a_0_mcgkg = daily_phen_dos_8a_0_mcgkg,
    daily_vaso_dose_8a_0 = daily_vaso_dose_8a_0,
    daily_dopa_dose_8a_0_mcg = daily_dopa_dose_8a_0_mcg,
    daily_dopa_dos_8a_0_mcgkg = daily_dopa_dos_8a_0_mcgkg,
    daily_ang2_8a_0_mcg = daily_ang2_8a_0_mcg,
    daily_ang2_8a_0_mcgkg = daily_ang2_8a_0_mcgkg,
    m_weight_kg = m_weight_kg
  )

  dplyr::case_when(
    sys_sepsis_0 == 1 &
      is_unchecked(daily_vasopressors_0___0) &
      (norepi_equiv > 0.5) ~ 1,
    .default = 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_septic_shock_0 columns (one row per record_id)
wrapper_calc_sys_septic_shock_0 <- function(data, dictionary) {
  # Get sepsis status using the sepsis function
  data_with_sepsis <- wrapper_calc_sys_sepsis_0(data, dictionary)

  # Get weight from Day 0
  data_with_weight <- data |>
    filter(event_label == 'Day 0') |>
    select(record_id, m_weight_kg)

  # Get vasopressor data from Daily In-Hospital Forms
  data_with_vasopressors <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    select(
      record_id,
      daily_vasopressors_0___0,
      daily_ne_dose_8a_0_mcg,
      daily_ne_dose_8a_0_mcgkg,
      daily_epi_dose_8a_0_mcg,
      daily_epi_dose_8a_0_mcgkg,
      daily_phen_dose_8a_0_mcg,
      daily_phen_dos_8a_0_mcgkg,
      daily_vaso_dose_8a_0,
      daily_dopa_dose_8a_0_mcg,
      daily_dopa_dos_8a_0_mcgkg,
      daily_ang2_8a_0_mcg,
      daily_ang2_8a_0_mcgkg
    )

  data_with_sys_septic_shock <- data_with_weight |>
    left_join(data_with_vasopressors, by = 'record_id') |>
    left_join(data_with_sepsis, by = 'record_id') |>
    mutate(
      sys_septic_shock_0 = calc_sys_septic_shock_0(
        sys_sepsis_0 = sys_sepsis_0,
        daily_vasopressors_0___0 = daily_vasopressors_0___0,
        daily_ne_dose_8a_0_mcg = daily_ne_dose_8a_0_mcg,
        daily_ne_dose_8a_0_mcgkg = daily_ne_dose_8a_0_mcgkg,
        daily_epi_dose_8a_0_mcg = daily_epi_dose_8a_0_mcg,
        daily_epi_dose_8a_0_mcgkg = daily_epi_dose_8a_0_mcgkg,
        daily_phen_dose_8a_0_mcg = daily_phen_dose_8a_0_mcg,
        daily_phen_dos_8a_0_mcgkg = daily_phen_dos_8a_0_mcgkg,
        daily_vaso_dose_8a_0 = daily_vaso_dose_8a_0,
        daily_dopa_dose_8a_0_mcg = daily_dopa_dose_8a_0_mcg,
        daily_dopa_dos_8a_0_mcgkg = daily_dopa_dos_8a_0_mcgkg,
        daily_ang2_8a_0_mcg = daily_ang2_8a_0_mcg,
        daily_ang2_8a_0_mcgkg = daily_ang2_8a_0_mcgkg,
        m_weight_kg = m_weight_kg
      )
    ) |>
    select(record_id, sys_septic_shock_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_sys_septic_shock, by = 'record_id')
}
