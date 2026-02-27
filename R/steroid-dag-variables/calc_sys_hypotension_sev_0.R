## -----------------------------------------------------------------------------
## Hypotension Severity (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Hypotension Severity'
#
# Values:
# - 0 = No pressors or inotrope, MAP >= 70
# - 1 = No pressors or inotrope, MAP < 70
# - 2 = On dobutamine/milrinone only (with low/no dopamine)
# - 3 = On vasopressors with NEE <= 0.1
# - 4 = NEE > 0.1
calc_sys_hypotension_sev_0 <- function(
  daily_vasopressors_0___0,
  daily_vasopressors_0___1,
  daily_vasopressors_0___2,
  daily_vasopressors_0___3,
  daily_vasopressors_0___4,
  daily_vasopressors_0___5,
  daily_vasopressors_0___6,
  daily_vasopressors_0___7,
  daily_vasopressors_0___8,
  daily_vasopressors_0___88,

  daily_sbp_8a_0,
  daily_dbp_8a_0,

  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_phen_dose_8a_0_mcg,
  daily_phen_dos_8a_0_mcgkg,
  daily_vaso_dose_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dos_8a_0_mcgkg,
  daily_dobuta_8a_0_mcg,
  daily_dobuta_8a_0_mcgkg,
  daily_ang2_8a_0_mcg,
  daily_ang2_8a_0_mcgkg,
  daily_milr_8a_0_mcg,
  daily_milr_8a_0_mcgkg,

  m_weight_kg
) {

  ## Calculate MAP from SBP and DBP: MAP = (2 * DBP + SBP) / 3
  map <- (2 * daily_dbp_8a_0 + daily_sbp_8a_0) / 3

  ## Calculate norepinephrine equivalents (for vasopressors only)
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

  ## Calculate dopamine dose in mcg/min/kg for threshold check
  dopa_dose_mcgkg <- dplyr::coalesce(daily_dopa_dos_8a_0_mcgkg, daily_dopa_dose_8a_0_mcg / m_weight_kg, 0)

  ## Calculate dobutamine and milrinone doses
  dobuta_dose <- dplyr::coalesce(daily_dobuta_8a_0_mcg, daily_dobuta_8a_0_mcgkg * m_weight_kg, 0)
  milr_dose <- dplyr::coalesce(daily_milr_8a_0_mcg, daily_milr_8a_0_mcgkg * m_weight_kg, 0)

  ## Check if on any vasopressors (NE, epi, phenylephrine, vasopressin, angiotensin II)
  on_vasopressors <- is_checked(daily_vasopressors_0___1) | # norepinephrine
                        is_checked(daily_vasopressors_0___2) | # epinephrine
                        is_checked(daily_vasopressors_0___3) | # phenylephrine
                        is_checked(daily_vasopressors_0___4) | # vasopressin
                        is_checked(daily_vasopressors_0___5) | # dopamine
                        is_checked(daily_vasopressors_0___6) | # dobutamine
                        is_checked(daily_vasopressors_0___7) | # angiotensin II
                        is_checked(daily_vasopressors_0___8) | # milrinone
                        is_checked(daily_vasopressors_0___88)  # Other

  ## Check if on OTHER vasopressors (excludes dopamine, dobutamine, milrinone)
  on_other_vasopressors <- is_checked(daily_vasopressors_0___1) | # norepinephrine
                           is_checked(daily_vasopressors_0___2) | # epinephrine
                           is_checked(daily_vasopressors_0___3) | # phenylephrine
                           is_checked(daily_vasopressors_0___4) | # vasopressin
                           is_checked(daily_vasopressors_0___7) | # angiotensin II
                           is_checked(daily_vasopressors_0___88)  # Other

  dplyr::case_when(
    # Score 0: No pressors/inotropes, MAP ≥ 70
    is_checked(daily_vasopressors_0___0) & (map >= 70) ~ 0,

    # Score 1: No pressors/inotropes, MAP < 70
    is_checked(daily_vasopressors_0___0) & (map < 70) ~ 1,

    # Score 2: (dobutamine/milrinone) AND (no or <= 5 dopamine) AND (no other vasopressors)
    (is_checked(daily_vasopressors_0___6) | is_checked(daily_vasopressors_0___8)) &
        (dopa_dose_mcgkg <= 5) & !on_other_vasopressors ~ 2,

    # Score 3: On vasopressors with NEE ≤ 0.1
    on_vasopressors & (norepi_equiv <= 0.1) ~ 3,

    # Score 4: NEE > 0.1
    norepi_equiv > 0.1 ~ 4,
  )
}

# Convenience wrapper function
# Returns a data frame with record_id and sys_hypotension_sev_0 columns (one row per record_id)
wrapper_calc_sys_hypotension_sev_0 <- function(data) {
  data_with_hypotension_sev <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    # Remove the weight variable and merge it back in from the "Day 0" event
    select(-m_weight_kg) |>
    left_join(data |>
      filter(event_label == 'Day 0') |>
      select(record_id, m_weight_kg),
      by = 'record_id') |>
    mutate(
      sys_hypotension_sev_0 = calc_sys_hypotension_sev_0(
        daily_vasopressors_0___0,
        daily_vasopressors_0___1,
        daily_vasopressors_0___2,
        daily_vasopressors_0___3,
        daily_vasopressors_0___4,
        daily_vasopressors_0___5,
        daily_vasopressors_0___6,
        daily_vasopressors_0___7,
        daily_vasopressors_0___8,
        daily_vasopressors_0___88,

        daily_sbp_8a_0,
        daily_dbp_8a_0,

        daily_ne_dose_8a_0_mcg,
        daily_ne_dose_8a_0_mcgkg,
        daily_epi_dose_8a_0_mcg,
        daily_epi_dose_8a_0_mcgkg,
        daily_phen_dose_8a_0_mcg,
        daily_phen_dos_8a_0_mcgkg,
        daily_vaso_dose_8a_0,
        daily_dopa_dose_8a_0_mcg,
        daily_dopa_dos_8a_0_mcgkg,
        daily_dobuta_8a_0_mcg,
        daily_dobuta_8a_0_mcgkg,
        daily_ang2_8a_0_mcg,
        daily_ang2_8a_0_mcgkg,
        daily_milr_8a_0_mcg,
        daily_milr_8a_0_mcgkg,

        m_weight_kg
      )
    ) |>
    select(record_id, sys_hypotension_sev_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_hypotension_sev, by = 'record_id')
}
