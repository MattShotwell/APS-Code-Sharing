## -----------------------------------------------------------------------------
## Chronic Moderate-Dose Steroid Use (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Chronic Moderate-Dose Steroid Use' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_chron_steroid_moddose_0 <- function(
  mhccster,
  m_endo_conditions___2,
  m_immunosup_conditions___1,
  m_endocrine,
  m_immunosuppression
) {
  dplyr::case_when(
    mhccster == "No" |
        (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) |
        (m_immunosuppression == "Yes" & is_checked(m_immunosup_conditions___1)) ~ 0,

    mhccster == "Yes" &
        m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) &
        m_immunosuppression %in% c("Yes", "No") & is_unchecked(m_immunosup_conditions___1) ~ 1,

    is_unknown(mhccster) | is_unknown(m_endocrine) | is_unknown(m_immunosuppression) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_chron_steroid_moddose_0 columns (one row per record_id)
wrapper_calc_sys_chron_steroid_moddose_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate sys_chron_steroid_moddose_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(sys_chron_steroid_moddose_0 = calc_sys_chron_steroid_moddose_0(
          mhccster = mhccster,
          m_endo_conditions___2 = m_endo_conditions___2,
          m_immunosup_conditions___1 = m_immunosup_conditions___1,
          m_endocrine = m_endocrine,
          m_immunosuppression = m_immunosuppression
        )) |>
        select(record_id, sys_chron_steroid_moddose_0),
      by = 'record_id'
    )
}
