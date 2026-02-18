## -----------------------------------------------------------------------------
## Chronic Moderate-Dose Steroid Use (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Chronic Moderate-Dose Steroid Use' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_chron_steroid_moddose_0 <- function(
  mhccster,
  m_endo_conditions___2,
  m_endocrine
) {
  dplyr::case_when(
    mhccster == "No" |
        (m_endocrine == "Yes" & is_checked(m_endo_conditions___2)) ~ 0,

    mhccster == "Yes" &
        m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 1,

    is_unknown(mhccster) | is_unknown(m_endocrine) ~ 99
  )
}

# Convenience wrapper function
# Returns a data frame with record_id and str_chron_steroid_moddose_0 columns (one row per record_id)
wrapper_calc_str_chron_steroid_moddose_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_chron_steroid_moddose_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_chron_steroid_moddose_0 = calc_str_chron_steroid_moddose_0(
          mhccster = mhccster,
          m_endo_conditions___2 = m_endo_conditions___2,
          m_endocrine = m_endocrine
        )) |>
        select(record_id, str_chron_steroid_moddose_0),
      by = 'record_id'
    )
}
