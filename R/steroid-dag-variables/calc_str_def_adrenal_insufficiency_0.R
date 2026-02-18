## -----------------------------------------------------------------------------
## Definite Adrenal Insufficiency (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Definite Adrenal Insufficiency' for Day 0
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_def_adrenal_insufficiency_0 <- function(
  m_endocrine,
  m_endo_conditions___2
) {
  dplyr::case_when(
    m_endocrine %in% c("Yes", "No") & is_unchecked(m_endo_conditions___2) ~ 0,
    m_endocrine == "Yes" & is_checked(m_endo_conditions___2) ~ 1,
    is_unknown(m_endocrine) ~ 99
  )
}

# Convenience wrapper function
# Returns a data frame with record_id and str_def_adrenal_insufficiency_0 columns (one row per record_id)
wrapper_calc_str_def_adrenal_insufficiency_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_def_adrenal_insufficiency_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_def_adrenal_insufficiency_0 = calc_str_def_adrenal_insufficiency_0(
          m_endocrine = m_endocrine,
          m_endo_conditions___2 = m_endo_conditions___2
        )) |>
        select(record_id, str_def_adrenal_insufficiency_0),
      by = 'record_id'
    )
}
