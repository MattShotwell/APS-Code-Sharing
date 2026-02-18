## -----------------------------------------------------------------------------
## Chronic Neuromuscular Disease (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Chronic Neuromuscular Disease'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_str_neuromuscular_disease_0 <- function(
  m_neurologic_conditions___6,
  m_neurologic
  ) {

  dplyr::case_when(
    is_checked(m_neurologic_conditions___6) ~ 1,
    m_neurologic == "Yes" & is_unchecked(m_neurologic_conditions___6) ~ 0,
    m_neurologic == "No" ~ 0,
    is_unknown(m_neurologic) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_neuromuscular_disease_0 columns (one row per record_id)
wrapper_calc_str_neuromuscular_disease_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_neuromuscular_disease_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_neuromuscular_disease_0 = calc_str_neuromuscular_disease_0(
          m_neurologic_conditions___6 = m_neurologic_conditions___6,
          m_neurologic = m_neurologic
        )) |>
        select(record_id, str_neuromuscular_disease_0),
      by = 'record_id'
    )
}
