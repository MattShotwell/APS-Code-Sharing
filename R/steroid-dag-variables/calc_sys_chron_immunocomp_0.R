## -----------------------------------------------------------------------------
## Chronic Immunocompromise (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Chronic Immunocompromise'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_chron_immunocomp_0 <- function(
  m_immunosuppression,
  m_immunosup_conditions___2,
  m_immunosup_conditions___3,
  m_immunosup_conditions___4,
  m_immunosup_conditions___5,
  m_immunosup_conditions___6,
  m_immunosup_conditions___7,
  m_immunosup_conditions___88
) {
  dplyr::case_when(
    m_immunosuppression %in% c("Yes", "No") &
      is_unchecked(m_immunosup_conditions___2) &
      is_unchecked(m_immunosup_conditions___3) &
      is_unchecked(m_immunosup_conditions___4) &
      is_unchecked(m_immunosup_conditions___5) &
      is_unchecked(m_immunosup_conditions___6) &
      is_unchecked(m_immunosup_conditions___7) &
      is_unchecked(m_immunosup_conditions___88) ~ 0,

    m_immunosuppression == "Yes" & (
      is_checked(m_immunosup_conditions___2) |
      is_checked(m_immunosup_conditions___3) |
      is_checked(m_immunosup_conditions___4) |
      is_checked(m_immunosup_conditions___5) |
      is_checked(m_immunosup_conditions___6) |
      is_checked(m_immunosup_conditions___7) |
      is_checked(m_immunosup_conditions___88)
      ) ~ 1,

    is_unknown(m_immunosuppression) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_chron_immunocomp_0 columns (one row per record_id)
wrapper_calc_sys_chron_immunocomp_0 <- function(data) {
  data |>
    distinct(record_id) |>

    left_join(
      data |>
        filter(event_label == 'Day 0') |>
        mutate(sys_chron_immunocomp_0 = calc_sys_chron_immunocomp_0(
          m_immunosuppression = m_immunosuppression,
          m_immunosup_conditions___2 = m_immunosup_conditions___2,
          m_immunosup_conditions___3 = m_immunosup_conditions___3,
          m_immunosup_conditions___4 = m_immunosup_conditions___4,
          m_immunosup_conditions___5 = m_immunosup_conditions___5,
          m_immunosup_conditions___6 = m_immunosup_conditions___6,
          m_immunosup_conditions___7 = m_immunosup_conditions___7,
          m_immunosup_conditions___88 = m_immunosup_conditions___88
        )) |>
        select(record_id, sys_chron_immunocomp_0),
      by = 'record_id'
    )
}
