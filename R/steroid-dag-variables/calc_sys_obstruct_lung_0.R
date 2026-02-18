## -----------------------------------------------------------------------------
## Obstructive Lung Disease (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Obstructive Lung Disease'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_obstruct_lung_0 <- function(
  m_pulmonary,
  m_pulm_conditions___1,
  m_pulm_conditions___3
) {
  dplyr::case_when(
    m_pulmonary == "Yes" &
      (is_checked(m_pulm_conditions___1) | is_checked(m_pulm_conditions___3)) ~ 1,
    m_pulmonary %in% c("Yes", "No") &
      (is_unchecked(m_pulm_conditions___1) & is_unchecked(m_pulm_conditions___3)) ~ 0,
    is_unknown(m_pulmonary) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_obstruct_lung_0 columns (one row per record_id)
wrapper_calc_sys_obstruct_lung_0 <- function(data) {
  data |>
    distinct(record_id) |>

    left_join(
      data |>
        filter(event_label == 'Day 0') |>
        mutate(sys_obstruct_lung_0 = calc_sys_obstruct_lung_0(
          m_pulmonary = m_pulmonary,
          m_pulm_conditions___1 = m_pulm_conditions___1,
          m_pulm_conditions___3 = m_pulm_conditions___3
        )) |>
        select(record_id, sys_obstruct_lung_0),
      by = 'record_id'
    )
}
