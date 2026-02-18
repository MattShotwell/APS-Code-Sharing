## -----------------------------------------------------------------------------
## Presence of Pneumonia Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of Pneumonia Syndrome'
#
# Values:
# - 0 = No
# - 1 = Yes
# - 99 = Unknown
calc_sys_pneumonia_0 <- function(
  pna_clinical_judgement
  ) {

  ## Determine pneumonia presence
  dplyr::case_when(
    pna_clinical_judgement == "No" ~ 0,
    pna_clinical_judgement == "Yes" ~ 1,
    is_unknown(pna_clinical_judgement) ~ 99
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_pneumonia_0 columns (one row per record_id)
wrapper_calc_sys_pneumonia_0 <- function(data) {
  data_with_pneumonia <- data |>
    filter(event_label == 'Syndrome Adjudication') |>
    mutate(
      sys_pneumonia_0 = calc_sys_pneumonia_0(
        pna_clinical_judgement
      )
    ) |>
    select(record_id, sys_pneumonia_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_pneumonia, by = 'record_id')
}
