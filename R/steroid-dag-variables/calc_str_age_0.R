## -----------------------------------------------------------------------------
## Age (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Age' for Day 0
#
# Value: Age in years
calc_str_age_0 <- function(
  age
) {
  dplyr::case_when(
    !is.na(age) & age >= 0 ~ age
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_age_0 columns (one row per record_id)
wrapper_calc_str_age_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_age_0 and join back to record_id
      data |>
        filter(event_label == 'Day 0') |>
        mutate(str_age_0 = calc_str_age_0(
          age = age
        )) |>
        select(record_id, str_age_0),
      by = 'record_id'
    )
}
