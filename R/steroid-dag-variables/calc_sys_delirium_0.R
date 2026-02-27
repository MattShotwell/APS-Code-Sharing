## -----------------------------------------------------------------------------
## Delirium (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Delirium'
#
# Values:
# - 0 = CAM done and delirium not present
# - 1 = CAM done and delirium present
# - 2 = CAM not done
calc_sys_delirium_0 <- function(
  # Core inputs
  cam_0,
  cam_m1,
  cam_m2,
  # Branching logic
  lowrass_orres_0,
  lowrass_orres_m1,
  lowrass_orres_m2,
  highestrass_orres_0,
  highestrass_orres_m1,
  highestrass_orres_m2
  ) {

  positive_text <- "Positive for delirium at least once on this day"
  negative_text <- "Negative for delirium on all assessments on this day"
  not_done_text <- "Not done"

  ## Check if all CAM assessments are negative
  all_negative <- (
    cam_m2 == negative_text &
    cam_m1 == negative_text &
    cam_0 == negative_text
  )

  ## Check if any CAM assessment is positive
  any_positive <- (
    cam_m2 == positive_text |
    cam_m1 == positive_text |
    cam_0 == positive_text
  )

  ## Check if any CAM assessment was "Not done"
  any_not_done <- (
    cam_m2 == "Not done" |
    cam_m1 == "Not done" |
    cam_0 == "Not done"
  )

  ## Check branching logic: cam should be visible if RASS scores are available and not both -4/-5
  ## For each day, check if cam is missing because branching logic was NOT satisfied (correctly hidden)

  # cam_0 branching logic: visible if both RASS scores exist and at least one is not -4 or -5
  cam_0_should_be_visible <- (!is.na(lowrass_orres_0) & !is.na(highestrass_orres_0)) &
                             ((lowrass_orres_0 != -4 & lowrass_orres_0 != -5) |
                              (highestrass_orres_0 != -4 & highestrass_orres_0 != -5))
  cam_0_correctly_missing <- is.na(cam_0) & !cam_0_should_be_visible

  # cam_m1 branching logic: visible if both RASS scores exist and at least one is not -4 or -5
  cam_m1_should_be_visible <- (!is.na(lowrass_orres_m1) & !is.na(highestrass_orres_m1)) &
                              ((lowrass_orres_m1 != -4 & lowrass_orres_m1 != -5) |
                               (highestrass_orres_m1 != -4 & highestrass_orres_m1 != -5))
  cam_m1_correctly_missing <- is.na(cam_m1) & !cam_m1_should_be_visible

  # cam_m2 branching logic: visible if both RASS scores exist and at least one is not -4 or -5
  cam_m2_should_be_visible <- (!is.na(lowrass_orres_m2) & !is.na(highestrass_orres_m2)) &
                              ((lowrass_orres_m2 != -4 & lowrass_orres_m2 != -5) |
                               (highestrass_orres_m2 != -4 & highestrass_orres_m2 != -5))
  cam_m2_correctly_missing <- is.na(cam_m2) & !cam_m2_should_be_visible

  ## Check if any cam is correctly missing due to branching logic not being satisfied
  any_correctly_missing_due_to_branching <- cam_0_correctly_missing |
                                            cam_m1_correctly_missing |
                                            cam_m2_correctly_missing

  ## Return result
  dplyr::case_when(
    all_negative ~ 0, # All negative
    any_positive ~ 1, # Delirium present (at least one positive)
    any_not_done ~ 2, # At least one "Not done"
    any_correctly_missing_due_to_branching ~ 2 # CAM correctly missing due to branching logic
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_delirium_0 columns (one row per record_id)
wrapper_calc_sys_delirium_0 <- function(data) {
  data_with_delirium <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    mutate(
      sys_delirium_0 = calc_sys_delirium_0(
        cam_0 = cam_0,
        cam_m1 = cam_m1,
        cam_m2 = cam_m2,

        lowrass_orres_0 = lowrass_orres_0,
        lowrass_orres_m1 = lowrass_orres_m1,
        lowrass_orres_m2 = lowrass_orres_m2,

        highestrass_orres_0 = highestrass_orres_0,
        highestrass_orres_m1 = highestrass_orres_m1,
        highestrass_orres_m2 = highestrass_orres_m2
      )
    ) |>
    select(record_id, sys_delirium_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_delirium, by = 'record_id')
}
