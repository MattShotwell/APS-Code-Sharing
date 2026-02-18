## -----------------------------------------------------------------------------
## Inflammatory Profile (Systematic DAG)
## -----------------------------------------------------------------------------


# Calculate SYSTEMATIC DAG 'Inflammatory Profile: CRP Component'
#
# Values:
# - 0 = CRP not checked
# - 1 = CRP checked and < 15 mg/L
# - 2 = CRP checked and >= 15 mg/L
calc_crp_0 <- function(
  daily_crp_8a_0,
  daily_crp_nc_0,
  daily_crp_8a_m1,
  daily_crp_nc_m1,
  daily_crp_8a_m2,
  daily_crp_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 CRP >= 15 (if missing, look back up to 2 days)
    daily_crp_8a_0 >= 15 |
      (is.na(daily_crp_8a_0) & daily_crp_8a_m1 >= 15) |
      (is.na(daily_crp_8a_0) & is.na(daily_crp_8a_m1) & daily_crp_8a_m2 >= 15) ~ 2,

    # Day 0 CRP >= 15 (if missing, look back up to 2 days)
    daily_crp_8a_0 < 15 |
      (is.na(daily_crp_8a_0) & daily_crp_8a_m1 < 15) |
      (is.na(daily_crp_8a_0) & is.na(daily_crp_8a_m1) & daily_crp_8a_m2 < 15) ~ 1,

    # Day 0 CRP not collected (if missing, look back up to 2 days)
    daily_crp_nc_0 == 'Not Collected' |
      daily_crp_nc_m1 == 'Not Collected' |
      daily_crp_nc_m2 == 'Not Collected' ~ 0
  )
}

# Calculate SYSTEMATIC DAG 'Inflammatory Profile: Ferritin Component'
#
# Values:
# - 0 = Ferritin not checked
# - 1 = Ferritin checked and < 700 ng/mL
# - 2 = Ferritin checked and >= 700 ng/mL
calc_ferritin_0 <- function(
  daily_ferritin_8a_0,
  daily_ferritin_nc_0,
  daily_ferritin_8a_m1,
  daily_ferritin_nc_m1,
  daily_ferritin_8a_m2,
  daily_ferritin_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 Ferritin >= 700 (if missing, look back up to 2 days)
    daily_ferritin_8a_0 >= 700 |
      (is.na(daily_ferritin_8a_0) & daily_ferritin_8a_m1 >= 700) |
      (is.na(daily_ferritin_8a_0) & is.na(daily_ferritin_8a_m1) & daily_ferritin_8a_m2 >= 700) ~ 2,

    # Day 0 Ferritin < 700 (if missing, look back up to 2 days)
    daily_ferritin_8a_0 < 700 |
      (is.na(daily_ferritin_8a_0) & daily_ferritin_8a_m1 < 700) |
      (is.na(daily_ferritin_8a_0) & is.na(daily_ferritin_8a_m1) & daily_ferritin_8a_m2 < 700) ~ 1,

    # Day 0 Ferritin not collected (if missing, look back up to 2 days)
    daily_ferritin_nc_0 == 'Not Collected' |
      daily_ferritin_nc_m1 == 'Not Collected' |
      daily_ferritin_nc_m2 == 'Not Collected' ~ 0
  )
}

# Calculate SYSTEMATIC DAG 'Inflammatory Profile: Fibrinogen Component'
#
# Values:
# - 0 = Fibrinogen not checked
# - 1 = Fibrinogen checked and < 150 mg/dL
# - 2 = Fibrinogen checked and >= 150 mg/dL
calc_fibrinogen_0 <- function(
  daily_fibrinogen_8a_0,
  daily_fibrinogen_nc_0,
  daily_fibrinogen_8a_m1,
  daily_fibrinogen_nc_m1,
  daily_fibrinogen_8a_m2,
  daily_fibrinogen_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 Fibrinogen >= 150 (if missing, look back up to 2 days)
    daily_fibrinogen_8a_0 >= 150 |
      (is.na(daily_fibrinogen_8a_0) & daily_fibrinogen_8a_m1 >= 150) |
      (is.na(daily_fibrinogen_8a_0) & is.na(daily_fibrinogen_8a_m1) & daily_fibrinogen_8a_m2 >= 150) ~ 2,

    # Day 0 Fibrinogen < 150 (if missing, look back up to 2 days)
    daily_fibrinogen_8a_0 < 150 |
      (is.na(daily_fibrinogen_8a_0) & daily_fibrinogen_8a_m1 < 150) |
      (is.na(daily_fibrinogen_8a_0) & is.na(daily_fibrinogen_8a_m1) & daily_fibrinogen_8a_m2 < 150) ~ 1,

    # Day 0 Fibrinogen not collected (if missing, look back up to 2 days)
    daily_fibrinogen_nc_0 == 'Not Collected' |
      daily_fibrinogen_nc_m1 == 'Not Collected' |
      daily_fibrinogen_nc_m2 == 'Not Collected' ~ 0
  )
}

# Calculate SYSTEMATIC DAG 'Inflammatory Profile: D-dimer Component'
#
# Values:
# - 0 = D-dimer not checked
# - 1 = D-dimer checked and < 1500 ng/mL
# - 2 = D-dimer checked and >= 1500 ng/mL
calc_ddimer_0 <- function(
  daily_ddimer_8a_0,
  daily_ddimer_nc_0,
  daily_ddimer_8a_m1,
  daily_ddimer_nc_m1,
  daily_ddimer_8a_m2,
  daily_ddimer_nc_m2
  ) {
  dplyr::case_when(
    # Day 0 D-dimer >= 1500 (if missing, look back up to 2 days)
    daily_ddimer_8a_0 >= 1500 |
      (is.na(daily_ddimer_8a_0) & daily_ddimer_8a_m1 >= 1500) |
      (is.na(daily_ddimer_8a_0) & is.na(daily_ddimer_8a_m1) & daily_ddimer_8a_m2 >= 1500) ~ 2,

    # Day 0 D-dimer < 1500 (if missing, look back up to 2 days)
    daily_ddimer_8a_0 < 1500 |
      (is.na(daily_ddimer_8a_0) & daily_ddimer_8a_m1 < 1500) |
      (is.na(daily_ddimer_8a_0) & is.na(daily_ddimer_8a_m1) & daily_ddimer_8a_m2 < 1500) ~ 1,

    # Day 0 D-dimer not collected (if missing, look back up to 2 days)
    daily_ddimer_nc_0 == 'Not Collected' |
      daily_ddimer_nc_m1 == 'Not Collected' |
      daily_ddimer_nc_m2 == 'Not Collected' ~ 0
  )
}


# Convenience wrapper function for SYSTEMATIC DAG
# Returns a data frame with record_id and the four component columns (one row per record_id)
wrapper_calc_sys_inflamprofile_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate all four inflammatory profile components and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(
          crp_0 = calc_crp_0(
            daily_crp_8a_0 = daily_crp_8a_0,
            daily_crp_nc_0 = daily_crp_nc_0,
            daily_crp_8a_m1 = daily_crp_8a_m1,
            daily_crp_nc_m1 = daily_crp_nc_m1,
            daily_crp_8a_m2 = daily_crp_8a_m2,
            daily_crp_nc_m2 = daily_crp_nc_m2
          ),
          ferritin_0 = calc_ferritin_0(
            daily_ferritin_8a_0 = daily_ferritin_8a_0,
            daily_ferritin_nc_0 = daily_ferritin_nc_0,
            daily_ferritin_8a_m1 = daily_ferritin_8a_m1,
            daily_ferritin_nc_m1 = daily_ferritin_nc_m1,
            daily_ferritin_8a_m2 = daily_ferritin_8a_m2,
            daily_ferritin_nc_m2 = daily_ferritin_nc_m2
          ),
          fibrinogen_0 = calc_fibrinogen_0(
            daily_fibrinogen_8a_0 = daily_fibrinogen_8a_0,
            daily_fibrinogen_nc_0 = daily_fibrinogen_nc_0,
            daily_fibrinogen_8a_m1 = daily_fibrinogen_8a_m1,
            daily_fibrinogen_nc_m1 = daily_fibrinogen_nc_m1,
            daily_fibrinogen_8a_m2 = daily_fibrinogen_8a_m2,
            daily_fibrinogen_nc_m2 = daily_fibrinogen_nc_m2
          ),
          ddimer_0 = calc_ddimer_0(
            daily_ddimer_8a_0 = daily_ddimer_8a_0,
            daily_ddimer_nc_0 = daily_ddimer_nc_0,
            daily_ddimer_8a_m1 = daily_ddimer_8a_m1,
            daily_ddimer_nc_m1 = daily_ddimer_nc_m1,
            daily_ddimer_8a_m2 = daily_ddimer_8a_m2,
            daily_ddimer_nc_m2 = daily_ddimer_nc_m2
          )
        ) |>
        select(record_id, crp_0, ferritin_0, fibrinogen_0, ddimer_0),
      by = 'record_id'
    )
}
