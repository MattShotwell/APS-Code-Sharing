## -----------------------------------------------------------------------------
## Gastrointestinal Bleeding (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'Gastrointestinal Bleeding'
#
# Value: Total packed red blood cell (PRBC) units transfused
calc_str_gi_bleeding_0 <- function(
  trx_0,
  trx_m1,
  trx_m2,
  blood_prbc_units_m2,
  blood_prbc_units_m1,
  blood_prbc_units_0,
  daily_blood_product_m2,
  daily_blood_product_m1,
  daily_blood_product_0,
  blood_product_spec_m2___1,
  blood_product_spec_m1___1,
  blood_product_spec_0___1
) {

  # Day -2:
  prbc_m2 <- dplyr::case_when(
    (daily_blood_product_m2 == "Administered" &
      is_checked(blood_product_spec_m2___1)) ~ blood_prbc_units_m2,

    (daily_blood_product_m2 == "UNK" &
      is_unchecked(blood_product_spec_m2___1)) ~ 1,

    (daily_blood_product_m2 %in% c("Administered", "Not administered") &
      is_unchecked(blood_product_spec_m2___1)) ~ 0,

    (trx_m2 == 'Not Available') ~ 0
  )

  # Day -1:
  prbc_m1 <- dplyr::case_when(
    (daily_blood_product_m1 == "Administered" &
      is_checked(blood_product_spec_m1___1)) ~ blood_prbc_units_m1,

    (daily_blood_product_m1 == "UNK" &
      is_unchecked(blood_product_spec_m1___1)) ~ 1,

    (daily_blood_product_m1 %in% c("Administered", "Not administered") &
      is_unchecked(blood_product_spec_m1___1)) ~ 0,

    (trx_m1 == 'Not Available') ~ 0
  )

  # Day 0:
  prbc_0 <- dplyr::case_when(
    (daily_blood_product_0 == "Administered" &
      is_checked(blood_product_spec_0___1)) ~ blood_prbc_units_0,

    (daily_blood_product_0 == "UNK" &
      is_unchecked(blood_product_spec_0___1)) ~ 1,

    (daily_blood_product_0 %in% c("Administered", "Not administered") &
      is_unchecked(blood_product_spec_0___1)) ~ 0,

    (trx_0 == 'Not Available') ~ 0 # If missing for valid reasons, assume 0 units
  )

  # Sum PRBC units across all three days
  # - If any day has invalid configuration, return NA
  # - Otherwise sum all days (values and 0s)
  total_prbc <- rowSums(
    cbind(prbc_m2, prbc_m1, prbc_0),
    na.rm = FALSE
  )

  return(total_prbc)
}


# Convenience wrapper function
# Returns a data frame with record_id and str_gi_bleeding_0 columns (one row per record_id)
wrapper_calc_str_gi_bleeding_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_gi_bleeding_0 and join back to record_id
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        mutate(str_gi_bleeding_0 = calc_str_gi_bleeding_0(
          trx_0 = trx_0,
          trx_m1 = trx_m1,
          trx_m2 = trx_m2,
          blood_prbc_units_m2 = blood_prbc_units_m2,
          blood_prbc_units_m1 = blood_prbc_units_m1,
          blood_prbc_units_0 = blood_prbc_units_0,
          daily_blood_product_m2 = daily_blood_product_m2,
          daily_blood_product_m1 = daily_blood_product_m1,
          daily_blood_product_0 = daily_blood_product_0,
          blood_product_spec_m2___1 = blood_product_spec_m2___1,
          blood_product_spec_m1___1 = blood_product_spec_m1___1,
          blood_product_spec_0___1 = blood_product_spec_0___1
        )) |>
        select(record_id, str_gi_bleeding_0),
      by = 'record_id'
    )
}
