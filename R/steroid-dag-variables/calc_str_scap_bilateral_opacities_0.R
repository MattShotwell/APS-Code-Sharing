## -----------------------------------------------------------------------------
## SCAP Criterion: Bilateral Opacities (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'SCAP Criterion: Bilateral Opacities'
#
# Values:
# - 0 = No
# - 1 = Yes
calc_str_scap_bilateral_opacities_0 <- function(
  cxr_dm2_available,
  cxr_dm2_opacity_code,
  ct_dm2_available,
  ct_dm2_opacity_code,

  cxr_dm1_available,
  cxr_dm1_opacity_code,
  ct_dm1_available,
  ct_dm1_opacity_code,

  cxr_d0_available,
  cxr_d0_opacity_code,
  ct_d0_available,
  ct_d0_opacity_code
) {

  ## Check if any opacity value is "2" (bilateral)
  has_opacity <- (
    cxr_dm2_opacity_code == "2" |
    ct_dm2_opacity_code == "2" |

    cxr_dm1_opacity_code == "2" |
    ct_dm1_opacity_code == "2" |

    cxr_d0_opacity_code == "2" |
    ct_d0_opacity_code == "2"
  )

  # Two methods were used in assembling these variables:
  # Method 1 - cxr/ct_available indicates if imaging was done, with the result stored in cxr/ct_opacity (if present)
  # Method 2 - cxr_opacity set to "00" or "0" indicates nothing was found for BOTH cxr and ct
  # Thus we check both options for each day and if any day has invalid data, it should be queried
  has_no_opacity <- (

    # Day -2
    (
      (cxr_dm2_opacity_code %in% c("00", "0", "1", "99")) |
      (cxr_dm2_available %in% c("No", "Yes") & ct_dm2_available %in% c("No", "Yes"))
    ) &

    # Day -1
    (
      (cxr_dm1_opacity_code %in% c("00", "0", "1", "99")) |
      (cxr_dm1_available %in% c("No", "Yes") & ct_dm1_available %in% c("No", "Yes"))
    ) &

    # Day 0
    (
      (cxr_d0_opacity_code %in% c("00", "0", "1", "99")) |
      (cxr_d0_available %in% c("No", "Yes") & ct_d0_available %in% c("No", "Yes"))
    )
  )

  dplyr::case_when(
    has_opacity ~ 1,
    has_no_opacity ~ 0
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and str_scap_bilateral_opacities_0 columns (one row per record_id)
wrapper_calc_str_scap_bilateral_opacities_0 <- function(data, dictionary) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_scap_bilateral_opacities_0 and join back to record_id
      data |>
        filter(event_label == 'Syndrome Adjudication') |>
        left_join(
          get_code_label_map('cxr_dm2_opacity', dictionary),
          by = 'cxr_dm2_opacity'
        ) |>
        left_join(
          get_code_label_map('ct_dm2_opacity', dictionary),
          by = 'ct_dm2_opacity'
        ) |>
        left_join(
          get_code_label_map('cxr_dm1_opacity', dictionary),
          by = 'cxr_dm1_opacity'
        ) |>
        left_join(
          get_code_label_map('ct_dm1_opacity', dictionary),
          by = 'ct_dm1_opacity'
        ) |>
        left_join(
          get_code_label_map('cxr_d0_opacity', dictionary),
          by = 'cxr_d0_opacity'
        ) |>
        left_join(
          get_code_label_map('ct_d0_opacity', dictionary),
          by = 'ct_d0_opacity'
        ) |>
        mutate(str_scap_bilateral_opacities_0 = calc_str_scap_bilateral_opacities_0(
          cxr_dm2_available = cxr_dm2_available,
          cxr_dm2_opacity_code = cxr_dm2_opacity_code,
          ct_dm2_available = ct_dm2_available,
          ct_dm2_opacity_code = ct_dm2_opacity_code,
          cxr_dm1_available = cxr_dm1_available,
          cxr_dm1_opacity_code = cxr_dm1_opacity_code,
          ct_dm1_available = ct_dm1_available,
          ct_dm1_opacity_code = ct_dm1_opacity_code,
          cxr_d0_available = cxr_d0_available,
          cxr_d0_opacity_code = cxr_d0_opacity_code,
          ct_d0_available = ct_d0_available,
          ct_d0_opacity_code = ct_d0_opacity_code
        )) |>
        select(record_id, str_scap_bilateral_opacities_0),
      by = 'record_id'
    )
}
