# ==============================================================================
# Steroid DAG Variables
# ==============================================================================
#
# This file is the starting point for deriving all steroid DAG variables for
# APS Aim 6. Use `source("steroid-dag-variables.R")` to get access to all
# variable functions, or source individual files for specific variables as needed.
# In addition to source statements for each steroid DAG file, this file contains
# two helper functions, one for calculating all DAG variables
# (`calc_all_steroid_dag_variables()`) and one for identifying records that
# couldn't be classified (left as NA) under one or more DAG variables
# (`get_records_with_missing_values()`).
#
# A given variable may have both a "systematic" and "streamlined" version, though
# many have only one or the other. The prefix "sys_" is used for SYSTEMATIC
# variables and "str_" is used for STREAMLINED variables. The filename generally
# matches the underlying function name, however, when a variable has multiple
# component variables, the file is named more generically after the variable
# (e.g., calc_sys_hyperglycemia_0.R).
#
# Each file contains a main function that contains the logic for calculating the
# variable from the appropriate inputs and a convenience wrapper function that
# takes the full dataset (and sometimes the data dictionary) as input, extracts
# the necessary variables, calculates the variable (using the main function), and
# returns a data frame with record_id and variable columns (one row per record_id).
#
# For example, `calc_sys_nmblockade_0.R` contains `calc_sys_nmblockade_0()` and
# `wrapper_calc_sys_nmblockade_0()`.
#
# We HIGHLY recommend using the wrapper function instead of the main function,
# because the process for extracting the necessary variables is tedious and error-prone,
# often requiring joining multiple event labels together. The wrapper function abstracts
# away that complexity.
#
# ==============================================================================

# Functions to calculate SYSTEMATIC DAG Variables
source("R/steroid-dag-variables/calc_sys_global_phys_sev_0.R")
source("R/steroid-dag-variables/calc_sys_hypotension_sev_0.R")
source("R/steroid-dag-variables/calc_sys_nmblockade_0.R")
source("R/steroid-dag-variables/calc_sys_inflamprofile_0.R")
source("R/steroid-dag-variables/calc_sys_resp_failure_sev_0.R")
source("R/steroid-dag-variables/calc_sys_def_adrenal_insufficiency_0.R")
source("R/steroid-dag-variables/calc_sys_chron_steroid_moddose_0.R")
source("R/steroid-dag-variables/calc_sys_steroid_comorb_0.R")
source("R/steroid-dag-variables/calc_sys_obstruct_lung_0.R")
source("R/steroid-dag-variables/calc_sys_active_covid19_0.R")
source("R/steroid-dag-variables/calc_sys_chron_immunocomp_0.R")
source("R/steroid-dag-variables/calc_sys_hyperglycemia_0.R")
source("R/steroid-dag-variables/calc_sys_active_influenza_0.R")
source("R/steroid-dag-variables/calc_sys_baseline_performance_status_0.R")
source("R/steroid-dag-variables/calc_sys_organ_failure_trajectory.R")
source("R/steroid-dag-variables/calc_sys_delirium_0.R")
source("R/steroid-dag-variables/calc_sys_active_fungal_0.R")
source("R/steroid-dag-variables/calc_sys_sepsis_0.R")
source("R/steroid-dag-variables/calc_sys_ards_0.R")
source("R/steroid-dag-variables/calc_sys_pneumonia_0.R")
source("R/steroid-dag-variables/calc_sys_septic_shock_0.R")

# Functions to calculate STREAMLINED DAG Variables
source("R/steroid-dag-variables/calc_str_inflamprofile_0.R")
source("R/steroid-dag-variables/calc_str_def_adrenal_insufficiency_0.R")
source("R/steroid-dag-variables/calc_str_chron_steroid_moddose_0.R")
source("R/steroid-dag-variables/calc_str_chron_steroid_highdose_0.R")
source("R/steroid-dag-variables/calc_str_steroid_comorb_0.R")
source("R/steroid-dag-variables/calc_str_hyperglyc_hist_0.R")
source("R/steroid-dag-variables/calc_str_septic_shock_0.R")
source("R/steroid-dag-variables/calc_str_gi_bleeding_0.R")
source("R/steroid-dag-variables/calc_str_major_surgery_0.R")
source("R/steroid-dag-variables/calc_str_age_0.R")
source("R/steroid-dag-variables/calc_str_dementia_0.R")
source("R/steroid-dag-variables/calc_str_acute_allergic_reaction_0.R")
source("R/steroid-dag-variables/calc_str_dah_0.R")
source("R/steroid-dag-variables/calc_str_drug_toxicity_0.R")
source("R/steroid-dag-variables/calc_str_cop_0.R")
source("R/steroid-dag-variables/calc_str_scap_rr_0.R")
source("R/steroid-dag-variables/calc_str_scap_bun_0.R")
source("R/steroid-dag-variables/calc_str_scap_acidosis_0.R")
source("R/steroid-dag-variables/calc_str_scap_bilateral_opacities_0.R")
source("R/steroid-dag-variables/calc_str_scap_leukopenia_0.R")
source("R/steroid-dag-variables/calc_str_scap_thrombocytopenia_0.R")
source("R/steroid-dag-variables/calc_str_scap_hypothermia_0.R")
source("R/steroid-dag-variables/calc_str_neuromuscular_disease_0.R")
source("R/steroid-dag-variables/calc_str_psi_score_0.R")
source("R/steroid-dag-variables/calc_str_vasculitis_0.R")


# Convenience function to calculate all steroid DAG variables
# - Input:
#   - dataset containing all raw variables needed to calculate the DAG variables
#   - data dictionary
# - Returns: Single data frame with columns for each DAG variable and one row per record_id
calc_all_steroid_dag_variables <- function(data, dictionary) {
    data |>
        # Get unique record IDs
        distinct(record_id) |>
        # DAG Variables
        left_join(wrapper_calc_sys_nmblockade_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_chron_steroid_moddose_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_chron_steroid_moddose_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_def_adrenal_insufficiency_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_def_adrenal_insufficiency_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_hyperglycemia_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_hyperglyc_hist_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_resp_failure_sev_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_sys_steroid_comorb_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_steroid_comorb_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_acute_allergic_reaction_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_age_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_chron_steroid_highdose_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_cop_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_dah_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_dementia_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_drug_toxicity_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_gi_bleeding_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_inflamprofile_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_inflamprofile_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_major_surgery_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_neuromuscular_disease_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_psi_score_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_scap_acidosis_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_scap_bilateral_opacities_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_scap_bun_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_scap_hypothermia_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_scap_leukopenia_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_scap_rr_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_scap_thrombocytopenia_0(data), by = "record_id") |>
        left_join(wrapper_calc_str_septic_shock_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_str_vasculitis_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_active_covid19_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_sys_active_fungal_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_active_influenza_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_sys_ards_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_baseline_performance_status_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_sys_chron_immunocomp_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_delirium_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_global_phys_sev_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_sys_hypotension_sev_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_obstruct_lung_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_organ_failure_trajectory(data), by = "record_id") |>
        left_join(wrapper_calc_sys_pneumonia_0(data), by = "record_id") |>
        left_join(wrapper_calc_sys_sepsis_0(data, dictionary), by = "record_id") |>
        left_join(wrapper_calc_sys_septic_shock_0(data, dictionary), by = "record_id")
}

# Helper function to identify records with missing values in any of the DAG variables
# - Input: data frame containing all DAG variables for each record_id (output of calc_all_steroid_dag_variables)
# - Returns: Data frame with one row for each record_id that has at least one missing value, along with the list of which variables are missing
get_records_with_missing_values <- function(data) {
  # For each row, find columns with NA values
  missing_list <- lapply(1:nrow(data), function(i) {
    row <- data[i, ]
    missing_cols <- names(row)[sapply(row, is.na)]

    if (length(missing_cols) > 0) {
      tibble(
        record_id = data$record_id[i],
        missing_cols = list(missing_cols)
      )
    } else {
      NULL
    }
  })

  # Combine all non-NULL results
  bind_rows(missing_list) |>
    arrange(record_id)
}
