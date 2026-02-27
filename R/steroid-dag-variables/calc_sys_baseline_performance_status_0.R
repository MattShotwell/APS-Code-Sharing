## -----------------------------------------------------------------------------
## Baseline Performance Status (Systematic DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Baseline Performance Status: Frailty Status'
#
# Values:
# - 0 = Not performed
# - 1 = Very Fit
# - 2 = Fit
# - 3 = Managing Well
# - 4 = Living with very mild frailty
# - 5 = Living with mild frailty
# - 6 = Living with moderate frailty
# - 7 = Living with severe frailty
# - 8 = Living with very severe frailty
# - 9 = Terminally ill
# - 99 = Unknown
calc_sys_frailty_status_0 <- function(
  frailty_base_code,
  frailty_perf
) {
  dplyr::case_when(
    frailty_perf == "No" ~ 0,
    frailty_perf == "Yes" & !is.na(frailty_base_code) ~ as.numeric(frailty_base_code),
    frailty_perf == "Yes" & is.na(frailty_base_code) ~ 99
  )
}



# Calculate SYSTEMATIC DAG 'Baseline Performance Status: Comorbidity Burden'
#
# Value: Charlson Comorbidity Index
calc_sys_comorbid_burden_0 <- function(
  # Cardiovascular conditions
  m_cv_conditions___2,      # CHF
  m_cv_conditions___4,      # MI
  m_cv_conditions___6,      # Peripheral vascular disease

  # Neurologic conditions
  m_neurologic_conditions___1,  # Dementia
  m_neurologic_conditions___2,  # Stroke
  m_neurologic_conditions___3,  # TIA
  m_neurologic_conditions___4,  # Hemiplegia
  m_neurologic_conditions___5,  # Paraplegia

  # Pulmonary conditions
  m_pulm_conditions___1,    # Asthma
  m_pulm_conditions___2,    # Interstitial lung disease
  m_pulm_conditions___3,    # COPD
  m_pulm_conditions___6,    # Obstructive sleep apnea

  # Kidney and liver conditions
  m_kid_liver_conditions___1,  # Chronic kidney disease
  m_kid_liver_conditions___2,  # Cirrhosis
  mhckdsev,                 # CKD severity
  mhhepsev,                 # Liver disease severity
  mhhepfail,                # Hepatic failure

  # Endocrine conditions
  m_endo_conditions___1,    # Diabetes
  mhdmsev,                  # Diabetes severity

  # Cancer conditions
  m_cancer_conditions___1,  # Leukemia
  m_cancer_conditions___2,  # Lymphoma
  m_cancer_conditions___4,  # Solid organ cancer
  mhsolidmeta,              # Solid tumor metastasis

  # Immunosuppression conditions
  m_immunosup_conditions___5,  # AIDS

  # Other conditions
  mhrheumd,                 # Rheumatologic/connective tissue disease
  m_eso_stom_int_conditions___1  # Peptic ulcer disease
) {

  # Initialize score at 0
  score <- 0

  # Charlson Comorbidity Index scoring (standard weights)

  # 1 POINT CONDITIONS

  # Myocardial infarction: +1
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___4), 1, 0)

  # Congestive heart failure: +1
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___2), 1, 0)

  # Peripheral vascular disease: +1
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___6), 1, 0)

  # Cerebrovascular disease (stroke or TIA): +1
  score <- score + dplyr::if_else(
    is_checked(m_neurologic_conditions___2) | is_checked(m_neurologic_conditions___3),
    1,
    0
  )

  # Dementia: +1
  score <- score + dplyr::if_else(is_checked(m_neurologic_conditions___1), 1, 0)

  # Chronic pulmonary disease: +1
  score <- score + dplyr::if_else(
    is_checked(m_pulm_conditions___1) |
    is_checked(m_pulm_conditions___2) |
    is_checked(m_pulm_conditions___3) |
    is_checked(m_pulm_conditions___6),
    1,
    0
  )

  # Connective tissue disease: +1
  score <- score + dplyr::if_else(mhrheumd == "Yes", 1, 0)

  # Ulcer disease: +1
  score <- score + dplyr::if_else(is_checked(m_eso_stom_int_conditions___1), 1, 0)

  # Mild liver disease: +1
  # (has liver disease AND (mild severity OR unknown severity) AND (no hepatic failure OR unknown failure))
  # Note: Only count if NOT moderate/severe (checked later)
  has_mild_liver <- is_checked(m_kid_liver_conditions___2) &
                    (mhhepsev == "Mild (chronic hepatitis w/o portal HTN)" |
                      mhhepsev == "Unknown cirrhosis severity") &
                    (mhhepfail == "No" | mhhepfail == "Unknown")
  mild_liver_points <- dplyr::if_else(has_mild_liver, 1, 0)

  # Diabetes uncomplicated: +1
  # (has diabetes AND (mild/uncomplicated severity OR unknown severity))
  # Note: Only count if NOT complicated (checked later)
  has_diabetes_uncomplicated <- is_checked(m_endo_conditions___1) &
                                (mhdmsev == "Uncomplicated: treated with insulin or oral diabetic medication, no end organ damage" |
                                mhdmsev == "Unknown severity")
  diabetes_uncomplicated_points <- dplyr::if_else(has_diabetes_uncomplicated, 1, 0)

  # 2 POINT CONDITIONS

  # Hemiplegia or paraplegia: +2
  score <- score + dplyr::if_else(
    is_checked(m_neurologic_conditions___4) | is_checked(m_neurologic_conditions___5),
    2,
    0
  )

  # Moderate or severe renal disease: +2
  # (has CKD AND severity is moderate (2) OR severe (3))
  has_mod_severe_renal <- is_checked(m_kid_liver_conditions___1) &
                          (mhckdsev == "Baseline (before acute illness) creatinine >3.0 mg/dl" |
                          mhckdsev == "Chronic kidney replacement therapy (such as hemodialysis or peritoneal dialysis)")
  score <- score + dplyr::if_else(has_mod_severe_renal, 2, 0)

  # Diabetes with end organ damage: +2
  # (has diabetes AND complicated/severe)
  has_diabetes_complicated <- is_checked(m_endo_conditions___1) &
                              (mhdmsev == "Complicated: treated with insulin or oral diabetic medication, end-organ damage present")
  diabetes_complicated_points <- dplyr::if_else(has_diabetes_complicated, 2, 0)

  # Add diabetes points (only one or the other, not both, preference to complicated if both checked)
  score <- score + dplyr::if_else(
    has_diabetes_complicated,
    diabetes_complicated_points,
    diabetes_uncomplicated_points
  )

  # Any tumor without metastasis: +2
  # (has solid tumor AND (no mets OR unknown mets))
  # Note: Only count if NOT metastatic (checked later)
  has_tumor_no_mets <- is_checked(m_cancer_conditions___4) &
                       (mhsolidmeta == "No" | mhsolidmeta == "Unknown")
  tumor_no_mets_points <- dplyr::if_else(has_tumor_no_mets, 2, 0)

  # Leukemia: +2
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___1), 2, 0)

  # Lymphoma: +2
  score <- score + dplyr::if_else(is_checked(m_cancer_conditions___2), 2, 0)

  # 3 POINT CONDITIONS

  # Moderate or severe liver disease: +3
  # (has liver disease AND (moderate severity (2) OR severe severity (3) OR hepatic failure))
  # Note: Only count if NOT mild (replaces mild liver score)
  has_mod_severe_liver <- is_checked(m_kid_liver_conditions___2) &
                          (mhhepsev == "Mod (cirrhosis with portal HTN)" |
                          mhhepsev == "Severe (cirrhosis with portal HTN & variceal bleed)" |
                          mhhepfail == "Yes")
  mod_severe_liver_points <- dplyr::if_else(has_mod_severe_liver, 3, 0)

  # Add liver points (only one or the other, not both)
  score <- score + dplyr::if_else(
    has_mod_severe_liver,
    mod_severe_liver_points,
    mild_liver_points
  )

  # 6 POINT CONDITIONS

  # Metastatic solid tumor: +6
  # (has solid tumor AND has metastasis)
  # Note: Only count if metastatic (replaces non-metastatic score)
  has_tumor_with_mets <- is_checked(m_cancer_conditions___4) & mhsolidmeta == "Yes"
  tumor_with_mets_points <- dplyr::if_else(has_tumor_with_mets, 6, 0)

  # Add solid tumor points (only one or the other, not both)
  score <- score + dplyr::if_else(
    has_tumor_with_mets,
    tumor_with_mets_points,
    tumor_no_mets_points
  )

  # AIDS: +6
  score <- score + dplyr::if_else(is_checked(m_immunosup_conditions___5), 6, 0)

  return(score)
}


# Convenience wrapper function
# Returns a data frame with record_id, sys_frailty_status_0, and sys_comorbid_burden_0 columns (one row per record_id)
wrapper_calc_sys_baseline_performance_status_0 <- function(data, dictionary) {
  # Get frailty data from Patient/Surrogate Interview
  data_with_frailty <- data |>
    filter(event_label == "Patient/Surrogate Interview") |>
    left_join(
      get_code_label_map('frailty_base', dictionary),
      by = 'frailty_base'
    ) |>
    mutate(
      sys_frailty_status_0 = calc_sys_frailty_status_0(
        frailty_base_code = frailty_base_code,
        frailty_perf = frailty_perf
      )
    ) |>
    select(record_id, sys_frailty_status_0)

  # Get comorbidity data from Day 0 and Patient/Surrogate Interview
  data_with_comorbid <- data |>
    filter(event_label == 'Day 0') |>
    mutate(
      sys_comorbid_burden_0 = calc_sys_comorbid_burden_0(
        # Cardiovascular conditions
        m_cv_conditions___2,
        m_cv_conditions___4,
        m_cv_conditions___6,
        # Neurologic conditions
        m_neurologic_conditions___1,
        m_neurologic_conditions___2,
        m_neurologic_conditions___3,
        m_neurologic_conditions___4,
        m_neurologic_conditions___5,
        # Pulmonary conditions
        m_pulm_conditions___1,
        m_pulm_conditions___2,
        m_pulm_conditions___3,
        m_pulm_conditions___6,
        # Kidney and liver conditions
        m_kid_liver_conditions___1,
        m_kid_liver_conditions___2,
        mhckdsev,
        mhhepsev,
        mhhepfail,
        # Endocrine conditions
        m_endo_conditions___1,
        mhdmsev,
        # Cancer conditions
        m_cancer_conditions___1,
        m_cancer_conditions___2,
        m_cancer_conditions___4,
        mhsolidmeta,
        # Immunosuppression conditions
        m_immunosup_conditions___5,
        # Other conditions
        mhrheumd,
        m_eso_stom_int_conditions___1
      )
    ) |>
    select(record_id, sys_comorbid_burden_0)

  data |>
    distinct(record_id) |>
    left_join(data_with_frailty, by = 'record_id') |>
    left_join(data_with_comorbid, by = 'record_id')
}
