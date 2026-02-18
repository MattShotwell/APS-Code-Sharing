## -----------------------------------------------------------------------------
## PSI Score (Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate STREAMLINED DAG 'PSI Score'
#
# Value: Pneumonia Severity Index (PSI) score
calc_str_psi_score_0 <- function(
  age,
  sex,
  prssrc,

  m_cv_conditions___2,
  m_neurologic_conditions___2,
  m_neurologic_conditions___3,
  m_cancer,
  m_kid_liver_conditions___1,
  m_kid_liver_conditions___2,

  cam_0,
  cam_m1,
  cam_m2,

  daily_gcs_8a_0,
  daily_gcs_8a_m1,
  daily_gcs_8a_m2,

  sofa_base_cns_dysnfx,
  sofa_base_gcs_chronic,

  highhr_vsorres,
  highrr_vsorres,
  lowsysbp_vsorres,
  lowtemp_vsorres,
  hightemp_vsorres,

  daily_bun_8a_0,
  daily_bun_8a_m1,
  daily_bun_8a_m2,

  daily_gluc_8a_0,
  daily_gluc_8a_m1,
  daily_gluc_8a_m2,

  daily_hct_8a_0,
  daily_hct_8a_m1,
  daily_hct_8a_m2,

  daily_na_8a_0,
  daily_na_8a_m1,
  daily_na_8a_m2,

  daily_pa02_lowest_0,
  daily_pa02_lowest_m1,
  daily_pa02_lowest_m2,

  daily_ph_lowest_0,
  daily_ph_lowest_m1,
  daily_ph_lowest_m2,

  pna_effusion
) {

  # Demographics

  # - Age
  score <- age

  # - Sex: Female -10 points
  score <- score + dplyr::if_else(sex == "Female", -10, 0)


  # Nursing home resident: +10 points
  score <- score + dplyr::if_else(
    prssrc %in% c(
      'Rehabilitation facility',
      'Long term acute care hospital (LTACH)',
      'Nursing home or Skilled Nursing Facility (SNF)'
      ), 10,
    0,
    missing = 0
  )

  # Coexisting Illnesses
  # - Neoplastic disease: +30 points
  score <- score + dplyr::if_else(m_cancer == "Yes", 30, 0, missing = 0)

  # - Liver disease: +20 points (Cirrhosis)
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___2), 20, 0, missing = 0)

  # - Congestive heart failure: +10 points
  score <- score + dplyr::if_else(is_checked(m_cv_conditions___2), 10, 0, missing = 0)

  # - Cerebrovascular disease: +10 points (Prior stroke or TIA)
  score <- score + dplyr::if_else(
    is_checked(m_neurologic_conditions___2) | is_checked(m_neurologic_conditions___3), 10,
    0,
    missing = 0
  )

  # - Renal disease: +10 points (Chronic kidney disease)
  score <- score + dplyr::if_else(is_checked(m_kid_liver_conditions___1), 10, 0, missing = 0)

  # Physical examination findings

  # - Altered mental status: +20 points

  # Get GCS with lookback (excluding T values and "not documented")
  gcs <- get_gcs_with_lookback(daily_gcs_8a_0, daily_gcs_8a_m1, daily_gcs_8a_m2)
  gcs_numeric <- as.numeric(gcs)

  # Get CAM value with lookback
  # QUESTION: Do we use lookback for CAM here?
  cam <- get_value_with_lookback(cam_0, cam_m1, cam_m2)
  cam_positive <- cam == "Positive for delirium at least once on this day"

  # Calculate altered mental status based on the two conditions
  altered_mental_status <- dplyr::case_when(
    # Condition 1: No baseline CNS dysfunction (or missing) AND (CAM positive OR GCS < 15)
    (is.na(sofa_base_cns_dysnfx) | sofa_base_cns_dysnfx == "No") &
      (cam_positive | gcs_numeric < 15) ~ TRUE,

    # Condition 2: Has baseline CNS dysfunction AND GCS meets chronic threshold
    sofa_base_cns_dysnfx == "Yes" & (
      (sofa_base_gcs_chronic == "13-14" & gcs_numeric <= 12) |
      (sofa_base_gcs_chronic == "10-12" & gcs_numeric <= 9) |
      (sofa_base_gcs_chronic == "6-9" & gcs_numeric <= 5)
    ) ~ TRUE,

    # Default: no altered mental status
    .default = FALSE
  )

  score <- score + dplyr::if_else(altered_mental_status, 20, 0, missing = 0)

  # - Respiratory rate >= 30/min: +20 points
  score <- score + dplyr::if_else(highrr_vsorres >= 30, 20, 0, missing = 0)

  # - SBP < 90 mm Hg: +20 points
  score <- score + dplyr::if_else(lowsysbp_vsorres < 90, 20, 0, missing = 0)

  # - Temperature < 35°C or >= 40°C: +15 points
  temp_abnormal <- (lowtemp_vsorres < 35) | (lowtemp_vsorres >= 40) |
                   (hightemp_vsorres < 35) | (hightemp_vsorres >= 40)
  score <- score + dplyr::if_else(temp_abnormal, 15, 0, missing = 0)

  # - Pulse >= 125/min: +10 points
  score <- score + dplyr::if_else(highhr_vsorres >= 125, 10, 0, missing = 0)


  # Laboratory and radiographic findings

  # - Arterial pH < 7.35: +30 points
  ph_lowest <- get_value_with_lookback(daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2)
  score <- score + dplyr::if_else(ph_lowest < 7.35, 30, 0, missing = 0)

  # - BUN >= 30 mg/dl: +20 points
  bun <- get_value_with_lookback(daily_bun_8a_0, daily_bun_8a_m1, daily_bun_8a_m2)
  score <- score + dplyr::if_else(bun >= 30, 20, 0, missing = 0)

  # - Sodium < 130 mmol/L: +20 points
  na <- get_value_with_lookback(daily_na_8a_0, daily_na_8a_m1, daily_na_8a_m2)
  score <- score + dplyr::if_else(na < 130, 20, 0, missing = 0)

  # - Glucose >= 250 mg/dl: +10 points
  gluc <- get_value_with_lookback(daily_gluc_8a_0, daily_gluc_8a_m1, daily_gluc_8a_m2)
  score <- score + dplyr::if_else(gluc >= 250, 10, 0, missing = 0)

  # - Hematocrit < 30%: +10 points
  hct <- get_value_with_lookback(daily_hct_8a_0, daily_hct_8a_m1, daily_hct_8a_m2)
  score <- score + dplyr::if_else(hct < 30, 10, 0, missing = 0)

  # - PaO2 < 60 mm Hg: +10 points
  pa02_lowest <- get_value_with_lookback(daily_pa02_lowest_0, daily_pa02_lowest_m1, daily_pa02_lowest_m2)
  score <- score + dplyr::if_else(pa02_lowest < 60, 10, 0, missing = 0)

  # - Pleural effusion: +10 points (pna_effusion = Yes)
  score <- score + dplyr::if_else(pna_effusion == "Yes", 10, 0, missing = 0)

  return(score)
}


# Convenience wrapper function
# Returns a data frame with record_id and str_psi_score_0 columns (one row per record_id)
wrapper_calc_str_psi_score_0 <- function(data) {
  data |>
    # Ensure one row per record_id (even if data is missing)
    distinct(record_id) |>

    left_join(
      # Calculate str_psi_score_0 and join back to record_id
      # This requires joining from multiple event labels
      data |>
        filter(event_label == 'Daily In-Hospital Forms') |>
        select(record_id, cam_0, cam_m1, cam_m2,
               daily_gcs_8a_0, daily_gcs_8a_m1, daily_gcs_8a_m2,
               daily_bun_8a_0, daily_bun_8a_m1, daily_bun_8a_m2,
               daily_gluc_8a_0, daily_gluc_8a_m1, daily_gluc_8a_m2,
               daily_hct_8a_0, daily_hct_8a_m1, daily_hct_8a_m2,
               daily_na_8a_0, daily_na_8a_m1, daily_na_8a_m2,
               daily_pa02_lowest_0, daily_pa02_lowest_m1, daily_pa02_lowest_m2,
               daily_ph_lowest_0, daily_ph_lowest_m1, daily_ph_lowest_m2) |>
        # Join Day 0 variables (demographics, coexisting illnesses, physical exam)
        left_join(
          data |>
            filter(event_label == 'Day 0') |>
            select(record_id, age, sex, prssrc,
                   m_cv_conditions___2, m_neurologic_conditions___2, m_neurologic_conditions___3,
                   m_cancer, m_kid_liver_conditions___1, m_kid_liver_conditions___2,
                   sofa_base_cns_dysnfx, sofa_base_gcs_chronic,
                   highhr_vsorres, highrr_vsorres, lowsysbp_vsorres, lowtemp_vsorres, hightemp_vsorres),
          by = 'record_id'
        ) |>
        # Join Syndrome Adjudication variables (pleural effusion)
        left_join(
          data |>
            filter(event_label == 'Syndrome Adjudication') |>
            select(record_id, pna_effusion),
          by = 'record_id'
        ) |>
        mutate(str_psi_score_0 = calc_str_psi_score_0(
          # Demographics (Day 0)
          age = age,
          sex = sex,
          prssrc = prssrc,
          # Coexisting Illnesses (Day 0)
          m_cv_conditions___2 = m_cv_conditions___2,
          m_neurologic_conditions___2 = m_neurologic_conditions___2,
          m_neurologic_conditions___3 = m_neurologic_conditions___3,
          m_cancer = m_cancer,
          m_kid_liver_conditions___1 = m_kid_liver_conditions___1,
          m_kid_liver_conditions___2 = m_kid_liver_conditions___2,
          # Altered Mental Status (Daily In-Hospital Forms)
          cam_0 = cam_0,
          cam_m1 = cam_m1,
          cam_m2 = cam_m2,
          daily_gcs_8a_0 = daily_gcs_8a_0,
          daily_gcs_8a_m1 = daily_gcs_8a_m1,
          daily_gcs_8a_m2 = daily_gcs_8a_m2,
          # Altered Mental Status - (Day 0)
          sofa_base_cns_dysnfx = sofa_base_cns_dysnfx,
          sofa_base_gcs_chronic = sofa_base_gcs_chronic,
          # Physical Examination Findings (Day 0)
          highhr_vsorres = highhr_vsorres,
          highrr_vsorres = highrr_vsorres,
          lowsysbp_vsorres = lowsysbp_vsorres,
          lowtemp_vsorres = lowtemp_vsorres,
          hightemp_vsorres = hightemp_vsorres,
          # Laboratory Findings - BUN (Daily In-Hospital Forms)
          daily_bun_8a_0 = daily_bun_8a_0,
          daily_bun_8a_m1 = daily_bun_8a_m1,
          daily_bun_8a_m2 = daily_bun_8a_m2,
          # Laboratory Findings - Glucose (Daily In-Hospital Forms)
          daily_gluc_8a_0 = daily_gluc_8a_0,
          daily_gluc_8a_m1 = daily_gluc_8a_m1,
          daily_gluc_8a_m2 = daily_gluc_8a_m2,
          # Laboratory Findings - Hematocrit (Daily In-Hospital Forms)
          daily_hct_8a_0 = daily_hct_8a_0,
          daily_hct_8a_m1 = daily_hct_8a_m1,
          daily_hct_8a_m2 = daily_hct_8a_m2,
          # Laboratory Findings - Sodium (Daily In-Hospital Forms)
          daily_na_8a_0 = daily_na_8a_0,
          daily_na_8a_m1 = daily_na_8a_m1,
          daily_na_8a_m2 = daily_na_8a_m2,
          # Laboratory Findings - PaO2 (Daily In-Hospital Forms)
          daily_pa02_lowest_0 = daily_pa02_lowest_0,
          daily_pa02_lowest_m1 = daily_pa02_lowest_m1,
          daily_pa02_lowest_m2 = daily_pa02_lowest_m2,
          # Laboratory Findings - pH (Daily In-Hospital Forms)
          daily_ph_lowest_0 = daily_ph_lowest_0,
          daily_ph_lowest_m1 = daily_ph_lowest_m1,
          daily_ph_lowest_m2 = daily_ph_lowest_m2,
          # Radiographic Findings - Pleural Effusion (Syndrome Adjudication)
          pna_effusion = pna_effusion
        )) |>
        select(record_id, str_psi_score_0),
      by = 'record_id'
    )
}
