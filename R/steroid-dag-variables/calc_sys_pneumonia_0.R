## -----------------------------------------------------------------------------
## Presence of Pneumonia Syndrome (Systematic & Streamlined DAG)
## -----------------------------------------------------------------------------

# Calculate SYSTEMATIC DAG 'Presence of Pneumonia Syndrome'
#
# Values:
# - 0 = PNU1 syndrome criteria + antimicrobial criterion not all present
# - 1 = PNU1 syndrome criteria + antimicrobial criterion all present
# - NA = Unknown/insufficient data
calc_sys_pneumonia_0 <- function(
  # PNU1 Criterion #1: Opacity (calculated fields)
  d0_pos,
  dm1_pos,
  dm2_pos,
  # Or check individual opacity codes
  cxr_dm2_opacity_code,
  ct_dm2_opacity_code,
  cxr_dm1_opacity_code,
  ct_dm1_opacity_code,
  cxr_d0_opacity_code,
  ct_d0_opacity_code,
  
  # PNU1 Criterion #2: Fever, WBC, or AMS if elderly
  hightemp_vsorres,
  daily_wbc_8a_0,
  daily_wbc_8a_m1,
  daily_wbc_8a_m2,
  age,
  daily_gcs_8a_0_processed,
  daily_gcs_8a_m1_processed,
  daily_gcs_8a_m2_processed,
  daily_resp_8a_0_code,
  daily_resp_8a_m1_code,
  daily_resp_8a_m2_code,
  
  # PNU1 Criterion #3: Symptoms/signs
  pna_cdc3,
  pna_symptoms___1,
  pna_symptoms___2,
  pna_symptoms___4,
  pna_symptoms___5,
  pna_symptoms___6,
  pna_symptoms___7,
  pna_symptoms___8,
  
  # Antimicrobial criterion
  daily_antibiotics_0,
  daily_antiviral_0,
  mh_antivirals_type___2,
  mh_antivirals_type___6,
  mh_antivirals_type___7,
  mh_antivirals_type___8,
  mh_antivirals_type___9,
  mh_antivirals_type___10,
  mh_antivirals_type___11,
  mh_antivirals_type___12,
  mh_antivirals_type___13,
  mh_antivirals_type___15,
  has_qualifying_pathogen
) {

  ## PNU1 Criterion #1: New airspace opacity (unilateral or bilateral)
  # Check calculated fields first
  criterion_1_calc <- (d0_pos == 1 | dm1_pos == 1 | dm2_pos == 1)
  
  # Also check individual opacity codes (code "2" = bilateral, code "1" = unilateral)
  criterion_1_raw <- (
    cxr_dm2_opacity_code == "1" |
    ct_dm2_opacity_code == "1" |
    cxr_dm1_opacity_code == "1" |
    ct_dm1_opacity_code == "1" |
    cxr_d0_opacity_code == "1" |
    ct_d0_opacity_code == "1"
  )
  
  # Criterion #1 met if either calculated or raw check is true
  criterion_1 <- (criterion_1_calc | criterion_1_raw)
  
  ## PNU1 Criterion #2: fever, WBC, or (if elderly) AMS
  # Sub-criterion 2a: Fever > 38°C
  criterion_2a <- (!is.na(hightemp_vsorres) & hightemp_vsorres > 38)
  
  # Sub-criterion 2b: High WBC >= 12
  criterion_2b <- (
    (!is.na(daily_wbc_8a_0) & daily_wbc_8a_0 >= 12) |
    (!is.na(daily_wbc_8a_m1) & daily_wbc_8a_m1 >= 12) |
    (!is.na(daily_wbc_8a_m2) & daily_wbc_8a_m2 >= 12)
  )
  
  # Sub-criterion 2c: Low WBC <= 4
  criterion_2c <- (
    (!is.na(daily_wbc_8a_0) & daily_wbc_8a_0 <= 4) |
    (!is.na(daily_wbc_8a_m1) & daily_wbc_8a_m1 <= 4) |
    (!is.na(daily_wbc_8a_m2) & daily_wbc_8a_m2 <= 4)
  )
  
  # Sub-criterion 2d: Elderly (>=70) with AMS
  # AMS = GCS < 15 AND not on IMV (resp codes 1 or 3)
  # GCS values have already been processed to treat T values as 15
  ams_day_0 <- (
    !is.na(daily_gcs_8a_0_processed) & 
    !is.na(daily_resp_8a_0_code) &
    as.numeric(daily_gcs_8a_0_processed) < 15 &
    !(daily_resp_8a_0_code %in% c("1", "3"))
  )
  
  ams_day_m1 <- (
    !is.na(daily_gcs_8a_m1_processed) & 
    !is.na(daily_resp_8a_m1_code) &
    as.numeric(daily_gcs_8a_m1_processed) < 15 &
    !(daily_resp_8a_m1_code %in% c("1", "3"))
  )
  
  ams_day_m2 <- (
    !is.na(daily_gcs_8a_m2_processed) & 
    !is.na(daily_resp_8a_m2_code) &
    as.numeric(daily_gcs_8a_m2_processed) < 15 &
    !(daily_resp_8a_m2_code %in% c("1", "3"))
  )
  
  criterion_2d <- (
    !is.na(age) & 
    age >= 70 & 
    (ams_day_0 | ams_day_m1 | ams_day_m2)
  )
  
  # Criterion #2 met if any sub-criterion is met
  criterion_2 <- (criterion_2a | criterion_2b | criterion_2c | criterion_2d)
  
  ## PNU1 Criterion #3: symptoms/signs present
  # Option A: pna_cdc3 == "Yes"
  criterion_3a <- (!is.na(pna_cdc3) & pna_cdc3 == "Yes")
  
  # Option B: If pna_cdc3 is missing, calculate from pna_symptoms
  # Create 4 temporary variables
  temp_var_1 <- (
    (is_checked(pna_symptoms___1) | is_checked(pna_symptoms___2)) * 1
  )
  
  temp_var_2 <- (
    (is_checked(pna_symptoms___4) | is_checked(pna_symptoms___5) | is_checked(pna_symptoms___6)) * 1
  )
  
  temp_var_3 <- is_checked(pna_symptoms___7) * 1
  
  temp_var_4 <- is_checked(pna_symptoms___8) * 1
  
  # Sum temporary variables; criterion met if pna_cdc3 is missing AND sum >= 2
  symptom_sum <- temp_var_1 + temp_var_2 + temp_var_3 + temp_var_4
  criterion_3b <- (is.na(pna_cdc3) & symptom_sum >= 2)
  
  # Use Option A if pna_cdc3 available, otherwise Option B
  criterion_3 <- (criterion_3a | criterion_3b)
  
  ## Antimicrobial criterion
  # Check if qualifying antivirals were administered
  qualifying_antiviral <- daily_antiviral_0 == 'Administered' & (
    is_checked(mh_antivirals_type___2) |
    is_checked(mh_antivirals_type___6) |
    is_checked(mh_antivirals_type___7) |
    is_checked(mh_antivirals_type___8) |
    is_checked(mh_antivirals_type___9) |
    is_checked(mh_antivirals_type___10) |
    is_checked(mh_antivirals_type___11) |
    is_checked(mh_antivirals_type___12) |
    is_checked(mh_antivirals_type___13) |
    is_checked(mh_antivirals_type___15)
  )
  
  # Antimicrobial criterion met if any of these are true
  antimicrobial_criterion <- (
    daily_antibiotics_0 == 'Administered' |
    qualifying_antiviral |
    has_qualifying_pathogen
  )
  
  ## Determine pneumonia presence
  # All 3 PNU1 criteria + antimicrobial criterion must be present
  dplyr::case_when(
    # All criteria met -> Pneumonia present
    criterion_1 & criterion_2 & criterion_3 & antimicrobial_criterion ~ 1,
    
    # At least one criterion definitively not met -> Pneumonia not present
    (!is.na(criterion_1) & !criterion_1) | 
    (!is.na(criterion_2) & !criterion_2) | 
    (!is.na(criterion_3) & !criterion_3) | 
    (!is.na(antimicrobial_criterion) & !antimicrobial_criterion) ~ 0,

    # Has valid responses that indicate no pneumonia -> Pneumonia not present
    pna_cdc3 == 'No' ~ 0,
    daily_antibiotics_0 %in% c('Administered', 'Not administered', 'UNK') ~ 0,
    daily_antiviral_0 %in% c('Administered', 'Not administered', 'UNK') ~ 0,
    
    # Otherwise -> Unknown
    TRUE ~ NA_real_
  )
}


# Convenience wrapper function
# Returns a data frame with record_id and sys_pneumonia_0 columns (one row per record_id)
wrapper_calc_sys_pneumonia_0 <- function(data, dictionary) {
  # Get enrollment times (Day 0 date)
  data_enrollment <- data |>
    filter(event_label == 'Day 0') |>
    select(record_id, enrollment_time)

  # Get qualifying respiratory pathogens identified on or before Day 0
  data_qualifying_pathogen <- data |>
    filter(event_label == 'Hospital Discharge and Summary') |>
    left_join(
      get_code_label_map('cxpos', dictionary),
      by = 'cxpos'
    ) |>
    left_join(
      get_code_label_map('resp_pathogen', dictionary),
      by = 'resp_pathogen'
    ) |>
    filter(
      cxpos_code == "3" &
      resp_pathogen_code %in% c("1", "2", "3", "4", "5", "7", "8", "17", "18", "19", "20", "21")
    ) |>
    select(record_id, pathogen_date) |>
    left_join(data_enrollment, by = 'record_id') |>
    # Keep only pathogens identified on or before Day 0
    filter(pathogen_date <= enrollment_time) |>
    # Group by patient and mark if ANY qualifying pathogen was found
    group_by(record_id) |>
    summarise(has_qualifying_pathogen = TRUE, .groups = 'drop')

  # Get antiviral type data from Day 0 event
  data_day0_antivirals <- data |>
    filter(event_label == 'Day 0') |>
    select(
      record_id,
      mh_antivirals_type___2,
      mh_antivirals_type___6,
      mh_antivirals_type___7,
      mh_antivirals_type___8,
      mh_antivirals_type___9,
      mh_antivirals_type___10,
      mh_antivirals_type___11,
      mh_antivirals_type___12,
      mh_antivirals_type___13,
      mh_antivirals_type___15
    )

  # Get data from Day 0 event (temperature, age)
  data_day0 <- data |>
    filter(event_label == 'Day 0') |>
    select(record_id, hightemp_vsorres, age)

  # Get opacity and symptom data from Syndrome Adjudication event
  data_syndrome_adjudication <- data |>
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
    select(
      record_id,
      d0_pos, dm1_pos, dm2_pos,
      cxr_dm2_opacity_code, ct_dm2_opacity_code,
      cxr_dm1_opacity_code, ct_dm1_opacity_code,
      cxr_d0_opacity_code, ct_d0_opacity_code,
      pna_cdc3,
      pna_symptoms___1, pna_symptoms___2, pna_symptoms___4,
      pna_symptoms___5, pna_symptoms___6, pna_symptoms___7, pna_symptoms___8
    )

  # Get daily assessment data (WBC, GCS, respiratory support, antibiotics, antivirals)
  data_daily <- data |>
    filter(event_label == 'Daily In-Hospital Forms') |>
    left_join(
      get_code_label_map('daily_resp_8a_0', dictionary),
      by = 'daily_resp_8a_0'
    ) |>
    left_join(
      get_code_label_map('daily_resp_8a_m1', dictionary),
      by = 'daily_resp_8a_m1'
    ) |>
    left_join(
      get_code_label_map('daily_resp_8a_m2', dictionary),
      by = 'daily_resp_8a_m2'
    ) |>
    # Process GCS values to handle T values
    mutate(
      daily_gcs_8a_0_processed = process_gcs_value(daily_gcs_8a_0),
      daily_gcs_8a_m1_processed = process_gcs_value(daily_gcs_8a_m1),
      daily_gcs_8a_m2_processed = process_gcs_value(daily_gcs_8a_m2)
    ) |>
    select(
      record_id,
      daily_wbc_8a_0, daily_wbc_8a_m1, daily_wbc_8a_m2,
      daily_gcs_8a_0_processed, daily_gcs_8a_m1_processed, daily_gcs_8a_m2_processed,
      daily_resp_8a_0_code, daily_resp_8a_m1_code, daily_resp_8a_m2_code,
      daily_antibiotics_0, daily_antiviral_0
    )

  # Calculate pneumonia classification
  data |>
    distinct(record_id) |>
    left_join(data_day0, by = 'record_id') |>
    left_join(data_syndrome_adjudication, by = 'record_id') |>
    left_join(data_daily, by = 'record_id') |>
    left_join(data_day0_antivirals, by = 'record_id') |>
    left_join(data_qualifying_pathogen, by = 'record_id') |>
    # If no qualifying pathogen found, set to FALSE
    mutate(has_qualifying_pathogen = coalesce(has_qualifying_pathogen, FALSE)) |>
    mutate(
      sys_pneumonia_0 = calc_sys_pneumonia_0(
        d0_pos = d0_pos,
        dm1_pos = dm1_pos,
        dm2_pos = dm2_pos,
        cxr_dm2_opacity_code = cxr_dm2_opacity_code,
        ct_dm2_opacity_code = ct_dm2_opacity_code,
        cxr_dm1_opacity_code = cxr_dm1_opacity_code,
        ct_dm1_opacity_code = ct_dm1_opacity_code,
        cxr_d0_opacity_code = cxr_d0_opacity_code,
        ct_d0_opacity_code = ct_d0_opacity_code,
        hightemp_vsorres = hightemp_vsorres,
        daily_wbc_8a_0 = daily_wbc_8a_0,
        daily_wbc_8a_m1 = daily_wbc_8a_m1,
        daily_wbc_8a_m2 = daily_wbc_8a_m2,
        age = age,
        daily_gcs_8a_0_processed = daily_gcs_8a_0_processed,
        daily_gcs_8a_m1_processed = daily_gcs_8a_m1_processed,
        daily_gcs_8a_m2_processed = daily_gcs_8a_m2_processed,
        daily_resp_8a_0_code = daily_resp_8a_0_code,
        daily_resp_8a_m1_code = daily_resp_8a_m1_code,
        daily_resp_8a_m2_code = daily_resp_8a_m2_code,
        pna_cdc3 = pna_cdc3,
        pna_symptoms___1 = pna_symptoms___1,
        pna_symptoms___2 = pna_symptoms___2,
        pna_symptoms___4 = pna_symptoms___4,
        pna_symptoms___5 = pna_symptoms___5,
        pna_symptoms___6 = pna_symptoms___6,
        pna_symptoms___7 = pna_symptoms___7,
        pna_symptoms___8 = pna_symptoms___8,
        daily_antibiotics_0 = daily_antibiotics_0,
        daily_antiviral_0 = daily_antiviral_0,
        mh_antivirals_type___2 = mh_antivirals_type___2,
        mh_antivirals_type___6 = mh_antivirals_type___6,
        mh_antivirals_type___7 = mh_antivirals_type___7,
        mh_antivirals_type___8 = mh_antivirals_type___8,
        mh_antivirals_type___9 = mh_antivirals_type___9,
        mh_antivirals_type___10 = mh_antivirals_type___10,
        mh_antivirals_type___11 = mh_antivirals_type___11,
        mh_antivirals_type___12 = mh_antivirals_type___12,
        mh_antivirals_type___13 = mh_antivirals_type___13,
        mh_antivirals_type___15 = mh_antivirals_type___15,
        has_qualifying_pathogen = has_qualifying_pathogen
      )
    ) |>
    select(record_id, sys_pneumonia_0)
}
