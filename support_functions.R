require('dplyr')

## Data dictionary functions

## parse the "select_choices_or_calculations" field in the data dictionary
## for checkbox and multiple choice fields
## choices - a string vector of length 1
parse_choices <-  function(choices) {
  if(length(choices) != 1)
    stop("'choices' must have length 1")
  keyvals <- strsplit(choices, split = ' ?\\| ?')[[1]]
  keys <- sub('^([^,]+),.+$', '\\1', keyvals)
  values <- sub('^[^,]+, (.+)$', '\\1', keyvals)
  data.frame(key=keys, value=values)
}

## create a label-to-code mapping table for a multiple choice field
## field_name - name of field corresponding to 'field_name' in dictionary
## dictionary - data dictionary table
get_code_label_map <- function(field_name, dictionary) {
  field_name_orig <- field_name
  field_name_code <- paste0(field_name, '_code')
  dictionary %>%
    filter(field_name == field_name_orig) %>%
    pull(select_choices_or_calculations) %>%
    parse_choices() %>%
    rename(!!field_name_orig := value,
           !!field_name_code := key)
}


## View common dictionary fields for a given field name
## field - name of field corresponding to 'field_name' in dictionary
## dictionary - data dictionary table
view_dict <- function(field, dictionary) {
  dictionary %>%
    filter(field_name == field) %>%
    select('field_name', 'field_type', 'form_label', 'field_label',
           'select_choices_or_calculations', 'branching_logic') %>%
    unclass()
}

# Helper function to determine the event label associated with a given data field
view_label <- function(data, field) {
  # Handle both quoted strings and unquoted names
  field_name <- rlang::as_name(rlang::enquo(field))

  data |>
    filter(!is.na(.data[[field_name]])) |>
    count(event_label) |>
    pull(event_label) |>
    as.character()
}


#' Helper function to identify "Checked" values
is_checked <- function(x) {
  x == "Checked"
}

#' Helper function to identify "Unchecked" values
is_unchecked <- function(x) {
  x == "Unchecked"
}

#' Helper function to identify "Unknown" values
is_unknown <- function(x) {
  x == "Unknown"
}

# Helper function: Get value with lookback (Day 0 -> Day -1 -> Day -2)
# - Used to find the most recent non-missing value for a given variable
get_value_with_lookback <- function(val_0, val_m1, val_m2) {
  dplyr::coalesce(val_0, val_m1, val_m2)
}

# Helper function: Process single GCS value, treating T and 'not documented' as 15
# - If GCS contains 'T' or is 'not documented', return "15" (normal GCS, 0 SOFA points)
# - Otherwise return the GCS value
# - This is consistent with Footnote C from the SOFA 2 paper
process_gcs_value <- function(daily_gcs_8a) {
  # Convert to character from factor
  gcs <- as.character(daily_gcs_8a)

  # Treat 'not documented' as 15
  gcs <- ifelse(gcs == 'not documented', "15", gcs)

  # Treat any value with 'T' as 15 (normal GCS)
  gcs <- ifelse(grepl('T', gcs, fixed = TRUE), "15", gcs)

  return(gcs)
}

# Helper function: Get GCS with lookback, excluding T values and 'not documented'
# - 1. Look back to find the last available value without a T
# - 2. If no prior value available without a T, then assume a "normal" GCS/SOFA score for
#      that patient. Award 0 GCS/SOFA points (return "15" which corresponds to 0 points for
#      both measures). This is consistent with Footnote C from the SOFA 2 paper.
get_gcs_with_lookback <- function(daily_gcs_8a_0, daily_gcs_8a_m1, daily_gcs_8a_m2) {
  # Convert to character from factor for all time points
  gcs_0 <- as.character(daily_gcs_8a_0)
  gcs_m1 <- as.character(daily_gcs_8a_m1)
  gcs_m2 <- as.character(daily_gcs_8a_m2)

  # Treat 'not documented' as NA for all time points
  gcs_0 <- ifelse(gcs_0 == 'not documented', NA, gcs_0)
  gcs_m1 <- ifelse(gcs_m1 == 'not documented', NA, gcs_m1)
  gcs_m2 <- ifelse(gcs_m2 == 'not documented', NA, gcs_m2)

  # Look back to find the last available value without a T
  gcs <- dplyr::case_when(
    !is.na(gcs_0) & !grepl('T', gcs_0, fixed = TRUE) ~ gcs_0,
    !is.na(gcs_m1) & !grepl('T', gcs_m1, fixed = TRUE) ~ gcs_m1,
    !is.na(gcs_m2) & !grepl('T', gcs_m2, fixed = TRUE) ~ gcs_m2,
    .default = "15"  # Default to normal GCS if no value without T is found
  )

  return(gcs)
}

# Helper function: Display count table with grand total row
# Usage: data |> display_count_with_total(column_name)
# - Most commonly used in .Rmd files to dispaly a count table for a given
#   variable with a grand total row at the bottom
display_grand_total <- function(data, col) {
  data |>
    count({{ col }}) |>
    gt() |>
    grand_summary_rows(
      columns = n,
      fns = list(label = md("**TOTAL**"), id = "totals") ~ sum(.)
    )
}


## SOFA score functions

## calculate p/f ratio for respiratory SOFA score
## low_pao2 - lowest PaO2 (e.g., daily_pa02_lowest_m2)
## resp_low_pao2 - respiratory support at lowest PaO2 (e.g., daily_resp_lowest_pao2_m2)
## fio2_low_pao2 - FiO2 at lowest PaO2 (e.g., daily_fio2_lowest_pao2_m2)
## o2_low_pao2 - Std. Oxygen L/min at lowest PaO2 (e.g., daily_o2_lowest_pao2_m2)
calc_pf_ratio <- function(low_pao2, resp_low_pao2, fio2_low_pao2, o2_low_pao2) {
  case_when(

    ## patient on ECMO without invasive mechanical ventilation (FiO2 is nonsense)
    resp_low_pao2 == 'ECMO without invasive mechanical ventilation' ~ NA,

    ## lowest PaO2 available and patient on IMV, NIV, or HFNC
    !is.na(low_pao2) & !is.na(fio2_low_pao2) ~ low_pao2/fio2_low_pao2,

    ## lowest PaO2 available and patient on supplemental oxygen (0.21 + 0.03 * L/min)
    !is.na(low_pao2) & !is.na(o2_low_pao2) ~ low_pao2/pmin(0.21 + 0.03*o2_low_pao2, 1),

    ## lowest PaO2 available and patient on room air
    !is.na(low_pao2) & grepl('^No respiratory support', resp_low_pao2) ~ low_pao2/0.21,

    TRUE ~ NA
  )
}

## calculate s/f ratio for respiratory SOFA score
## low_spo2 - lowest SpO2 (e.g., daily_spo2_lowest_m2)
## resp_low_spo2 - respiratory support at lowest SpO2 (e.g., daily_resp_lowest_m2)
## fio2_low_spo2 - FiO2 at lowest SpO2 (e.g., daily_fio2_lowest_m2)
## o2_low_spo2 - Std. Oxygen L/min at lowest SpO2 (e.g., daily_o2_lowest_m2)
calc_sf_ratio <- function(low_spo2, resp_low_spo2, fio2_low_spo2, o2_low_spo2) {
  case_when(

    ## low SpO2 >97; S/F ratio invalid
    !is.na(low_spo2) & low_spo2 > 97 ~ NA,

    ## patient on ECMO without invasive mechanical ventilation (FiO2 is nonsense)
    resp_low_spo2 == 'ECMO without invasive mechanical ventilation' ~ NA,

    ## lowest SpO2 available and patient on IMV, NIV, or HFNC
    !is.na(low_spo2) & !is.na(fio2_low_spo2) ~ low_spo2/fio2_low_spo2,

    ## lowest SpO2 available and patient on supplemental oxygen (0.21 + 0.03 * L/min)
    !is.na(low_spo2) & !is.na(o2_low_spo2) ~ low_spo2/pmin(0.21 + 0.03*o2_low_spo2, 1),

    ## lowest SpO2 available and patient on room air
    !is.na(low_spo2) & grepl('^No respiratory support', resp_low_spo2) ~ low_spo2/0.21,

    TRUE ~ NA
  )
}

## calculate respiratory SOFA score
## pf_ratio - worst PaO2/FiO2 ratio
## sf_ratio - worst SpO2/FiO2 ratio
calc_sofa_resp <- function(pf_ratio, sf_ratio) {
  case_when(
    pf_ratio  < 100 | sf_ratio <  67  ~ 4,
    pf_ratio  < 200 | sf_ratio <  142 ~ 3,
    pf_ratio  < 300 | sf_ratio <  221 ~ 2,
    pf_ratio  < 400 | sf_ratio <  302 ~ 1,
    pf_ratio >= 400 | sf_ratio >= 302 ~ 0,
    TRUE ~ NA
  )
}

## calculate coagulation SOFA score
## platelets - platelets (10^3/mm^3)
calc_sofa_coag <- function(platelets) {
  case_when(
    platelets  < 20 ~ 4,
    platelets  < 50 ~ 3,
    platelets  < 100 ~ 2,
    platelets  < 150 ~ 1,
    platelets >= 150 ~ 0,
    TRUE ~ NA
  )
}

## calculate liver SOFA score
## bilirubin - bilirubin (mg/dL)
calc_sofa_livr <- function(bilirubin) {
  case_when(
    bilirubin >= 12 ~ 4,
    bilirubin >= 6 ~ 3,
    bilirubin >= 2 ~ 2,
    bilirubin >= 1.2 ~ 1,
    bilirubin  < 1.2 ~ 0,
    TRUE ~ NA
  )
}

## calculate cardiovascular SOFA score
## sbp - SBP (mmHg)
## dbp - DBP (mmHg)
## dopa_mcg - dopamine dose (mcg/min)
## dopa_mcgkg - dopamine dose (mcg/min/kg)
## dobu_mcg - dobutamine dose (mcg/min)
## dobu_mcgkg - dobutamine dose (mcg/min/kg)
## epin_mcg - epinephrine dose (mcg/min)
## epin_mcgkg - epinephrine dose (mcg/min/kg)
## nore_mcg - norepinephrine dose (mcg/min)
## nore_mcgkg - norepinephrine dose (mcg/min/kg)
## weight_kg - weight (kg)
calc_sofa_card <- function(sbp, dbp, dopa_mcg, dopa_mcgkg, dobu_mcg, dobu_mcgkg,
                           epin_mcg, epin_mcgkg, nore_mcg, nore_mcgkg, weight_kg) {
  map <- 1/3*sbp + 2/3*dbp
  case_when(
    dopa_mcgkg > 15  | dopa_mcg/weight_kg > 15  |
      epin_mcgkg > 0.1 | epin_mcg/weight_kg > 0.1 |
      nore_mcgkg > 0.1 | nore_mcg/weight_kg > 0.1  ~ 4,
    dopa_mcgkg > 5.1 | dopa_mcg/weight_kg > 5.1 |
      epin_mcgkg > 0.0 | epin_mcg/weight_kg > 0.0 |
      nore_mcgkg > 0.0 | nore_mcg/weight_kg > 0.0  ~ 3,
    dopa_mcgkg > 0.0 | dopa_mcg/weight_kg > 0.0 |
      dobu_mcgkg > 0.0 | dobu_mcg/weight_kg > 0.0  ~ 2,
    map < 70                                       ~ 1,
    map >= 70                                      ~ 0,
    TRUE ~ NA
  )
}

## calculate CNS SOFA score
## gcs - Glasgow Coma Score
calc_sofa_cns <- function(gcs) {

  ## treat 'not documented' as NA
  gcs <- ifelse(gcs == 'not documented', NA, gcs)
  ## GCS contains T/patient was intubated/no verbal score
  gcs_intub <- grepl('T$', gcs)
  ## extract quantitative component of GCS
  gcs_quant <- as.numeric(sub('T', '', gcs))

  ## imputed GCS
  gcs_imput <- case_when(
    TRUE ~ gcs_quant
  )

  ## convert to SOFA CNS score
  case_when(
    gcs_imput <  6  ~ 4,
    gcs_imput <= 9  ~ 3,
    gcs_imput <= 12 ~ 2,
    gcs_imput <= 14 ~ 1,
    gcs_imput == 15 ~ 0,
    TRUE ~ NA
  )
}

## calculate renal SOFA score
## cr - creatinine concentration (mg/dL)
calc_sofa_rena <- function(cr) {
  case_when(
    cr > 5.0 ~ 4,
    cr > 3.5 ~ 3,
    cr > 2.0 ~ 2,
    cr > 1.2 ~ 1,
    cr > 0.0 ~ 0,
    TRUE ~ NA
  )
}

## calculate respiratory SOFA-2 score
## pf_ratio  - worst PaO2/FiO2 ratio
## sf_ratio  - worst SpO2/FiO2 ratio
## resp_low_spo2 - Respiratory support at the time of lowest SpO2 measurement on this day
calc_sofa_2_resp <- function(pf_ratio, sf_ratio, resp_low_pao2, resp_low_spo2) {
  
  ## if P/F available, use that and corresponding respiratory support value
  pf_available <- !is.na(pf_ratio) 
  resp_low <- ifelse(pf_available, resp_low_pao2, resp_low_spo2)
  
  ## ECMO
  ecmo <- grepl('ECMO', resp_low)
  
  ## advanced ventilatory support
  adv_vent <- case_when(
    resp_low %in% c(
      'ECMO and invasive mechanical ventilation',
      'Invasive mechanical ventilation without ECMO',
      'Non-invasive ventilation',
      'High-flow nasal oxygen (high flow nasal oxygen)') ~ TRUE,
    resp_low %in% c(
      'ECMO without invasive mechanical ventilation',
      'Standard flow supplemental oxygen',
      'No respiratory support or supplemental oxygen (spontaneously breathing room air)') ~ FALSE,
    TRUE ~ NA)
  
  case_when(
    ## any ECMO
    ecmo ~ 4,
    
    ## P/F criteria
    ( pf_available & pf_ratio <= 75 ) & (is.na(adv_vent) | adv_vent) ~ 4,
    ( pf_available & pf_ratio <= 150) & (is.na(adv_vent) | adv_vent) ~ 3,
    ( pf_available & pf_ratio <= 225)                                ~ 2,
    ( pf_available & pf_ratio <= 300)                                ~ 1,
    ( pf_available & pf_ratio >  300)                                ~ 0,
    
    ## S/F criteria
    (!pf_available & sf_ratio <= 120) & (is.na(adv_vent) | adv_vent) ~ 4,
    (!pf_available & sf_ratio <= 200) & (is.na(adv_vent) | adv_vent) ~ 3,
    (!pf_available & sf_ratio <= 249)                                ~ 2,
    (!pf_available & sf_ratio <= 300)                                ~ 1,
    (!pf_available & sf_ratio >  300)                                ~ 0,
    
    ## not enough information
    TRUE ~ NA
  )
}

## calculate coagulation SOFA-2 score
## platelets - platelets (10^3/mm^3)
calc_sofa_2_coag <- function(platelets) {
  case_when(
    platelets <= 50  ~ 4,
    platelets <= 80  ~ 3,
    platelets <= 100 ~ 2,
    platelets <= 150 ~ 1,
    platelets >  150 ~ 0,
    TRUE ~ NA
  )
}

## calculate liver SOFA-2 score
## bilirubin - bilirubin (mg/dL)
calc_sofa_2_livr <- function(bilirubin) {
  case_when(
    bilirubin >= 12  ~ 4,
    bilirubin >  6   ~ 3,
    bilirubin >  3   ~ 2,
    bilirubin >  1.2 ~ 1,
    bilirubin <= 1.2 ~ 0,
    TRUE ~ NA
  )
}

## calculate cardiovascular SOFA-2 score
## sbp        - SBP (mmHg)
## dbp        - DBP (mmHg)
## dopa_mcg   - dopamine dose (mcg/min)
## dopa_mcgkg - dopamine dose (mcg/min/kg)
## dobu_mcg   - dobutamine dose (mcg/min)
## dobu_mcgkg - dobutamine dose (mcg/min/kg)
## epin_mcg   - epinephrine dose (mcg/min)
## epin_mcgkg - epinephrine dose (mcg/min/kg)
## nore_mcg   - norepinephrine dose (mcg/min)
## nore_mcgkg - norepinephrine dose (mcg/min/kg)
## phen_mcg   - phenylephrine dose (mcg/min)
## phen_mcgkg - phenylephrine dose (mcg/min/kg)
## vaso_dose  - vasopressin dose (units/min)
## ang2_mcg   - angiotensin II dose (mcg/min)
## ang2_mcgkg - angiotensin II dose (mcg/min/kg)
## weight_kg  - weight (kg)
## resp_supp  - respiratory support
calc_sofa_2_card <- function(sbp, dbp, dopa_mcg, dopa_mcgkg, dobu_mcg, dobu_mcgkg,
                             epin_mcg, epin_mcgkg, nore_mcg, nore_mcgkg, 
                             phen_mcg, phen_mcgkg, vaso_dose,ang2_mcg, ang2_mcgkg, 
                             weight_kg, resp_supp) {
  
  ## calculate MAP
  map <- 1/3*sbp + 2/3*dbp
  
  ## convert to mcg/kg/min
  dopa_mcgkg <- ifelse(!is.na(dopa_mcgkg), dopa_mcgkg, dopa_mcg/weight_kg)
  dobu_mcgkg <- ifelse(!is.na(dobu_mcgkg), dobu_mcgkg, dobu_mcg/weight_kg)
  epin_mcgkg <- ifelse(!is.na(epin_mcgkg), epin_mcgkg, epin_mcg/weight_kg)
  nore_mcgkg <- ifelse(!is.na(nore_mcgkg), nore_mcgkg, nore_mcg/weight_kg)
  phen_mcgkg <- ifelse(!is.na(phen_mcgkg), phen_mcgkg, phen_mcg/weight_kg)
  ang2_mcgkg <- ifelse(!is.na(ang2_mcgkg), ang2_mcgkg, ang2_mcg/weight_kg)
  
  ## if missing infusion rate, set equal to zero
  dopa_mcgkg <- ifelse(!is.na(dopa_mcgkg), dopa_mcgkg, 0)
  dobu_mcgkg <- ifelse(!is.na(dobu_mcgkg), dobu_mcgkg, 0)
  epin_mcgkg <- ifelse(!is.na(epin_mcgkg), epin_mcgkg, 0)
  nore_mcgkg <- ifelse(!is.na(nore_mcgkg), nore_mcgkg, 0)
  phen_mcgkg <- ifelse(!is.na(phen_mcgkg), phen_mcgkg, 0)
  ang2_mcgkg <- ifelse(!is.na(ang2_mcgkg), ang2_mcgkg, 0)
  vaso_dose  <- ifelse(!is.na(vaso_dose), vaso_dose, 0)
  
  ## is dopamine the only vasopressor used?
  dopa_only <- 
    dopa_mcgkg >  0 &
    dobu_mcgkg == 0 &
    epin_mcgkg == 0 &
    nore_mcgkg == 0 &
    phen_mcgkg == 0 &
    ang2_mcgkg == 0 &
    vaso_dose  == 0
    
  ecmo <- grepl('ECMO', resp_supp)
  
  ## calculate cardiovascular component
  case_when(
    epin_mcgkg + nore_mcgkg > 0.4 |
      ((epin_mcgkg + nore_mcgkg > 0.2) &
         (dopa_mcgkg > 0.0 |
          dobu_mcgkg > 0.0 |
          phen_mcgkg > 0.0 | 
          vaso_dose  > 0.0 |
          ang2_mcgkg > 0.0 |
          ecmo)) |
      (dopa_only & dopa_mcgkg > 40) ~ 4,
    epin_mcgkg + nore_mcgkg > 0.2 |
      ((epin_mcgkg + nore_mcgkg > 0) &
         (dopa_mcgkg > 0.0 |
            dobu_mcgkg > 0.0 |
            phen_mcgkg > 0.0 | 
            vaso_dose  > 0.0 |
            ang2_mcgkg > 0.0)) |
      (dopa_only & dopa_mcgkg > 20) ~ 3,
    epin_mcgkg + nore_mcgkg > 0.0 |
      dopa_mcgkg > 0.0 |  
      dobu_mcgkg > 0.0 | 
      phen_mcgkg > 0.0 | 
      vaso_dose  > 0.0 |
      ang2_mcgkg  > 0.0 ~ 2,
    map < 70  ~ 1,
    map >= 70 ~ 0,
    TRUE ~ NA
  )
}

## calculate CNS SOFA-2 score
## gcs - Glasgow Coma Score
## del_med - TRUE/FALSE - Any drug treatment for delirium
calc_sofa_2_cns <- function(gcs, del_med = FALSE) {
  
  ## treat 'not documented' as missing
  gcs <- ifelse(gcs == 'not documented', NA, gcs)

  ## GCS contains T/patient was intubated/no verbal score:
  gcs_imput <- ifelse(grepl('T$', gcs),
    ## extract quantitative component of GCS and add 4
    as.numeric(sub('T', '', gcs))+4,
    ## otherwise take gcs value as is
    gcs)

  ## convert to SOFA CNS score
  score <- case_when(
    gcs_imput <= 5  ~ 4,
    gcs_imput <= 8  ~ 3,
    gcs_imput <= 12 ~ 2,
    gcs_imput <= 14 ~ 1,
    gcs_imput == 15 ~ 0,
    TRUE ~ NA
  )
  
  ## if any drug treatment for delirium score at least 1
  score <- ifelse(score == 0 & del_med, 1, score)
  
  return(score)
}

## calculate renal SOFA-2 score
## cr  - creatinine concentration (mg/dL)
## rrt - TRUE/FALSE - chronic or new RRT
## uop - urine output (mL)
calc_sofa_2_rena <- function(cr, rrt, uop) {
  case_when(
    rrt ~ 4,
    cr > 3.5 ~ 3,
    cr > 2.0 ~ 2,
    cr > 1.2 ~ 1,
    cr > 0.0 ~ 0,
    TRUE ~ NA
  )
}

## last observation carry forward (LOCF)
## x - vector to impute
locf <- function(x) {
  
  ## return if length == 0
  if(!length(x))
    stop('length zero "x"')
  
  ## no imputation if all missing
  na <- is.na(x)
  if (all(na)) 
    return(x)
  
  ## fill gaps
  ok <- which(!na)
  if (is.na(x[1L])) 
    ok <- c(1L, ok)
  gaps <- diff(c(ok, length(x) + 1L))
  
  return(rep(x[ok], gaps))
}

## impute mising values by imputing 'normal' (score 0) for the first eligible
## missing value for among study days -2:0
## score - score vector to impute
## impute_eligible - TRUE/FALSE vector is the score eligible to impute
## study_day - study day
calc_sofa_2_impute <- function(score, impute_eligible, study_day) {
  
  ## mark missing values before imputation
  na <- is.na(score)
  
  ## find index of first value eligible for imputation
  idx <- match(TRUE, study_day <= 0 & impute_eligible & is.na(score))
  if(!is.na(idx))
    score[idx] <- 0
  
  ## last observation carry forward
  score <- locf(score)
  
  ## mark missing ineligible scores as NA
  score[!impute_eligible & na] <- NA
  
  return(score)
}


## calculate norepinephrine equivalents (NEE) for vasopressor dosing
## Converts all vasopressor and inotrope doses to norepinephrine-equivalent mcg/min
## Used in SOFA cardiovascular scoring and septic shock definitions
##
## Parameters: doses in mcg/min or mcg/min/kg for various agents
## daily_ne_dose_8a_0_mcg - norepinephrine dose (mcg/min)
## daily_ne_dose_8a_0_mcgkg - norepinephrine dose (mcg/min/kg)
## daily_epi_dose_8a_0_mcg - epinephrine dose (mcg/min)
## daily_epi_dose_8a_0_mcgkg - epinephrine dose (mcg/min/kg)
## daily_phen_dose_8a_0_mcg - phenylephrine dose (mcg/min)
## daily_phen_dos_8a_0_mcgkg - phenylephrine dose (mcg/min/kg)
## daily_vaso_dose_8a_0 - vasopressin dose (units/min)
## daily_dopa_dose_8a_0_mcg - dopamine dose (mcg/min)
## daily_dopa_dos_8a_0_mcgkg - dopamine dose (mcg/min/kg)
## daily_ang2_8a_0_mcg - angiotensin II dose (mcg/min)
## daily_ang2_8a_0_mcgkg - angiotensin II dose (mcg/min/kg)
## m_weight_kg - patient weight (kg)
##
## Returns: norepinephrine equivalent dose (mcg/min)
## Conversion factors: NE = 1, Epi = 1, Phenylephrine / 10, Vasopressin * 2.5,
##                     Dopamine / 100, Angiotensin II * 10
calc_norepi_equivalents <- function(
  daily_ne_dose_8a_0_mcg,
  daily_ne_dose_8a_0_mcgkg,
  daily_epi_dose_8a_0_mcg,
  daily_epi_dose_8a_0_mcgkg,
  daily_phen_dose_8a_0_mcg,
  daily_phen_dos_8a_0_mcgkg,
  daily_vaso_dose_8a_0,
  daily_dopa_dose_8a_0_mcg,
  daily_dopa_dos_8a_0_mcgkg,
  daily_ang2_8a_0_mcg,
  daily_ang2_8a_0_mcgkg,
  m_weight_kg
) {

  ## Convert all doses to mcg/min using weight if needed
  ne_dose <- dplyr::coalesce(daily_ne_dose_8a_0_mcg, daily_ne_dose_8a_0_mcgkg * m_weight_kg, 0)
  epi_dose <- dplyr::coalesce(daily_epi_dose_8a_0_mcg, daily_epi_dose_8a_0_mcgkg * m_weight_kg, 0)
  phen_dose <- dplyr::coalesce(daily_phen_dose_8a_0_mcg, daily_phen_dos_8a_0_mcgkg * m_weight_kg, 0)
  dopa_dose <- dplyr::coalesce(daily_dopa_dose_8a_0_mcg, daily_dopa_dos_8a_0_mcgkg * m_weight_kg, 0)
  ang2_dose <- dplyr::coalesce(daily_ang2_8a_0_mcg, daily_ang2_8a_0_mcgkg * m_weight_kg, 0)
  vaso_dose <- dplyr::coalesce(daily_vaso_dose_8a_0, 0)

  ## Calculate norepinephrine equivalents
  ## NE = 1, Epi = 1, Phenylephrine = 10, Vasopressin = 0.04 units = 1 mcg NE equivalent
  ## Dopamine at high doses ~ 0.01, Angiotensin II ~ 1
  norepi_equiv <- ne_dose +
                  epi_dose +
                  (phen_dose / 10) +
                  (vaso_dose * 2.5) +

                  (dopa_dose / 100) +
                  (ang2_dose * 10)

  return(norepi_equiv)
}