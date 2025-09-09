#' Derive variable `neuromuscular_blockade_0`
#'
#' This variable represents daily neuromuscular blocking agent use on admission day
#' Returns vector with values:
#' - 0 = No neuromuscular blockade on day 0 or missing
#' - 1 = Neuromuscular blockade given on day 0"
derive_neuromuscular_blockade_0 <- function(data) {

  if (is.null(data$daily_paralysis_0)) {
      stop(
        "Unable to derive neuromuscular blockade:",
        "Variable 'daily_paralysis_0' not found in data."
      )
  }

  nmb <- as.character(data$daily_paralysis_0)

  nmb <- replace(nmb, is.na(nmb) | nmb == 'UNK' | nmb == 'Not administered', 0)
  nmb <- replace(nmb, nmb == 'Administered', 1)

  as.numeric(nmb)
}

#' Derive variable `inflammatory_profile_0`
#'
#' This variable represents hyperinflammatory profile — CRP, ferritin, fibrinogen, D-dimer closest to 8am on this day
#' TODO: Currently this function only derives inflammatory profile for streamlined, not systematic. Systematic will be added later.
#' Returns vector with values:
#' - 0 = Day 0 CRP not checked or < 15
#' - 1 = Day 0 CRP checked and ≥ 15
derive_inflammatory_profile_0 <- function(data, is.streamlined = FALSE) {

    if (is.null(data$daily_crp_8a_0)) {
    stop(
      "Unable to derive inflammatory profile:",
      "Variable 'daily_crp_8a_0' not found in data."
    )
  }


  ifp <- data$daily_crp_8a_0

  ifp <- replace(ifp, is.na(ifp) | ifp < 15, 0)
  ifp <- replace(ifp, ifp >= 15, 1)

  ifp
}

