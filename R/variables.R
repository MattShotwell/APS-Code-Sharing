#' Derive variable `neuromuscular_blockade_0`
#'
#' This variable represents daily neuromuscular blocking agent use on admission day
#' Values:
#' - 0 = No neuromuscular blockade on day 0 or missing
#' - 1 = Neuromuscular blockade given on day 0"
derive_neuromuscular_blockade_0 <- function(data) {

  if (is.null(data$daily_paralysis_0)) {
      stop("Unable to derive neuromuscular blocakde: Variable 'daily_paralysis_0' not found in data")
  }

  nmb <- as.character(data$daily_paralysis_0)

  nmb <- replace(nmb, is.na(nmb) | nmb == 'UNK' | nmb == 'Not administered', 0)
  nmb <- replace(nmb, nmb == 'Administered', 1)

  as.numeric(nmb)
}


