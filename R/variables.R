derive_neuromuscular_blockade_0 <- function(data) {

  if (is.null(data$daily_paralysis_0)) {
      stop("Unable to derive neuromuscular blocakde: Variable 'daily_paralysis_0' not found in data")
  }

  nmb <- as.character(data$daily_paralysis_0)

  nmb <- replace(nmb, is.na(nmb), 0)
  nmb <- replace(nmb, nmb == 'UNK', 0)
  nmb <- replace(nmb, nmb == 'Not administered', 0)
  nmb <- replace(nmb, nmb == 'Administered', 1)

  as.numeric(nmb)
}
