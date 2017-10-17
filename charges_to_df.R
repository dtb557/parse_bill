charges_to_df <- function(charge_vec) {
  require(stringr)
  require(tibble)
  lbls <- charge_vec[seq(1, length(charge_vec), by = 2)]
  vals <- charge_vec[seq(2, length(charge_vec), by = 2)] %>%
    str_replace("\\$", "") %>%
    as.numeric() %>%
    as.list()
  names(vals) <- lbls
  as.tibble(vals)
}