tidy_equipment_charges <- function(ec) {
  require(stringr)
  installment_pattern <- "Installment (\\d{1,2}) of (\\d{1,2})"
  ec <- ec[[1]]
  payments_left <- ec %>%
    .[str_detect(., installment_pattern)] %>%
    str_match(installment_pattern) %>%
    .[ , 2:3] %>%
    as.integer() %>%
    diff()
  list(c(ec[1:2], "Payments left", payments_left))
}