reformat_phone_number <- function(ph) {
  ph %>%
    str_replace_all("\\.", "-") %>%
    str_replace_all(" ", "")
}