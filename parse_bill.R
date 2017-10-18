library(rvest)
library(tidyverse)

source("lines.R")
source("tidy_equipment_charges.R")
source("charges_to_df.R")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

divvy_charges <- function(month_year, lines) {
  bill_path <- file.path(
    "html_bills", 
    paste0(month_year, ".html")
  )
  bill_df <- parse_bill(bill_path, lines)
  write_csv(bill_df, file.path("bill_dfs", paste0(month_year, ".csv")))
  bill_df <- bill_df %>%
    rename_at(vars(starts_with("Monthly plan charges")), ~ "Monthly plan charges") %>%
    mutate("Charge per line" = getmode(`Monthly plan charges`),
           "Base plan charge" = max(`Monthly plan charges`) - `Charge per line`, 
           "Divvied base charge" = `Base plan charge` / nrow(bill_df),
           "Government taxes & fees" = if_else(
             is.na(`Government taxes & fees`), 
             0, `Government taxes & fees`
           )
    )
  bill_summary <- bill_df %>%
    mutate(Owes = 
             `Charge per line` + 
             `Divvied base charge` + 
             `Equipment charges` + 
             `Surcharges & fees` + 
             `Government taxes & fees`
    ) %>%
    mutate(Owes = round(Owes, 2)) %>%
    select(Name, Owes) %>%
    bind_rows(tibble("Name" = "Total", "Owes" = sum(.$Owes)))
  write_tsv(bill_summary, 
            file.path(
              "bill_summaries", 
              paste0(month_year, ".txt")
            )
  )
}

parse_bill <- function(bill_path, lines) {
  bill_html <- read_html(bill_path)
  map_dfr(
    lines, 
    function(ph) {
      line_tbl_selector <- sprintf("div[id*='%s']", ph)
      line_tbls <- html_nodes(bill_html, line_tbl_selector)
      charges <- map(line_tbls, html_nodes, "div.faux-table-cell") %>%
        map(html_text, trim = TRUE)
      is_equipment_charge <- map_lgl(charges, ~ .[1] == "Equipment charges")
      if (any(is_equipment_charge)) {
        charges[is_equipment_charge] <- tidy_equipment_charges(
          charges[is_equipment_charge]
        )
      } else {
        charges <- c(
          charges, 
          list(
            c(
              "Equipment charges", "0", 
              "Payments left", NA
            )
          )
        )
      }
      bind_cols(
        tibble(
          "Name" = names(lines)[lines == ph], 
          "Number" = ph
        ), 
        
        map_dfc(charges, charges_to_df)
      )
    }
  )
}