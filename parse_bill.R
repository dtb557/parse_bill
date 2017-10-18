library(rvest)
library(tidyverse)

source("lines.R")
source("tidy_equipment_charges.R")
source("charges_to_df.R")
source("reformat_phone_number.R")
source("round_up_to_nearest_cent.R")

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
  usage_path <- file.path("html_usage", paste0(month_year, ".html"))
  usage <- parse_usage(usage_path, lines)
  write_csv(usage, file.path("usage_summaries", paste0(month_year, ".csv")))
  if("Data & text usage charges" %in% names(bill_df)) {
    cat("It appears there are overage charges on this bill; check bill_df to make sure they are divvied properly.\n")
    # If overage is assigned to primary user, assume it's a plan-wide overage
    # Else assume it is a line-specific overage
    bill_df <- bill_df %>%
      left_join(usage, by = "Name") %>%
      mutate(
        `Has usage charges` = !is.na(`Data & text usage charges`),
        `Plan-wide overages` = if_else(
          `Has usage charges`[1], 
          `Data & text usage charges`[1], 0
        ), 
        `Line-specific overages` = if_else(
          `Has usage charges`,
          `Data & text usage charges`, 0
        ),
        `Share of usage` = Usage / sum(Usage), 
        `Share of plan-wide overages` = `Plan-wide overages` * `Share of usage`
      )
    
    bill_df$`Line-specific overages`[1] <- 0
  } else {
    bill_df <- bill_df %>%
      mutate(
        `Share of plan-wide overages` = 0, 
        `Line-specific overages` = 0
      )
  }
  write_csv(bill_df, file.path("bill_dfs", paste0(month_year, ".csv")))
  bill_summary <- bill_df %>%
    mutate(Owes = 
             `Charge per line` + 
             `Divvied base charge` + 
             `Equipment charges` + 
             `Surcharges & fees` + 
             `Government taxes & fees` + 
             `Share of plan-wide overages` + 
             `Line-specific overages`
    ) %>%
    mutate(Owes = round_up_to_nearest_cent(Owes)) %>%
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

parse_usage <- function(usage_path, lines) {
  usage_html <- read_html(usage_path)
  number_selector <- "#ac8panel0 .letterSpaceScrReader"
  numbers <- usage_html %>%
    html_nodes(number_selector) %>%
    html_text(trim = TRUE) %>%
    map_chr(reformat_phone_number)
  usage_selector <- ".dataUsageCount .ng-binding"
  usage <- usage_html %>%
    html_nodes(usage_selector) %>%
    html_text(trim = TRUE) %>%
    as.numeric()
  usage <- usage[!is.na(usage)]
  tibble(
    Name = names(lines)[map_int(numbers, ~ which(lines == .))], 
    Usage = usage
  )
}