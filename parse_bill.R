library(rvest)
library(tidyverse)

source("lines.R")
source("tidy_equipment_charges.R")
source("charges_to_df.R")

divvy_charges <- function(month_year, lines) {
  bill_path <- file.path(
    "html_bills", 
    paste0(month_year, ".html")
  )
  bill_df <- parse_bill(bill_path, lines)
  write_csv(bill_df, file.path("bill_dfs", paste0(month_year, ".csv")))
  bill_df <- bill_df %>%
    rename_at(vars(starts_with("Monthly plan charges")), ~ "Monthly plan charges") %>%
  bill_summary <- bill_df %>%
    select()
}

bill_path <- "html_bills/sep_2017_my_bill.html"

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

bill_df <- parse_bill(bill_path, lines)

write_csv(bill_df, )




# CSS selectors -----------------------------------------------------------

name_number_total_selector <- ".margin-bottom20 div:nth-child(2) .no-dots b"
monthly_plan_charges_selector <- ".MonthlyChargeAlert .no-dots .ng-binding"
equipment_charges_selector <- ".EquipmentAlert b"
surcharges_fees_selector <- "#printfa .ng-scope .ng-scope :nth-child(4) b"
govt_taxes_fees_selector <- ":nth-child(10) :nth-child(4) b , :nth-child(8) :nth-child(4) b, :nth-child(6) :nth-child(4) b, .accord-content-block:nth-child(5) b"

tmp_selector <- "#panel1 :nth-child(1)"
tmp_selector <- "div[id^='tab']"
tmp_selector <- "div[id*='651-592-5063']"

# Parse bill --------------------------------------------------------------

bill_path <- "html_bills/sep_2017.html"
html <- read_html(bill_path)

text_of_selected <- function(html, selector) {
  map_chr(html_nodes(html, selector), html_text)
}

select_after <- function(html, selector1, selector2) {
  first_node <- html_node(html, selector1)
  html_node(first_node)
}

text_of_selected(html, name_number_total_selector)
text_of_selected(html, monthly_plan_charges_selector)
text_of_selected(html, equipment_charges_selector)

divs <- html_nodes(html, "div")

main_container <- html_nodes(
  html, 
  "body > div.main-container"
)

main_children <- html_children(main_container)
main_div_children <- html_nodes(main_children, "div")

map_chr(main_div_children, html_attr, "id")

bs <- html_nodes(html, css = "b")

"#printfa > div.wireless-details > div > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > span > b"