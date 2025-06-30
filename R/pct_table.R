#' Demographic Table
#'
#'This function produces a formatted demographic table .
#'
#' @param df a formatted dataframe
#' @param df1 a bigN2 dataframe contains
#' @param label a label for naming above the targeted line
#'
#' @return A table of demographic table
#' @export
#'
#' @importFrom tidyverse
#' @importFrom dplyr mutate group_by summarise arrange select distinct ungroup relocate row_number case_when
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom sassy create_table column_defaults spanning_header stub define create_report set_margins options_fixed titles page_header add_content page_footer footnotes write_report

dm_table <- function(df, df1, label = "Derived AJCC") {
  library(tidyr)
  library(dplyr)
  library(sassy)
  library(forcats)
  library(tidyverse)
  exclude_cols <- c("Variable","Category","order1","order2","order3","PG","ptcnt")
  target_vars  <- setdiff(names(df), exclude_cols)
  start <- target_vars[1]
  end <- target_vars[length(target_vars) - 1]
  total <- target_vars[length(target_vars)]

  for (i in seq_along(target_vars)) {
    if (i <= length(target_vars) - 1) {
      var_i_name <- paste0("var_", i)
      var_i_value <- target_vars[i]
      assign(var_i_name, var_i_value)
    } else {
      total <- target_vars[i]
    }
  }

  tbl <- create_table(df, first_row_blank = TRUE, borders = c("bottom", "top"), width = 20) %>%
    column_defaults(from = start, to = total, width = 1.2, standard_eval = T) %>%
    spanning_header(from = start, to = end, label = label, standard_eval = T) %>%
    stub(vars = v(Variable, Category), label = "Variable \n Category", width = 3) %>%
    define(Variable, blank_after = TRUE) %>%
    define(Category, indent = 0.25) %>%
    define(order1, visible = FALSE) %>%
    define(order2, visible = FALSE) %>%
    define(order3, visible = FALSE) %>%
    define(PG, visible = FALSE) %>%
    define(ptcnt, visible = FALSE)

  for (i in seq_along(target_vars)) {
    tbl <- tbl %>%
      define(target_vars[i],
             align = "center",
             label = paste0(target_vars[i], "\n", "(N=", df1$N[i], ")"),
             standard_eval = T)
  }

  create_report(tbl, font = "Courier", font_size = 9) %>%
    set_margins(top = 1.0, left = 1, right = 1, bottom = .5) %>%
    options_fixed(line_count = 35) %>%
    titles("Table 3.2.1.1", "Analysis of Demographic Characteristics", "Safety Population") %>%
    page_header(
      left = c("PROTOCOL: DIDA 0000-123", "DRUG/INDICATION: DIDA00001/COMPOUND-ASSOCIATED STUDY",
               "TLF Version: Final Database LOCK (10JUNE2025)"),
      right = c("Page [pg] of [tpg]", "DATABASE VERSION: 10JUNE2025", "TASK: Primary Analysis")
    ) %>%
    add_content(tbl) %>%
    page_footer("Program: Demographic Characteristics.R", right = Sys.time(), center = "Confidential") %>%
    footnotes("Note: ..........")
}
