#' Standardize Variable Names in a DataFrame
#'
#' This function uses `janitor::make_clean_names` to clean and standardize variable names
#' of a dataframe to the specified case format using `dplyr::rename_with`. By default,
#' the names are converted to 'small_camel' case.
#'
#' @param data A dataframe or tibble whose variable names need to be standardized.
#' @param case_type The type of case conversion to apply, default is "small_camel".
#'
#' @return A dataframe or tibble with standardized variable names.
#'
#' @examples
#' data = tibble::tibble(`First name` = c("Alice", "Bob"), `Last name` = c("Smith", "Jones"))
#' standardized_data = standardizeNames(data)
#'
#' @importFrom dplyr rename_with
#' @importFrom janitor make_clean_names
#' @export
standardizeNames = function(data, case_type = "small_camel") {
  if (!"tibble" %in% class(data) && !"data.frame" %in% class(data)) {
    stop("Input must be a dataframe or tibble.")
  }
  require(dplyr)
  require(janitor)
  case_fn = switch(case_type,
                    "small_camel" = snakecase::to_camel_case,
                    "snake" = snakecase::to_snake_case,
                    "screaming_snake" = snakecase::to_screaming_snake_case,
                    "kebab" = snakecase::to_kebab_case,
                    snakecase::to_camel_case) # default to small_camel if case_type is unrecognized
  data %>%
    dplyr::rename_with(~ janitor::make_clean_names(.) %>% case_fn(), .cols = everything())
}
