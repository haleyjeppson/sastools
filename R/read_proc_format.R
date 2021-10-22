#' Read Proc Format File
#'
#' @param file Path to the SAS formats file
#'
#' @examples
#' \dontrun{
#' read_proc_format("pu_ssocs18_format.txt")
#' }
#'
#' @family SAS helper functions
#' @export
read_proc_format <- function(file){
  pf_raw <- readLines(file)
  pf_raw <- paste(pf_raw, collapse = "\n")
  pf_raw <- stringr::str_remove_all(pf_raw, "<92>")

  # want everything after 'proc format;' and before 'data OUT' or 'run;'
  if(stringr::str_detect(pf_raw, "data OUT")){
    pf_statements <- rematch2::re_match(pf_raw, "(proc format|PROC FORMAT).*;\\s*(?<pf>[\\S\\s]+?)\\s*(data OUT)")$pf
  } else pf_statements <- rematch2::re_match(pf_raw, "(proc format|PROC FORMAT).*;\\s*(?<pf>[\\S\\s]+?)\\s*(run|RUN)\\s*;")$pf

  strsplit(pf_statements, ";")[[1]] %>%
    stringr::str_trim("left")
}


#' Read Proc Fmt File
#'
#' @param file Path to the SAS fmts file
#'
#' @examples
#' \dontrun{
#' read_proc_format_vars("pu_ssocs18_format.txt")
#' }
#'
#' @family SAS helper functions
#' @export
read_proc_format_vars <- function(file){
  pf_raw <- readLines(file)
  pf_raw <- paste(pf_raw, collapse = "\n")
  pf_raw <- stringr::str_remove_all(pf_raw, "<92>")

  # want everything after 'format' and before 'run;'
  regex_format_inner <- "\n(format)\n\\s*(?<pf>[\\S\\s]+?)\\s*\\.\\s*\\n*;\\s*(run|RUN)\\s*;"
  if(stringr::str_detect(pf_raw, "format")) pf_statements <- rematch2::re_match(pf_raw, regex_format_inner)$pf
  else pf_statements <- pf_raw

  stringr::str_split(pf_statements, "\\.\\s*\\n")[[1]] %>%
    stringr::str_trim() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(value = toupper(value)) %>%
    tidyr::separate(value, into = c("varname", "formatname"), sep = "\\s+\\$?")
}



extract_statement <- function(pf_statement){
  rx_formatname <- "^\\s*(VALUE|value)\\s*\\$?\\s*(?<formatname>[a-zA-Z_][a-zA-Z0-9_]{0,31})"
  rx_value <- "(^|\n)(?<value>(value|VALUE)[\\S\\s]+)"
  pf_formatname <- rematch2::re_match(pf_statement, rx_formatname)[, "formatname"]
  pf_value <- rematch2::re_match(pf_statement, rx_value)[, "value"]
  dplyr::bind_cols(pf_formatname, pf_value) %>%
    dplyr::mutate(formatname = toupper(formatname))
}

#' @importFrom stats setNames
labelize_values <- function(pf_value){
  rx_value_var <- "^\\s*(VALUE|value)\\s*\\$?\\s*[a-zA-Z_][a-zA-Z0-9_]{0,31}"

  pfv <- stringr::str_remove(pf_value, rx_value_var)  %>%
    stringr::str_remove_all('(\")') %>%
    stringr::str_remove_all("(\')") %>%
    strsplit(split = "\n") %>%
    .[[1]] %>%
    purrr::map_chr(stringr::str_trim) %>%
    purrr::map(~strsplit(., "=")[[1]]) %>%
    purrr::keep(~length(.) > 0) %>%
    purrr::transpose() %>%
    purrr::imap(~stringr::str_trim(.x))

  setNames(pfv[[1]], pfv[[2]])
}
