#' Read SAS file and value labels according to PROC FORMAT
#'
#' Reads a SAS `sas7bdat` file using [haven::read_sas] and then applies the
#' labels given in the formats file (containing `PROC FORMAT` statements).
#' The labels use [labelled::labelled] and may be coerced to factors or ordered
#' factors if desired.
#'
#' @param datafile Path to the `sas7bdat` file to be read by [haven::read_sas]
#' @param formatfile Path to the SAS formats file to be read by [read_proc_format]
#' @param fmtfile Path to the SAS fmts file to be read by [read_proc_format_vars]
#' @param as_factor If TRUE, coerce labelled variables to factor with [haven::as_factor]
#' @param ordered If TRUE and as_factor is TRUE, coerce to ordered factor
#'
#' @examples
#' \dontrun{
#' read_ssocs("pu_ssocs18.sas7bdat", "pu_ssocs18_format.txt")
#' }
#'
#' @return A tibble, data frame variant with nice defaults.
#'
#'   Variable labels are stored in the "label" attribute of each variable.
#'   It is not printed on the console, but the RStudio viewer will show it.
#'
#'   The vectors may be a labelled vector that may be coerced into a
#'   factor or ordered factor.
#'
#' @family SAS helper functions
#' @export
read_ssocs <- function(datafile, formatfile, fmtfile = NULL,
                       as_factor = TRUE, ordered = TRUE){
  df <- haven::read_sas(datafile)

  if(is.null(fmtfile)) fmtfile <- formatfile
  fmts <- read_proc_format_vars(fmtfile)

  formatvalues <- read_proc_format(formatfile) %>%
    purrr::map_dfr(extract_statement) %>%
    dplyr::mutate(label = purrr::map(.data$value, labelize_values)) %>%
    dplyr::left_join(fmts) %>%
    dplyr::select(varname, formatname, label) %>%
    dplyr::mutate(varname = ifelse(is.na(varname), formatname, varname),
           varname = stringr::str_remove(varname, "F$"))

  pf <- purrr::set_names(formatvalues$label, formatvalues$varname) %>%
    purrr::keep(~length(.[!is.na(.)]) > 0)

  pf <- pf[!(names(pf) %in% setdiff(names(pf), names(df)))]
  df <- df %>%
    dplyr::mutate_at(names(pf), as.character)  %>%
    dplyr::mutate_if(is.character, stringr::str_trim)

  labelled::val_labels(df) <- pf

  if(!as_factor) return(df)
  df <- df %>% dplyr::mutate_if(haven::is.labelled, labelled::to_factor, ordered = ordered)
  return(df)
}
