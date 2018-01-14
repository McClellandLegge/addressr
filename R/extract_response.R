#' @export
extractResponseItems <- function(response) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # convert each item to a data table retaining the ID attribute
  dt_list <- lapply(response, function(resp_item) {
    # extract the id attribute for rejoining with the indexed set
    id <- attr(resp_item, "ID")

    # check for the occurrence of the error node
    errors <- resp_item[["Error"]]
    if (is.null(errors)) {

      # unlist the lower levels of the fields so it is a named list of charater
      # elements, then convert to data.table
      tags    <- unlist(resp_item[addressr:::available_tags], recursive = FALSE)
      item_dt <- data.table::as.data.table(tags)

      # add the id
      data.table::set(item_dt, i = NULL, j = "id", value = id)

      # check for any additional return text, append
      return_text <- resp_item[['ReturnText']]
      if (!is.null(return_text)) {
        return_msg <- paste0(return_text, collapse = "; ")
        data.table::set(item_dt, i = NULL, j = "ReturnText", return_msg)
      }

      return(item_dt)
    } else {
      # handle multiple errors
      error_dsc <- with(errors, paste(Source, Description, sep = ": "))
      error_msg <- paste0(error_dsc, collapse = "; ")
      error_dt  <- data.table::data.table(Error = error_msg, id = id)

      return(error_dt)
    }


  }) #/ end dt_list lapply

  # stack and fill all possibly null fields
  resp_dt <- data.table::rbindlist(dt_list, fill = TRUE)

  return(resp_dt)
}
