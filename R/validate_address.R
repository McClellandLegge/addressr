#' Validate a parsed address according to USPS standards

#' @param userid The userid given to you by USPS, see the Details for more
#' @param address A data.frame or \code{\link[data.table]{data.table}} with the
#'     columns: `Address1`, `Address2`, `City`, `State`, `Zip5` and `Zip4`. You
#'     must supply either the `City` and `State` or the `Zip5` (with or without `Zip4`)
#'     for each address.
#'
#' @details If you do not have a `userid` you should register at \url{https://www.usps.com/business/web-tools-apis}
#'     before trying to use this function.
#'
#' @seealso \code{\link{prepareAddress}}
#' @export
#' @import data.table
validateAddress <- function(userid, address) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table`` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("`RCurl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("`xml2` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("`utils` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  atags <- addressr:::available_tags
  if (any(!atags %in% names(address))) {
    msg <- paste("Must have columns:", paste0(atags, collapse = ", "))
    stop(msg)
  }

  # instead of NAs we want blank strings so the html tags will be empty
  # FIXME: why doesn't this reliably work? address[is.na(address)] <- ""
  address[, (atags) := lapply(.SD, function(x) {
    x[is.na(x)] <- ""
    return(x)
  }), .SDcols = atags]

  # make an id column to keep track of the sites across the asynchronous requests
  if ("id" %in% names(address)) {
    if (any(duplicated(address[['id']]))) {
      stop("The 'id' column does not consist of unique elements!")
    }
  } else {
    writeLines("No 'id' column supplied, creating one...")
    address[, id := .I]
  }

  # compose the request by appending children
  req_url   <- composeRequest(userid, address)

  # prepend the root API url and encode
  root_req  <- paste0(addressr:::root_url, req_url)
  req       <- sapply(root_req, utils::URLencode, USE.NAMES = FALSE)

  # make the request, convert the elements to xml2 documents and combine to
  # an R list object
  resp      <- executeCall(req)

  # extract each address to a datatable and stack and handle errors
  resp_dt   <- extractResponseItems(resp)

  # re-arrange the columns for aesthetic purposes
  resp_cn   <- names(resp_dt)
  tag_cols  <- resp_cn[resp_cn %in% addressr:::available_tags]
  alt_cols  <- resp_cn[resp_cn %in% c("ReturnText", "Error")]
  data.table::setcolorder(resp_dt, c("id", tag_cols, alt_cols))

  return(resp_dt)
}
