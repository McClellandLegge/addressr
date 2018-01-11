#' Clean a raw address
#'
#' @param userid The userid given to you by USPS, see the Details for more
#' @param data A data.frame or \code{\link[data.table]{data.table}} with at least
#'     a column containing raw addresses
#' @param address_column The name of the column containing the addresses
#' @param max_tries The maximum iterations to attempt when geocoding, see the Details for more
#'
#' @details The \code{\link[ggmap]{geocode}} function seems to throw spurious `OVER_QUERY_LIMIT`
#'     errors quite frequently so the `cleanAddress` function has some basic handling
#'     of these errors, allowing a certain number of retries for those addresses that did
#'     not get geocoded. When an address is successfully geocoded it will not be
#'     retried.
#'
#'     If you do not have a `userid` you should register at \url{https://www.usps.com/business/web-tools-apis}
#'     before trying to use this function.
#' @seealso \code{\link{validateAddress}}
#' @examples
#' \dontrun{
#' library("data.table")
#' library("addressr")
#' address_fl <- system.file("extdata", "wedding-addresses.csv", package = "addressr")
#' addresses  <- fread(address_fl)
#' cleanAddress("XXXXXX", addresses, address_column = 'Address', max_tries = 3L)
#' }
#' @export
cleanAddress <- function(userid, data, address_column = 'Address', max_tries = 10L) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table`` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("ggmap", quietly = TRUE)) {
    stop("`ggmap` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("methods", quietly = TRUE)) {
    stop("`methods` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  data_dt <- data.table::as.data.table(data)

  if (!address_column %in% names(data_dt)) {
    stop(paste("Couldn't find expected address column:", address_column))
  }

  # make an id column to keep track of the sites across the asynchronous requests
  if ("id" %in% names(data_dt)) {
    if (any(duplicated(data_dt[['id']]))) {
      stop("The 'id' column does not consist of unique elements!")
    }
  } else {
    writeLines("No 'id' column supplied, creating one...")
    data_dt[['id']] <- seq(nrow(data_dt))
  }

  # handling for possible spurious OVER_QUERY_LIMIT errors, retry
  writeLines("Geocoding address string...")
  tries      <- 1L
  good_addrs <- list()
  try_addrs  <- data_dt
  while (tries <= max_tries) {
    try_res <- suppressMessages({
      # FIXME: figure out why mutate_geocode wasn't working
      ggmap::geocode(
          location       = try_addrs[[address_column]]
        , output         = 'more'
        , messaging      = FALSE
        , force          = TRUE
        , override_limit = TRUE
      )
    })

    data.table::setDT(try_res)

    # identify the bad requests
    addrs <- try_res[['address']]

    if (is.null(addrs)) {
      next
    } else {
      ids <- try_addrs[['id']][is.na(addrs)]
    }

    good_addrs[[tries]] <- try_res[!is.na(address)]

    if (length(ids) > 0L) {
      try_addrs           <- data_dt[id %in% ids]
      tries               <- tries + 1L
    } else {
      break
    }
  }

  if (tries > 10L & nrow(try_addrs) > 0L) {
    warning("Tried 10 times, couldn't geocode all sites. Returning offenders")
    ggmap::geocodeQueryCheck(userType = "free")
    return(try_addrs)
  }

  # convert to a data.table and convert the factored columns to character
  geocoded_dt <- data.table::rbindlist(good_addrs)
  sel         <- names(which(sapply(geocoded_dt, is.factor), useNames = TRUE))
  geocoded_dt[, (sel) := lapply(.SD, as.character), .SDcols = sel]

  # collapse the street number and route to street address
  geocoded_dt[, street := paste(street_number, route)]
  setnames(geocoded_dt, addressr:::geocode_cols, addressr:::available_tags)

  # create an index
  geocoded_dt[['id']] <- data_dt[['id']]

  # validate the address with USPS api
  writeLines("Validating with USPS...")
  validated_dt <- addressr::validateAddress(userid, geocoded_dt)

  # merge the geocoding back in:
  orig_id_class <- class(data_dt[['id']])
  if (orig_id_class != "character") {
    validated_dt[['id']] <- methods::as(validated_dt[['id']], orig_id_class)
  }

  # change the names back to the original ones before re-merging to prevent
  valid_geo_dt <- merge(validated_dt, geocoded_dt, by = "id", all = TRUE, suffixes = c("", "_geo"))

  # merge in the original data
  final <- merge(valid_geo_dt, data_dt, by = "id", all = TRUE, suffixes = c("_orig", ""))

  return(valid_geo_dt)
}

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
#' @seealso \code{\link{cleanAddress}}
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
  if (!any(atags %in% names(address))) {
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
  req_url   <- addressr::composeRequest(userid, address)

  # prepend the root API url and encode
  root_req  <- paste0(addressr:::root_url, req_url)
  req       <- sapply(root_req, utils::URLencode, USE.NAMES = FALSE)

  # request and retrieve the URI, read the xml and convert to list
  resp_list <- RCurl::getURIAsynchronous(req)
  resp_xml  <- lapply(resp_list, xml2::read_xml)
  resp      <- do.call(c, lapply(resp_xml, xml2::as_list))

  # extract each address to a datatable and stack
  resp_dt   <- addressr::extractResponseItems(resp)

  # re-arrange the columns for aesthetic purposes
  resp_cn   <- names(resp_dt)
  tag_cols  <- resp_cn[resp_cn %in% addressr:::available_tags]
  alt_cols  <- resp_cn[resp_cn %in% c("ReturnText", "Error")]
  data.table::setcolorder(resp_dt, c("id", tag_cols, alt_cols))

  return(resp_dt)
}

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

#' @export
addChildren <- function(node, avps) {

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("`xml2` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # for each tag and value, add a child node to the one passed
  # FIXME: add option to exclude writing blank tags
  for (name in names(avps)) {
    xml2::xml_add_child(node, name, avps[[name]])
  }
  return(node)
}

#' @export
composeUserRequest <- function(userid) {

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("`xml2` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # compose the basic request XML nodes
  xml_doc <- xml2::as_xml_document(addressr:::request_template)
  xml2::xml_attr(xml_doc, "USERID") <- userid
  return(xml_doc)
}


#' @export
#' @import data.table
composeAddressRequest <- function(address) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("`xml2` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # identify the tag columns that are present
  addr_cn  <- names(address)
  tag_cols <- addr_cn[addr_cn %in% addressr:::available_tags]

  # the api needs a specific order
  tag_cols <- tag_cols[order(match(tag_cols, addressr:::available_tags))]

  # create a template with an 'Address' node to append individual requests
  user_req  <- addressr::composeUserRequest(userid)

  for (cid in address[['id']]) {
    # add a child node for this individual address
    address_node <- xml2::xml_add_child(user_req, "Address", ID = cid)

    # tag columns
    add_tags <- as.list(address[id == cid, tag_cols, with = FALSE])

    # append all the tags
    addressr::addChildren(address_node, add_tags)
  } #/ end 'for'

  char_req <- as.character(user_req)
  return(char_req)
}


#' @export
#' @import data.table
composeRequest <- function(userid, address) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # we can only batch five addresses at a time
  n_reqs  <- ceiling(nrow(address) / 5)

  # every five observations constitutes a new group
  address[['groups']] <- head(rep(1L:n_reqs, each = 5L), nrow(address))

  # compose a request for each of the groups
  urls_dt <- address[, .(url = addressr::composeAddressRequest(.SD)), by = groups]

  # return the column as a character vector
  return(urls_dt[["url"]])
}
