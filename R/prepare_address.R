#' Prepare a raw address by cleaning and validating
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
# library("data.table")
# library("addressr")
# address_fl <- system.file("extdata", "wedding-addresses.csv", package = "addressr")
# addresses  <- fread(address_fl)
# prepareAddress("XXXXXX", addresses, address_column = 'Address', max_tries = 3L)
#' }
#' @export
prepareAddress <- function(userid, data, address_column = 'Address', max_tries = 10L) {

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

  if (!inherits(data_dt[[address_column]], 'character')) {
    stop("The 'address_column' must be character")
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

  # escape pound sign
  if (any(grepl("#", data_dt[[address_column]]))) {
    warning("Pound signs '#' in the addresses usually aren't recognized by the Google geocoder -- better to replace them with 'Apt' or 'Unit'", call. = FALSE)
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

    # assign the ID here so it perpetuates even if the order gets jumbled in
    # the retries
    try_res[, id := try_addrs[['id']]]

    # identify the bad requests
    bad_ids <- try_res[is.na(lon) | is.na(lat)][['id']]

    # save the successful geocodes
    good_addrs[[tries]] <- try_res[!id %in% bad_ids]

    # if any bad ids, use their original records to try again
    if (length(bad_ids) > 0L) {
      try_addrs           <- data_dt[id %in% bad_ids]
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
  geocoded_dt <- data.table::rbindlist(good_addrs, fill = TRUE)
  sel         <- names(which(sapply(geocoded_dt, is.factor), useNames = TRUE))
  geocoded_dt[, (sel) := lapply(.SD, as.character), .SDcols = sel]

  # collapse the street number and route to street address
  # check for any non-existant columns (such as subpremise, which gets mapped to Address1)
  geocoded_dt[, street := paste(street_number, route)]
  sel <- addressr:::geocode_cols %in% names(geocoded_dt)
  setnames(geocoded_dt, addressr:::geocode_cols[sel], addressr:::available_tags[sel])

  # initialize any of the missing columns
  need_init <- addressr:::available_tags[!sel]
  for(col in need_init) {
    data.table::set(geocoded_dt, i = NULL, j = col, value = NA)
  }

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

  return(final)
}
