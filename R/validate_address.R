
#' Validate an Address
#'
#' @return
#' @export
validateAddress <- function(userid, address) {
  UseMethod("validateAddress")
}

#' @export
validateAddress.character <- function(userid, address) {

  tags        <- names(address)
  zips        <- c("Zip4", "Zip5")
  unused_tags <- tags[!tags %in% addressr:::available_tags]

  if (length(unused_tags) > 0L) {
    tag_msg <- paste0(unused_tags, collapse = ", ")
    msg     <- paste("Unexpected tags, will not be used in request:", tag_msg)
    warning(msg)

    tags <- tags[!tags %in% unused_tags]
  }

  if (all(zips %in% tags)) {
    stop("Cannot specify both 'Zip4' and 'Zip5'")
  }

  valid_zip <- all(c("City", "State") %in% tags) | any(zips %in% tags)
  if (!valid_zip) {
    stop("Must specify either 'City' and 'State' or either 'Zip4', 'Zip5'")
  }

  raw_req_xml <- addressr::composeRequest(userid, address)

  # cleanup
  req_xml <- gsub("^<.*?>", "", gsub(" *\n *", "", as.character(raw_req_xml)))

  req <- paste0(addressr:::root_url, req_xml)

  # resp <- httr::GET(req)

  # xml2::read_html(rawToChar(httr::content(x)))
  return(list(xml = raw_req_xml, req = req))
}

#' @export
addChildren <- function(node, avps) {
  for (name in names(avps)) {
    xml2::xml_add_child(node, name, avps[name])
  }
  return(node)
}

#' @export
composeUserRequest <- function(userid) {
  xml_doc  <- xml2::as_xml_document(addressr:::request_template)
  address  <- xml2::xml_find_first(xml_doc, "//Address")

  xml2::xml_attr(address, "ID")     <- "0"
  xml2::xml_attr(xml_doc, "USERID") <- userid
  return(xml_doc)
}

#' @export
composeRequest <- function(userid, address) {
  user_req <- addressr::composeUserRequest(userid)
  add_node <- xml2::xml_find_first(user_req, "//Address")
  add_node <- addChildren(add_node, address)

  return(user_req)
}
