
#' @import data.table
composeRequest <- function(userid, address) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # we can only batch five addresses at a time
  n_reqs  <- ceiling(nrow(address) / 5L)

  # every five observations constitutes a new group
  address[['groups']] <- utils::head(rep(1L:n_reqs, each = 5L), nrow(address))

  # compose a request for each of the groups
  urls_dt <- address[, .(url = composeAddressRequest(userid, .SD)), by = groups]

  # return the column as a character vector
  return(urls_dt[["url"]])
}


#' @import data.table
composeAddressRequest <- function(userid, address) {

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
  user_req  <- composeUserRequest(userid)

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

