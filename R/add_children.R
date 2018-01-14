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
