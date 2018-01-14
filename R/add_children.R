#' Add child nodes to the passed parent node
#'
#' @param node An \code{xml2} node
#' @param avps A named list
#'
#' @return An \code{xml2} node with the children appended
#'
#' @export
#' @examples
#' library("xml2")
#' parent    <- read_xml("<body/>")
#' add_nodes <- list(foo = "bar", fizz = "buzz")
#' parent    <- addressr::addChildren(parent, add_nodes)
addChildren <- function(node, avps) {

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("`xml2` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # try to coerce to a list, if not sucessful abort
  avps <- tryCatch(as.list(avps), error = function(e) NULL, warning = function(w) NULL)

  if (is.null(avps) || length(avps) == 0L) {
    stop("'avps' argument must provide a named list, or object that is able to be coerced to a named list!", call. = FALSE)
  }

  if (length(Filter(function(x) x != "", names(avps))) != length(avps)) {
    stop("All node/value pairs must be named!", call. = FALSE)
  }

  if (!all(sapply(avps, function(el) is.atomic(el) & length(el) == 1))) {
    stop("Cannot handle vector values for the child tags -- must be scalars!")
  }

  # for each tag and value, add a child node to the one passed
  # FIXME: add option to exclude writing blank tags
  for (name in names(avps)) {
    xml2::xml_add_child(node, name, avps[[name]])
  }

  return(node)
}
