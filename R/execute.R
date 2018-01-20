executeCall <- function(req) {
  resp_list <- RCurl::getURIAsynchronous(req)
  resp_xml  <- lapply(resp_list, xml2::read_xml)
  resp      <- do.call(c, lapply(resp_xml, xml2::as_list))
  return(resp)
}
