

root_url <- "http://production.shippingapis.com/ShippingAPI.dll?API=Verify&XML="

available_tags <- c("FirmName", "Address1", "Address2", "City", "State", "Urbanization", "Zip4", "Zip5")

request_template <- list(
  AddressValidateRequest = list(
    Revision = list(1)
    , Address  = list()
  )
)

devtools::use_data(root_url, request_template, available_tags, internal = TRUE, overwrite = TRUE)

