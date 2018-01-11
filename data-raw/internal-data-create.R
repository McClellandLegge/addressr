

root_url <- "http://production.shippingapis.com/ShippingAPI.dll?API=Verify&XML="

available_tags <- c("Address1", "Address2", "City", "State", "Zip5", "Zip4")

geocode_cols <- c("subpremise", "street", "locality",
                  "administrative_area_level_1", "postal_code",
                  "postal_code_suffix")

request_template <- list(
  AddressValidateRequest = list(
  )
)

devtools::use_data(
    root_url
  , geocode_cols
  , request_template
  , available_tags
  , internal = TRUE
  , overwrite = TRUE)


devtools::use_data(internal = FALSE, overwrite = TRUE)
