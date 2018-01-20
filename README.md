# `addressr`: Address Validation

[![Build Status](https://travis-ci.org/McClellandLegge/addressr.svg?branch=master)](https://travis-ci.org/McClellandLegge/addressr)

## Motivation

I'm getting married this year and apparently my family and friends can't be trusted
to give us valid mailing addresses for save-the-dates. As a result we needed to 
go through and check them by hand.

Obviously it took way longer than it would have by hand but it was a fun 
introduction to the [USPS web API](https://www.usps.com/business/web-tools-apis) suite.
A key benefit over [SmartyStreets](https://smartystreets.com/) and Experian tools
is that USPS doesn't throttle or limit your queries!

Once you have the validated addresses you can use the Microsot Word's mail-merge
wizard to import the csv to labels.

If you'd like to contribute, please see [this](./CONTRIBUTING.md) page.

## Pre-Requisites

In order to make calls to the USPS address verification API you'll need a user id.
Luckily they have automated email responses for basic access, just fill out the form
[here](https://registration.shippingapis.com/) and they'll email you an id within minutes.

## Installation 

```r
devtools::install_github("McClellandLegge/addressr")
```

## Usage

```r
library("data.table")
library("addressr")
address_fl <- system.file("extdata", "wedding-addresses.csv", package = "addressr")
addresses  <- fread(address_fl)
prepareAddress("XXXXXX", addresses, address_column = 'Address', max_tries = 3L)
```

## Acknowledgments

Hans Thompson's [repo](https://github.com/hansthompson/rusps)
tipped me off to the existance of the USPS api.

My wonderful fiancee for not killing me for starting a programming project when 
all she wanted was address labels made.
