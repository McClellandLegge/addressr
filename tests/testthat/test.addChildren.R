library("addressr")
library("xml2")
context("addChildren")

test_that("appends valid child nodes to parent", {
  expect_equal(
    addChildren(read_xml("<body/>"), list(foo = "bar", fizz = "buzz"))
    , read_xml("<body><foo>bar</foo><fizz>buzz</fizz></body>")
  )
})

test_that("empty tags are applied without error", {
  expect_equal(addChildren(read_xml("<body/>"), list("foo" = "")), read_xml("<body><foo/></body>"))
})

test_that("errors on malformed list", {
  expect_error(addChildren(read_xml("<body/>"), list(1, this = "that")))
})

test_that("errors for unsuccessful coercion to list", {
  expect_error(addChildren(read_xml("<body/>"), list()))
  expect_error(addChildren(read_xml("<body/>"), data.frame()))
  expect_error(addChildren(read_xml("<body/>"), list(one = 1:3)))
})
