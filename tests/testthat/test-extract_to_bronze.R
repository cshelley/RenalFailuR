test_that("data.frame is correctly assembled", {
  expect_equal(class(extract_to_bronze('Chronic Kidney Disease')), "data.frame")
  expect_equal(dim(extract_to_bronze("Chronic Kidney Disease")), c(400, 25))
})

