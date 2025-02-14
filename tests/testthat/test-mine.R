test_that("exam events are returned with proper colnames", {
  skip_if_offline()

  exams <- get_exam_events()

  expect_s3_class(exams, "data.frame")
  expect_identical(names(exams), c("project", "year"))
})
