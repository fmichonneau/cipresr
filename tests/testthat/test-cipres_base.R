
### check_file -----------------------------------------------------------------

context("check_file")

test_that("check_file throws error when file doesn't exist", {
    expect_error(check_file("test-test-test", "doesn't exist"))
})

test_that("check_file is silent when input is NULL",
          expect_silent(check_file(NULL)))

test_that("check_file is silent when input is file that exists", {
    tt <- tempfile()
    cat("test", file = tt)
    expect_silent(check_file(tt))
})


### add_meta_data --------------------------------------------------------------

context("add_meta_data")

test_that("add email to meta data is flag",
          expect_error(add_meta_data(bdy = list(),
                                     get_email = "true",
                                     job_name = NULL),
                       "not a flag")
          )

test_that("add email behaves correctly with TRUE/FALSE", {
    expect_identical(add_meta_data(bdy = list(), get_email = TRUE,
                                   job_name = NULL)$`metadata.statusEmail`,
                     "true")
    expect_identical(add_meta_data(bdy = list(), get_email = FALSE,
                                   job_name = NULL)$`metadata.statusEmail`,
                     "false")
})

test_that("job_name assertions behaves correctly", {
    expect_error(add_meta_data(bdy = list(), get_email = TRUE,
                               job_name = c("a", "b")))
})

test_that("job_name behaves correctly", {
    tt <- add_meta_data(bdy = list(),
                        get_email = TRUE,
                        job_name = "test job")
    expect_identical(tt$`metadata.clientJobName`, "test job")
})
