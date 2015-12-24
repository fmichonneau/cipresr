context("cipres_submit_beast1 validate input format")

test_that("use_beagle is logical", {
              infile <- tempfile()
              cat("test", file = infile)
              expect_error(cipres_submit_beast1(input_file = infile,
                                                n_patterns = 1000,
                                                use_beagle = 1),
                           "logical")
              unlink(infile)
          })
