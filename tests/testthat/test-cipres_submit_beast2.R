context("submit BEAST2")

test_that("generates list correctly", {
    tt <- tempfile()
    cat("test", file = tt)
    bdy <- .cipres_submit_beast2(input_file = tt,
                                 beast_version = "30",
                                 max_runtime = 10,
                                 use_beagle = TRUE,
                                 n_patterns = 1000,
                                 n_partitions = 3,
                                 use_seed = 12345,
                                 overwrite_logs = TRUE,
                                 job_name = "test job name",
                                 get_email = TRUE
                                 )
    bdy <- lapply(bdy, as.character)
    expect_true(inherits(bdy$input.infile_, "form_file"))
    expect_identical(bdy$vparam.is_partitioned_, "1")
    expect_identical(bdy$metadata.clientJobName, "test job name")
    expect_identical(bdy$metadata.statusEmail, "true")
    expect_identical(bdy$vparam.no_beagle_, "0")
    expect_identical(bdy$vparam.nu_partitions_, "3")
    expect_identical(bdy$vparam.nu_patterns_, "1000")
    expect_identical(bdy$vparam.overwrite_logs_, "1")
    expect_identical(bdy$vparam.runtime_, "10")
    expect_identical(bdy$vparam.seed_val_, "12345")
    expect_identical(bdy$vparam.spec_seed_, "1")
    expect_identical(bdy$vparam.which_beast2_, "30")
})
