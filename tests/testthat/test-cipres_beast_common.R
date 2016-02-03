### beast_check_partitions -----------------------------------------------------

context("beast_check_partitions")

test_that("beast_check_partitions throws error", {
    expect_error(beast_check_partitions(list(), "test"), "not a count")
    expect_error(beast_check_partitions(list(), c(1, 4)), "not a count")
})

test_that("beast check_partitions with 1 partition",
          expect_identical(beast_check_partitions(list(), 1)$`vparam.is_partitioned_`,
          "0")
)

test_that("beast check partitions with 2 partitions", {
    part <- beast_check_partitions(list(), 2, beast2 = TRUE)
    expect_identical(part$`vparam.is_partitioned_`, "1")
    expect_identical(part$`vparam.nu_partitions_`, 2)
})

### beast_use_seed -------------------------------------------------------------

context("beast_use_seed")

test_that("beast_use seed is NULL", {
    expect_identical(beast_use_seed(list(), NULL)$`vparam.spec_seed_`, "0")
})

test_that("beast_use_seed is specified", {
    expect_error(beast_use_seed(list(), "fdlq"), "is not a count")
})

test_that("beast_use_seed with seed", {
    seed <- beast_use_seed(list(), 12345)
    expect_identical(seed$`vparam.spec_seed_`, "1")
    expect_identical(seed$`vparam.seed_val_`, 12345)
})
