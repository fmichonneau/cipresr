test_that("is_maxruntime works", {
    expect_true(is_maxruntime(168))
    expect_false(is_maxruntime(168.1))
})
