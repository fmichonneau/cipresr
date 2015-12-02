
##' @importFrom assertthat assert_that
##' @importFrom assertthat is.count
##' @importFrom assertthat is.flag
##' @importFrom httr upload_file
cipres_submit_beast2 <- function(input_file,
                                 beast_version = c("2.3.0", "2.1.3"),
                                 max_runtime = 10,
                                 use_beagle = TRUE,
                                 n_patterns,
                                 n_partitions = 0,
                                 use_seed = NULL,
                                 overwrite_logs = TRUE,
                                 get_email = TRUE, ...) {


    ## documentation: http://www.phylo.org/rest/beast2_xsede.html

    beast_version <- match.arg(beast_version)
    beast_version <- switch(beast_version,
                            "2.3.0" = "30",
                            "2.1.3" = "13")

    assertthat::assert_that(assertthat::is.count(max_runtime))
    assertthat::assert_that(assertthat::is.count(n_patterns))
    assertthat::assert_that(assertthat::is.count(n_partitions))
    assertthat::assert_that(assertthat::is.flag(use_beagle))
    assertthat::assert_that(assertthat::is.flag(overwrite_logs))

    input_file <- normalizePath(input_file)
    check_file(input_file)

    bdy <- list(
        `input.infile_` = httr::upload_file(input_file),
        `vparam.runtime_` = max_runtime,
        `vparam.nu_patterns_` = n_patterns,
        `vparam.which_beast2_` = beast_version,
        `vparam.no_beagle_` = as.numeric(!use_beagle),
        `vparam.overwrite_logs_` = as.numeric(overwrite_logs)
        )

    if (n_partitions > 0) {
        bdy$`vparam.is_partitioned_` <- "1"
        bdy$`vparam.nu_partitions_` <- n_partitions
    } else {
        bdy$`vparam.is_partitioned_` <- "0"
    }

    if (!is.null(use_seed)) {
        assertthat::assert_that(is.count(use_seed))
        bdy$`vparam.spec_seed_` <- "1"
        bdy$`vparam.seed_val_` <- use_seed
    } else {
        bdy$`vparam.spec_seed_` <- "0"
    }

    if (get_email) {
        bdy$`metadata.statusEmail` <- "true"
    } else {
        bdy$`metadata.statusEmail` <- "false"
    }

    bdy <- lapply(bdy, as.character)
    bdy$"tool" <- "BEAST2_XSEDE"

    cipres_POST(body = bdy, ...)
}
