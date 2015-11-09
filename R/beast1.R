cipres_submit_beast1 <- function(input_file,
                                 beast_version = c("1.8.2", "1.8.1", "1.8.0"),
                                 use_beagle = TRUE,
                                 max_runtime = 10,
                                 is_partitioned = TRUE,
                                 codon_partitioning = TRUE,
                                 n_partitions = 3,
                                 use_seed = NULL,
                                 get_email = TRUE, ...) {

    ## Documentation here: http://www.phylo.org/rest/beast_tg.html

    ## Need better logic for partitioning to check that the user
    ## doesn't specify something impossible (e.g., data not
    ## partitioned but specify number of partitions).

    ## provide option to validate request before submitting

    ## provide option to give an ID to a job

    beast_version <- match.arg(beast_version)
    beast_version <- switch(beast_version,
                          "1.8.0" = "0",
                          "1.8.1" = "1",
                          "1.8.2" = "2"
                          )

    if (!file.exists(input_file))
        stop(sQuote(input_file), " doesn't exist.")
    else
        input_file <- normalizePath(input_file)

    if (!is.logical(use_beagle))
        stop(sQuote("use_beagle"), " should be a logical.")
    else
        no_beagle <- as.numeric(!use_beagle) ## they use the opposite logic hence the negation

    if (!is.numeric(max_runtime))
        stop(sQuote("max_runtime"), " should be a numeric value.")

    if (!is.logical(is_partitioned))
        stop(sQuote("is_partitioned"), " should be a logical.")

    if (!is.logical(codon_partitioning))
        stop(sQuote("codon_partitioning"), " should be a logical.")

    if (!is.numeric(n_partitions))
        stop(sQuote("n_partitions"), " should be a numeric.")

    infile <- httr::upload_file(input_file)

    bdy <- list(
        `input.infile_` = infile,
        `vparam.which_beast_` = beast_version,
        `vparam.no_beagle_` = no_beagle,
        `vparam.runtime_` = max_runtime,
        `vparam.is_partitioned_` = as.numeric(is_partitioned),
        `vparam.codon_partitioning_` = as.numeric(codon_partitioning),
        `vparam.nu_partitions_` = n_partitions
        )

    if (get_email) {
        bdy$`metadata.statusEmail` <- "true"
    } else {
        bdy$`metadata.statusEmail` <- "false"
    }

    if (!is.null(use_seed)) {
        if (!is.numeric(use_seed)) {
            stop("If you want to use a seed, you need to specify a numeric value for it.",
                 " (e.g., ", sQuote("use_seed=12345"), ")")
        } else {
            bdy <- c(bdy,
                     list(
                         `vparam.spec_seed_` = "1",
                         `vparam.seed_val_` = use_seed
                         )
                     )
        }
    } else {
        bdy <- c(bdy,
                 `vparam.spec_seed_` = "0")
    }

    bdy <- lapply(bdy, as.character)
    bdy <- c(tool = "BEAST_TG", bdy)
    cipres_POST(body = bdy, ...)
}
