##' @param codon_partitioning Do the partitions follow the codon positions?
##' @rdname beast
##' @export
##' @importFrom httr upload_file
##' @importFrom assertthat assert_that is.count
cipres_submit_beast1 <- function(input_file,
                                 beast_version = c("1.8.2", "1.8.1", "1.8.0"),
                                 use_beagle = TRUE,
                                 max_runtime = 10,
                                 codon_partitioning = FALSE,
                                 use_seed = NULL,
                                 job_name = NULL,
                                 get_email = TRUE, ...) {

    ## Documentation here: http://www.phylo.org/rest/beast_tg.html

    beast_version <- match.arg(beast_version)
    beast_version <- switch(beast_version,
                          "1.8.0" = "0",
                          "1.8.1" = "1",
                          "1.8.2" = "2"
                          )

    input_file <- normalizePath(input_file)
    check_file(input_file)

    assertthat::assert_that(assertthat::is.count(max_runtime))

    assertthat::assert_that(assertthat::is.flag(use_beagle))
    assertthat::assert_that(assertthat::is.flag(codon_partitioning))

    bdy <- list(
        `input.infile_` = httr::upload_file(input_file),
        `vparam.which_beast_` = beast_version,
        `vparam.no_beagle_` = as.numeric(!use_beagle), ## they use the opposite logic hence the negation
        `vparam.runtime_` = max_runtime,
        `vparam.codon_partitioning_` = as.numeric(codon_partitioning)
    )

    alg_info <- parse_beast1_xml(input_file)
    n_patterns <- alg_info[["n_patterns"]]
    assertthat::assert_that(assertthat::is.count(n_patterns))

    bdy$`vparam.nu_patterns_` <- n_patterns
    n_partitions <- alg_info[["n_partitions"]]

    bdy <- beast_check_partitions(bdy, n_partitions, beast2 = FALSE)
    bdy <- beast_use_seed(bdy, use_seed)
    bdy <- add_meta_data(bdy, get_email, job_name)

    bdy <- lapply(bdy, as.character)
    bdy$tool <- "BEAST_TG"
    res <- cipres_POST(body = bdy, ...)
    cipres_process_results(res)
}

beast_check_partitions <- function(bdy, n_partitions, beast2) {
    assertthat::assert_that(assertthat::is.count(n_partitions))
    if (n_partitions > 1) {
        if (beast2) { bdy$`vparam.is_partitioned_` <- "1" }
        bdy$`vparam.nu_partitions_` <- n_partitions
    } else {
        bdy$`vparam.is_partitioned_` <- "0"
    }
    bdy
}

beast_use_seed <- function(bdy, use_seed) {
    if (!is.null(use_seed)) {
        assertthat::assert_that(assertthat::is.count(use_seed))
        bdy$`vparam.spec_seed_` <- "1"
        bdy$`vparam.seed_val_` <- use_seed
    } else {
        bdy$`vparam.spec_seed_` <- "0"
    }
    bdy
}

##' @importFrom xml2 read_xml xml_find_all xml_contents
parse_beast1_xml <- function(input_file) {
    bst <- xml2::read_xml(x = input_file)

    ## Get the number of partitions based on the number of elements
    ## with siteModel
    n_partitions <- length(xml2::xml_find_all(bst, ".//siteModel[@id]"))

    ## Get length of sequences by calculating length of the first
    ## sequence for each partitions
    seq_info <- xml2::xml_contents(
        xml2::xml_find_all(bst, ".//alignment/sequence[1]")
    )
    clean_seq <- gsub("\\s|(^<.+)", "", seq_info)
    clean_seq <- clean_seq[nzchar(clean_seq)]
    n_patterns <- sum(sapply(clean_seq, nchar))

    list(n_partitions = n_partitions,
         n_patterns = n_patterns)
}
