
base_url <- "https://cipresrest.sdsc.edu/cipresrest/v1"

##' Easy programmatic access to the login information. This function
##' shouldn't be used by normal users and is only intended to be
##' called by other functions.
##'
##' You need to create the environment variables \code{CIPRES_USER},
##' \code{CIPRES_PWD}, \code{CIPRES_APP_ID} in your .Renviron file
##' after registering for an account at
##' \url{https://www.phylo.org/restusers/login.action}. Therefore, you
##' will need to enter your login information only once.
##'
##' @title Access login information for the CIPRES API
##' @param user Your CIPRES API user name
##' @param password Your CIPRES API password
##' @param app_id Your CIPRES API application token
##' @return A list
##' @author Francois Michonneau
##' @export
cipres_login <- function(user = Sys.getenv("CIPRES_USER"),
                         password = Sys.getenv("CIPRES_PWD"),
                         app_id = Sys.getenv("CIPRES_APP_ID")) {

    list(user = user, password = password, app_id = app_id)
}

cipres_check <- function(res) {
    if (res$status_code < 400)
        return(invisible())

    msg <- cipres_parse(res)$message
    stop("HTTP failure: ", res$status_code, "\n", msg, call. = FALSE)
}

cipres_parse <- function(res) {
    txt <- httr::content(res, as = "text")
    if (identical(txt, ""))
        stop("No output to parse", call. = FALSE)

}

##' The list of jobs currently running on CIPRES
##'
##' @title List jobs currently running on CIPRES
##' @return XML
##' @author Francois Michonneau
##' @export
##' @importFrom httr GET
##' @importFrom httr add_headers
##' @importFrom httr authenticate
##' @importFrom httr content
cipres_list_jobs <- function() {
    res <- httr::GET(url=paste(base_url, "job", cipres_login()$user, sep = "/"),
                      httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                      httr::authenticate(user = cipres_login()$user, password = cipres_login()$password)
                      )

    cipres_check(res)
    httr::content(res)
}

##' Submit a job to CIPRES using the API
##'
##' Possible tools are ...
##' @title Submit a job to CIPRES
##' @return XML
##' @author Francois Michonneau
##' @param tool The tool to use to analyze the data
##' @param input_file The input file to use with the selected tool
##' @export
##' @importFrom httr POST
##' @importFrom httr add_headers
##' @importFrom httr authenticate
##' @importFrom httr content
cipres_submit_job <- function(tool = "RAXMLHPC8_REST_XSEDE",
                              input_file = "./inst/raxml_inputphy.txt",
                              params) {
    ## need to check file exists
    ## need to check tool chosen is correct
    res <- httr::POST(url = paste(base_url, "job", cipres_login()$user, sep = "/"),
                      httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                      c(httr::verbose(), httr::authenticate(user = cipres_login()$user, password = cipres_login()$password)),
                      body = list(
                          `input.infile_` = paste0("@", input_file),
                          `tool` = tool,
                          `metadata.statusEmail` = "true"
                          ),
                      encode = "multipart")
    cipres_check(res)
    httr::content(res)
}

cipres_POST <- function(body, ...) {
    res <- httr::POST(url = paste(base_url, "job", cipres_login()$user, sep = "/"),
                      httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                      c(httr::verbose(), httr::authenticate(user = cipres_login()$user, password = cipres_login()$password)),
                      body = body,
                      encode = "multipart",
                      ...)
    cipres_check(res)
    httr::content(res)
}

cipres_GET <- function(path, ...) {
    res <- httr::GET(url = paste(base_url, "job", cipres_login()$user, path, sep = "/"),
                     httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                     c(httr::verbose(),
                       httr::authenticate(user = cipres_login()$user, password = cipres_login()$password))
                     )
    cipres_check(res)
    httr::content(res)
}


### https://www.phylo.org/restusers/docs/qs.html

## cipres_cancel_job <- function()

## cipres_job_status <- function()

## cipres_job_results <- function()

### https://www.phylo.org/restusers/docs/guide.html

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

    bdy <- list(
        `input.infile_` = I(paste0("@", input_file)),
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



cipres_list_files <- function(job_handle, ...) {
    cipres_GET(path = paste(job_handle, "output", sep = "/"))
}

cipres_download_file <- function(job_handle, file_id, outfile, ...) {
    res <- cipres_GET(path = paste(job_handle, "output", file_id, sep = "/"), ...)
    cat(res, file = outfile)
}
