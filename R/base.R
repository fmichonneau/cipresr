
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
                              input_file = "./inst/raxml_inputphy.txt") {
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


### https://www.phylo.org/restusers/docs/qs.html

## cipres_cancel_job <- function()

## cipres_job_status <- function()

## cipres_job_results <- function()

### https://www.phylo.org/restusers/docs/guide.html
