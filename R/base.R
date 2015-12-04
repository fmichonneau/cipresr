
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
##' @param params
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

cipres_results <- function(res) {
    if (identical(res[["headers"]][["content-type"]], "application/xml")) {
        return(xml2::read_xml(httr::content(res, as = "text")))
    } else {
        return(httr::content(res, as = "text"))
    }
}

cipres_POST <- function(body, ...) {
    res <- httr::POST(url = paste(base_url, "job", cipres_login()$user, sep = "/"),
                      httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                      c(#httr::verbose(),
                        httr::authenticate(user = cipres_login()$user, password = cipres_login()$password)),
                      body = body,
                      encode = "multipart",
                      ...)
    cipres_check(res)
    cipres_results(res)
}

cipres_GET <- function(path, ...) {
    res <- httr::GET(url = paste(base_url, "job", cipres_login()$user, path, sep = "/"),
                     httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                     c(#httr::verbose(),
                       httr::authenticate(user = cipres_login()$user, password = cipres_login()$password))
                     )
    cipres_check(res)
    cipres_results(res)
}


### https://www.phylo.org/restusers/docs/qs.html

## cipres_cancel_job <- function()

## cipres_job_status <- function()

## cipres_job_results <- function()

### https://www.phylo.org/restusers/docs/guide.html

## apparently the latest jobs submitted are listed last
cipres_list_jobs <- function() {
    res <- cipres_GET(path = "")
    titles <- xml_text(xml_find_all(res, ".//jobs/jobstatus/selfUri/title"))
    urls   <- xml_text(xml_find_all(res, ".//jobs/jobstatus/selfUri/url"))
    data.frame(`title` = titles, `url` = urls, stringsAsFactors = FALSE)
}


##' @param which_job job number: 0 last submitted job, -1 before last
##'     submitted job, 1 first ever submitted job
cipres_job_status <- function(list_jobs = cipres_list_jobs(),
                              which_job = 0, job_by_order = TRUE) {
    if (job_by_order) {
        if (which_job <=  0)
            which_job <- nrow(list_jobs) + which_job
    }
    res <- httr::GET(url = list_jobs[["url"]][which_job],
                     httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                     c(httr::authenticate(user = cipres_login()$user, password = cipres_login()$password))
                     )
    cipres_check(res)
    res <- cipres_results(res)
    handle <- xml_text(xml_find_all(res, ".//jobHandle"))
    stage <- xml_text(xml_find_all(res, ".//jobStage"))
    failed <- xml_text(xml_find_all(res, ".//failed"))
    date_submitted <- xml_text(xml_find_all(res, ".//dateSubmitted"))
    list(handle = handle,
         stage = stage,
         failed = failed,
         date_submitted = date_submitted)
}



check_file <- function(file) {
    if (!is.null(file) && !file.exists(file))
        stop(sQuote(file), " doesn't exist.")
}



cipres_list_files <- function(job_handle, ...) {
    res <- cipres_GET(path = paste(job_handle, "output", sep = "/"))

    what <- c("outputDocumentId", "filename", "length")
    res_lst <- lapply(what, function(x) {
                          xml2::xml_text(xml2::xml_find_all(res, paste0(".//", x)))
                      })

    res <- data.frame(res_lst, stringsAsFactors = FALSE)
    names(res) <- what
    res
}

cipres_download_file <- function(job_handle, file_id, outfile, ...) {
    res <- cipres_GET(path = paste(job_handle, "output", file_id, sep = "/"), ...)
    cat(res, file = outfile)
}

cipres_download_all <- function(job_handle, outdir, ...) {
    lst_files <- cipres_list_files(job_handle = job_handle, ...)
    apply(lst_files, 1, function(x) {
              cipres_download_file(job_handle = job_handle,
                                   file_id = x["outputDocumentId"],
                                   outfile = file.path(outdir, x["filename"]),
                                   ...)
          })
}

