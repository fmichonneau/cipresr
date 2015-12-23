
cipres_version <- "v1"
base_url <- paste("https://cipresrest.sdsc.edu/cipresrest", "v1", sep = "/")
APPNAME <- "cipresr"
APPAUTHOR <- "fmichonneau"

##' Easy programmatic access to the login information. This function
##' shouldn't be used by normal users and is only intended to be

cipres_check <- function(res) {
    if (res$status_code < 400)
        return(invisible())

    msg <- cipres_parse(res)$message
    stop("HTTP failure: ", res$status_code, "\n", msg, call. = FALSE)
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

cipres_DELETE <- function(path, ...) {
    res <- httr::DELETE(url = paste(base_url, "job", cipres_login()$user, path, sep = "/"),
                        httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                        c(#httr::verbose(),
                            httr::authenticate(user = cipres_login()$user, password = cipres_login()$password))
                        )
    cipres_check(res)
    cipres_results(res)
}

### https://www.phylo.org/restusers/docs/qs.html


check_file <- function(file) {
    if (!is.null(file) && !file.exists(file))
        stop(sQuote(file), " doesn't exist.")
}
