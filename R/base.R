
cipres_version <- "v1"
base_url <- paste("https://cipresrest.sdsc.edu/cipresrest", "v1", sep = "/")

## Variables for location of the appdir directory
APPNAME <- "cipresr"
APPAUTHOR <- "fmichonneau"

## check error codes
cipres_check <- function(res) {
    if (res$status_code < 400)
        return(invisible())

    msg <- cipres_parse(res)$message
    stop("HTTP failure: ", res$status_code, "\n", msg, call. = FALSE)
}


cipres_results <- function(res) {
    cipres_check(res)
    if (identical(res[["headers"]][["content-type"]], "application/xml")) {
        return(xml2::read_xml(httr::content(res, as = "text")))
    } else {
        return(httr::content(res, as = "text"))
    }
}

cipres_parse <- function(res) {
    txt <- httr::content(res, as = "text")
    if (identical(txt, ""))
        stop("No output to parse", call. = FALSE)

}

cipres_POST <- function(body, ...) {
    res <- httr::POST(url = paste(base_url, "job", cipres_login()$user, sep = "/"),
                      httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                      c(httr::authenticate(user = cipres_login()$user,
                                           password = cipres_login()$password)),
                      body = body,
                      encode = "multipart",
                      ...)
    cipres_results(res)
}

cipres_GET <- function(path, full_url, ...) {

    if (missing(full_url)) {
        url <- paste(base_url, "job", cipres_login()$user, path, sep = "/")
    } else if (missing(path)) {
        url <- full_url
    } else stop("Problem in URL construction")

    res <- httr::GET(url = url,
                     httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                     c(httr::authenticate(user = cipres_login()$user,
                                          password = cipres_login()$password)),
                     ...)
    cipres_results(res)
}

cipres_DELETE <- function(path, ...) {
    res <- httr::DELETE(url = paste(base_url, "job", cipres_login()$user, path, sep = "/"),
                        httr::add_headers(`cipres-appkey` = cipres_login()$app_id),
                        c(httr::authenticate(user = cipres_login()$user,
                                             password = cipres_login()$password))
                        )
    cipres_results(res)
}

### https://www.phylo.org/restusers/docs/qs.html


check_file <- function(file) {
    if (!is.null(file) && !file.exists(file))
        stop(sQuote(file), " doesn't exist.")
}

cipres_process_results <- function(res) {
    handle <- xml2::xml_text(xml2::xml_find_all(res, ".//jobHandle"))
    stage <- xml2::xml_text(xml2::xml_find_all(res, ".//jobStage"))
    failed <- xml2::xml_text(xml2::xml_find_all(res, ".//failed"))
    date_submitted <- xml2::xml_text(xml2::xml_find_all(res, ".//dateSubmitted"))
    list(handle = handle,
         stage = stage,
         failed = failed,
         date_submitted = date_submitted)
}

add_meta_data <- function(bdy, get_email) {
    if (get_email) {
        bdy$`metadata.statusEmail` <- "true"
    } else {
        bdy$`metadata.statusEmail` <- "false"
    }
    bdy
}
