##' @title List past and current jobs on the CIPRES server
##' @return A data frame with the job handle and its URL
##' @family job controls
##' @export
##' @importFrom xml2 xml_text xml_find_all
##' @template dotdotdot
cipres_list_jobs <- function(...) {
    res <- cipres_GET(path = "", ...)
    titles <- xml2::xml_text(xml2::xml_find_all(res, ".//jobs/jobstatus/selfUri/title"))
    urls   <- xml2::xml_text(xml2::xml_find_all(res, ".//jobs/jobstatus/selfUri/url"))
    data.frame(`handle` = titles, `url` = urls, stringsAsFactors = FALSE)
}



##' @title Job status
##' @param handle The job identifier
##' @template dotdotdot
##' @return A list containing the job handle, the job status
##'     (\code{stage}), whether the job failed, and the job data
##'     submission.
##' @author Francois Michonneau
##' @export
##' @importFrom xml2 xml_text xml_find_all
##' @importFrom assertthat assert_that is.string
##' @family job controls
##' @examples
##' \dontrun{
##'   ## Status of the latest job submitted
##'   lst_jobs <- cipres_list_jobs()
##'   cipres_job_status(lst_jobs$handle[nrow(lst_jobs)])
##' }
cipres_job_status <- function(handle, ...) {

    assertthat::assert_that(assertthat::is.string(handle))

    lst_jobs <- cipres_list_jobs(...)
    i_job <- match(handle, lst_jobs[["handle"]])

    if (is.na(i_job)) {
        stop("Job handle not found.")
    }

    res <- cipres_GET(full_url = lst_jobs[["url"]][i_job], ...)

    handle <- xml2::xml_text(xml2::xml_find_all(res, ".//jobHandle"))
    stage <- xml2::xml_text(xml2::xml_find_all(res, ".//jobStage"))
    failed <- xml2::xml_text(xml2::xml_find_all(res, ".//failed"))
    date_submitted <- xml2::xml_text(xml2::xml_find_all(res, ".//dateSubmitted"))
    list(handle = handle,
         stage = stage,
         failed = failed,
         date_submitted = date_submitted)
}


##' Deletes data and output associated with a job on CIPRES's server
##' (or cancel a job if it's still running.)
##'
##' Given a job handle, deletes data and output associated with the
##' job. If the job hasn't completed, it will abort it and deletes the
##' data and output.
##'
##' @title Delete or cancel jobs on CIPRES server
##' @param handle The job identifier
##' @param verbose Should a message be printed if the
##'     deletion/cancellation is successful?
##' @template dotdotdot
##' @return \code{TRUE} if the deletion/cancellation was succesfull,
##'     \code{FALSE} otherise.
##' @family job controls
##' @export
##' @examples
##' \dontrun{
##' ### Deletes the 5 oldest jobs. Be careful, there is no undo!
##' ### sapply(cipres_list_jobs()$title[1:5], cipres_delete_jobs)
##' }
cipres_delete_job <- function(handle, verbose = TRUE, ...) {
    res <- cipres_DELETE(path = handle, ...)
    if(identical(res, "")) {
        if (verbose) message("Job ", sQuote(handle), " deleted successfully.")
        return(invisible(TRUE))
    }
    else {
        warning("Something probably went wrong.")
        return(invisible(FALSE))
    }
}
