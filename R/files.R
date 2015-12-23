##' Be careful, files with identical names will be silently overwritten.
##'
##' @title List and download files associated with a job
##' @param job_handle the job identified
##' @return \code{cipres_list_files} returns a data frame listing all
##'     the files associated with a job. \code{cipres_download_file}
##'     and \code{cipres_download_all} returns whether the files were
##'     successfully downloaded.
##' @export
##' @importFrom assertthat assert_that is.string
##' @importFrom xml2 xml_text xml_find_all
##' @examples
##' \dontrun{
##' ### List the files associated with the oldest job
##' ### cipres_list_files(cipres_list_jobs$handle[1])
##'
##' ### Download all the files associated with the oldest job
##' ### cipres_download_all(cipres_list_jobs$handle[1], tempdir())
##' }
cipres_list_files <- function(job_handle, ...) {

    assertthat::assert_that(assertthat::is.string(job_handle))

    res <- cipres_GET(path = paste(job_handle, "output", sep = "/"),
                      ...)

    what <- c("outputDocumentId", "filename", "length")
    res_lst <- lapply(what, function(x) {
        xml2::xml_text(
            xml2::xml_find_all(res, paste0(".//", x)))
    })

    res <- data.frame(res_lst, stringsAsFactors = FALSE)
    names(res) <- what
    res
}

##' @param file_id the file identifier (outputDocumentId, not the file name)
##' @param outfile the path and file name where the output should be written
##' @export
##' @rdname cipres_list_files
cipres_download_file <- function(job_handle, file_id, outfile, ...) {

    assertthat::assert_that(assertthat::is.string(job_handle))
    assertthat::assert_that(assertthat::is.string(outfile))

    res <- cipres_GET(path = paste(job_handle, "output", file_id, sep = "/"),
                      ...)
    cat(res, file = outfile)
    invisible(file.exists(outfile))
}

##' @param outdir the path where the files should be downloaded
##' @param pattern an optional regular expression to download only the
##'     files that match the pattern
##' @param ignore.case should pattern matching be case sensitive?
##' @template dotdotdot
##' @export
##' @rdname cipres_list_files
cipres_download_all <- function(job_handle, outdir, pattern = NULL,
                                ignore.case = FALSE, ...) {

    if (!file.exists(outdir))
        stop(sQuote(outdir), " doesn't exist. Create it first.")

    lst_files <- cipres_list_files(job_handle = job_handle, ...)

    if (!is.null(pattern)) {
        which_files <- grep(pattern = pattern,
                            x = lst_files[["filename"]],
                            ignore.case = ignore.case)
        lst_files <- lst_files[which_files, ]
    }
    res <- apply(lst_files, 1, function(x) {
        cipres_download_file(job_handle = job_handle,
                             file_id = x["outputDocumentId"],
                             outfile = file.path(outdir, x["filename"]),
                             ...)
    })
    names(res) <- lst_files[["filename"]]
    invisible(res)
}
