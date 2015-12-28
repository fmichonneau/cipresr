
##' @importFrom storr storr_rds
cipres_notebook_store <- function(custom_path = FALSE) {
    storr::storr_rds(file.path(cipres_app_folder(custom_path),
                               "cipres_notebook"))

}


cipres_create_note <- function(handle, note, custom_path = FALSE) {
    st <- cipres_notebook_store(custom_path)
    st$set(handle, note)
}

cipres_in_notebook <- function(handle, custom_path = FALSE) {
    st <- cipres_notebook_store(custom_path)
    st$exists(handle)
}

cipres_get_notebook <- function(handle, what, custom_path = FALSE) {
    st <- cipres_notebook_store(custom_path)
    if (cipres_in_notebook(handle, custom_path))
        st$get(handle)[[what]]
    else
        NA_character_
}
