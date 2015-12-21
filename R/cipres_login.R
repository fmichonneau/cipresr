##' @importFrom rappdirs user_data_dir
cipres_app_folder <- function(custom_path = FALSE, create = TRUE) {

    if (custom_path) {
        cipres_dir <- Sys.getenv("CIPRES_DIR")
        if (!nzchar(cipres_dir)) {
            stop("If you want to use a custom path, you need to set the ",
                 "variable `CIPRES_DIR` in your .Renviron file.")
        }
    } else {
        cipres_dir <- rappdirs::user_data_dir(appname=APPNAME, appauthor=APPAUTHOR)
    }

    if (!file.exists(cipres_dir) && create) {
        message(cipres_dir, " doesn't exist.")
        if (! interactive())
            stop("You need to run in interactive mode before continuing.")
        create_dir <- readline(paste("Do you want to create", cipres_dir, "(y/n)? "))
        if (substr(create_dir, 1, 1) == "y") {
            dir.create(cipres_dir, recursive = TRUE)
        } else {
            stop("interrupted by user.")
        }
    }
    cipres_dir
}

##' @importFrom storr storr_rds
cipres_cred_store <- function(custom_path) {
    storr::storr_rds(file.path(cipres_app_folder(custom_path),
                               "cipres_credentials"))
}

cipres_get_creds <- function(what = c("CIPRES_USER", "CIPRES_PWD",
                                      "CIPRES_APP_ID"),
                             custom_path = FALSE) {

    st <- cipres_cred_store(custom_path)

    is_set <- vapply(what, function(x) st$exists(x), logical(1))

    for (q in seq_along(is_set[!is_set])) {
        wch <- names(is_set)[!is_set][q]
        message(wch, " doesn't exist.")
        if (! interactive())
            stop("You need to run in interactive mode before continuing.")
        rsp <- readline("Do you want to create it (y/n)? ")
        if (substr(rsp, 1, 1) == "y") {
            val <- readline(paste0("What is ", sQuote(wch), " ? "))
            st$set(wch, val)
        }
    }

    is_set <- vapply(what, function(x) st$exists(x), logical(1))

    if (any(!is_set)) {
        stop(paste(names(is_set)[!is_set], collapse = ", ", sep = ""),
             " is not set.")
    } else {
        invisible(st)
    }
}

cipres_update_creds <- function(key, value, custom_path = FALSE) {
    st <- cipres_cred_store(custom_path)
    if (! key %in% st$list()) {
        stop("invalid key")
    } else {
        st$set(key, value)
    }
}



##' List with the login information
##'
##' This function is mostly intended to be called internally but could
##' be useful to verify the login information.
##'
##' @title Access login information for the CIPRES API
##' @return A list with the login information (user, password, and
##'     application ID).
##' @author Francois Michonneau
##' @export
cipres_login <- function() {
    list(user = cipres_get_creds()$get("CIPRES_USER"),
         password = cipres_get_creds()$get("CIPRES_PWD"),
         app_id = cipres_get_creds()$get("CIPRES_APP_ID"))
}



##' Interactive function to setup the credential information
##'
##' This function guides you through the steps needed to store your
##' login information for using the CIPRES API. You should use it the
##' first time you use this package.
##' @title Set up CIPRES login information
##' @author Francois Michonneau
##' @export
cipres_setup <- function() {
    if (Sys.getenv("CIPRES_DIR") != "" &&
        !file.exists(cipres_app_folder(create = FALSE))) {
        if (! interactive()) stop("Setup can only be run in interactive mode.")
        message("It looks like you haven't used ", sQuote("cipresr"), " before. \n\n")

        cat(strwrap("First, you need to create an account with https://www.phylo.org/restusers/register.action"),
            sep = "\n")
        cat("\n", strwrap(paste("**Note** that it is a different account from the one you might use with the CIPRES Portal!",
                          "The email address you will provide CIPRES will be the one where you receive notifications",
                          "that the job has completed."), width = 70), sep = "\n")

        accnt <- readline("Did you create an account and activated it (y/n)? ")
        if (substr(accnt, 1, 1) != "y") stop("interrupted by user.")

        cat(strwrap(paste("Great! Now that your account is activated, on the website, go to ",
                    "Developer > Application Management, and click on the 'Create New Application' button.",
                    "Fill out the form with the appropriate information (the name of the application is up to you)",
                    "but make sure you choose 'DIRECT' for 'Authentification Type'.",
                    "After submiting the form, leave the webpage open."), width = 70), "\n", sep = "\n")

        cat(strwrap(paste("Now you will be asked to create the directory where your login credentials are stored,",
                          "followed by your username (CIPRES_USER), your password (CIPRES_PWD), and your",
                          "application ID (CIPRES_APP_ID, something like yourapp-E215F452YTJO146...)."),
                    width = 70), "\n",  sep = "\n")
        cipres_get_creds()
        message("You are all set!")
        message("You can check everything is correct by typing cipres_login() \n")
        message("If you need to change something, use cipres_update_creds().")
        message("e.g. cipres_update_creds('CIPRES_USER', 'correct_value')")
        } else {
            stop("It looks like you are already set up.")
        }
}
