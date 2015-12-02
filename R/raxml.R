cipres_submit_raxml <- function(input_file,
                                select_analysis = c("fa", "fd", "fo", "fA", "J", "y", "I"),
                                starting_tree = NULL,
                                constraint =  NULL,
                                binary_backbone = NULL,
                                partition = NULL,
                                exclude = NULL,
                                weights = NULL,
                                user_prot_matrix = NULL,
                                secondary_structure = NULL,
                                bootstrap_topologies = NULL,
                                max_runtime = 10,
                                CAT_categories = NULL,
                                parsimony_seed = NULL,
                                datatype = c("dna", "protein", "rna", "binary", "mutli"),
                                outgroups = NULL,
                                p_inv = FALSE,
                                empirical_freq = TRUE,
                                print_brlen = FALSE,
                                more_memory = FALSE,
                                n_char = NULL,
                                n_tax = NULL,
                                disable_check_sequence = FALSE,
                                mesquite_output = FALSE,
                                model = "GTRGAMMA",
                                n_runs = 1,
                                no_bfgs = FALSE,
                                intermediate_tree_files = FALSE,
                                convergence_criterion = FALSE,
                                majority_rule = c("MRE", "STRICT",
                                                  "MR_DROP",
                                                  "STRICT_DROP"),
                                protein_matrix = c("DAYHOFF", "DCMUT",
                                                   "JTT", "MTREV",
                                                   "WAG", "RTREV",
                                                   "CPREV", "VT",
                                                   "BLOSUM62",
                                                   "MTMAM", "LG",
                                                   "MTART", "MTZOA",
                                                   "PMB", "HIVB",
                                                   "HIVW", "JTTDCMUT",
                                                   "FLU", "DUMMY",
                                                   "DUMMY2", "AUTO",
                                                   "LG4M", "LG4X",
                                                   "PROT_FILE",
                                                   "GTR_UNLINKED",
                                                   "GTR"),
                                bootstrap_type = c("b", "x"),
                                seed_value = 12345,
                                n_bootstrap_rep = NULL,
                                bootstop_type = c("autoMRE", "autoFC", "autoMR", "autoMRE_IGN"),
                                get_email = TRUE,
                                ...
                                ) {


    check_file(input_file)
    check_file(starting_tree)

    check_file(constraint)
    check_file(binary_backbone)
    check_file(partition)
    check_file(exclude)
    check_file(weights)
    check_file(user_prot_matrix)
    check_file(secondary_structure)
    check_file(bootstrap_topologies)

    infile <- httr::upload_file(input_file)

    bdy <- list(
        `input.infile_` = infile
    )

    select_analysis <- match.arg(select_analysis)
    bdy$`vparam.select_analysis_` <- select_analysis

    assertthat::assert_that(is.numeric(max_runtime), max_runtime <= 168)
    bdy$`vparam.runtime_` <- max_runtime

    if (!is.null(CAT_categories)) {
        CAT_categories <- as.integer(CAT_categories)
        assertthat::assert_that(assertthat::noNA(CAT_categories))
    }

    if (!is.null(parsimony_seed)) {
        parsimony_seed <- as.integer(parsimony_seed)
        assertthat::assert_that(assertthat::noNA(parsimony_seed))
        bdy$`vparam.provide_parsimony_seed_` <- 1
        bdy$`vparam.parsimony_seed_val_` <- parsimony_seed
    } else {
        bdy$`vparam.provide_parsimony_seed_` <- 0
    }

    datatype <- match.arg(datatype)

    ## should check that outgroups match dimnames of input

    assertthat::assert_that(assertthat::is.flag(p_inv))
    assertthat::assert_that(assertthat::is.flag(empirical_freq))
    assertthat::assert_that(assertthat::is.flag(print_brlen))
    assertthat::assert_that(assertthat::is.flag(more_memory))

    if (!is.null(more_memory)) {
        n_char <- as.integer(n_char)
        n_tax <- as.integer(n_tax)
        assertthat::assert_that(assertthat::noNA(n_char))
        assertthat::assert_that(assertthat::noNA(n_tax))
    }

    assertthat::assert_that(assertthat::is.flag(disable_check_sequence))
    assertthat::assert_that(assertthat::is.flag(mesquite_output))

    if (identical(datatype, "dna")) {
        model <- match.arg(model, c("GTRGAMMA", "GTRCAT"))
        bdy$`vparam.datatype_` <- "dna"
        bdy$`vparam.dna_gtrcat_` <- model
    } else if (identical(datatype, "protein")) {
        ## custom protein matrices not yet implemented
        model <- match.arg(model, c("PROTGAMMA", "PROTCAT"))
        protein_matrix <- match.arg(protein_matrix)
        bdy$`vparam.datatype_` <- "protein"
        bdy$`vparam.prot_sub_model_` <- model
        bdy$`vparam.prot_matrix_spec_` <- protein_matrix
    } else if (identical(datatype, "rna")) {
        model <- match.arg(model, c("S6A", "S6B", "S6C", "S6D", "S6E",
                                    "S7A", "S7B", "S7C", "S7D", "S7E",
                                    "S7F", "S16A", "S16B"))
    } else if (identical(datatype, "binary")) {
        model <- match.arg(model, c("BINCAT", "BINGAMMA"))
    } else if (identical(datatype, "multi")) {
        model <- match.arg(model, c("MULTICAT", "MULTIGAMMA"))
    }


    ## n_runs
    if (n_runs > 1) {
        bdy$`vparam.specify_runs_` <- 1
        bdy$`vparam.altrun_number_` <- n_runs
    } else {
        bdy$`vparam.specify_runs_` <- 0
    }

    ## bootstrapping
    bootstrap_type <- match.arg(bootstrap_type)
    if (select_analysis %in% c("fa", "fd", "fo")) {
        bdy$`vparam.choose_bootstrap_` <- bootstrap_type
        bdy$`vparam.seed_value_` <- seed_value
        if (is.null(n_bootstrap_rep)) {
            bdy$`vparam.choose_bootstop_` <- "bootstop"
            bdy$`vparam.bootstopping_type_` <- match.arg(bootstop_type)
        } else {
            bdy$`vparam.choose_bootstop_` <- "specify"
            bdy$`vparam.bootstrap_value_` <- n_bootstrap_rep
        }

    }

    ## partition file
    if (!is.null(partition)) {
        bdy$`input.partition_` <- httr::upload_file(partition)
    }

    bdy <- lapply(bdy, as.character)
    bdy$`tool` <- "RAXMLHPC8_REST_XSEDE"

    if (get_email) {
        bdy$`metadata.statusEmail` <- "true"
    } else {
        bdy$`metadata.statusEmail` <- "false"
    }

    cipres_POST(body = bdy, ...)
}

if (FALSE) {
    chopper::fas2phy("~/Documents/Diademnidae/original_fromAndrea/20151109-sequences/20151109-COI.afa")

    cipres_submit_raxml(
        "~/Documents/Diademnidae/original_fromAndrea/20151109-sequences/20151109-COI.phy",
        "fa",
        datatype = "dna",
        model = "GTRGAMMA",
        parsimony_seed = "10101",
        bootstrap_type = "x",
        n_runs = "20",
        n_bootstrap_rep = "100",
        seed_value = "10101",
    )
}
