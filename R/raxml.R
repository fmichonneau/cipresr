##' Submit to CIPRES an RAxML job. For more information, please
##' consult the RAxML manual available on the RAxML website
##' (\url{http://sco.h-its.org/exelixis/web/software/raxml}).
##'
##' CIPRES offers several flavors of RAxML but \code{cipresr} only
##' provides an interface with the full RAxML version. Setting the
##' correct options for a given run can be challenging as RAxML is
##' complex and feature-rich software. Read the RAxML manual
##' carefully, double check for potential warnings and error messages
##' in the output files, and your results.
##'
##' @title Submit RAxML analysis
##' @param input_file An alignment file in relaxed interleaved for
##'     sequential PHYLIP (or FASTA) format (-s)
##' @param select_analysis The type of analysis to be performed.
##' @param starting_tree A NEWICK tree file specifying the starting
##'     tree. If \code{NULL} (default), RAxML uses a randomized
##'     stepwise addition Parsimony starting tree.
##' @param constraint A NEWICK tree file specifying topology
##'     constraining the tree search. If \code{NULL} (default), no
##'     constraint is used.
##' @param binary_backbone The file name of a binary constraint
##'     tree. If \code{NULL} (default), no constraint is used.
##' @param partition A file specifying the partitions in the alignment
##'     file using the RAxML format (refer to the RAxML manual for
##'     details). If \code{NULL} (default), the full alignment is
##'     considered as a single partition.
##' @param exclude A file specifying positions in the alignment to
##'     exclude. If \code{NULL} (default), no position is excluded.
##' @param weights A file indicating the weights for each column
##'     position in the alignment. If \code{NULL} (default), all
##'     alignments positions have identical weights.
##' @param user_prot_matrix A file containing an amino-acid
##'     substitution model.
##' @param secondary_structure A file specifying a secondary structure
##'     model for the alignment.
##' @param bootstrap_topologies A file containing topologies for a
##'     posteriori bootstrapping.
##' @template max_runtime
##' @param CAT_categories When using a CAT model, how many rate
##'     categories (-c)?
##' @param parsimony_seed For all options that use randomization, this
##'     seed must be specified (-p).
##' @param datatype The type of data used in the alignment.
##' @param outgroups Specify in name of a single outgroup
##'     (\dQuote{Cat}) or a comma-separated list of outgroups (no
##'     spaces: \dQuote{Cat,Dog,Bird}). If several species are listed
##'     as outgroup, only the first one in the list will be used a
##'     outgroup.
##' @param p_inv Should a proportion of invariant site be estimated?
##'     (default \code{FALSE}).
##' @param empirical_freq Should empirical frequencies be used with
##'     amino-acid alignments? (default \code{FALSE}).
##' @param print_brlen Should branch lengths be printed on the screen?
##'     Default \code{FALSE}.
##' @param more_memory Set to \code{TRUE} if your analysis requires
##'     more than 125GB of memory.
##' @param n_char If using \code{more_memory}, set to the number of
##'     characters in your alignment to estimate the amount of memory
##'     needed.
##' @param n_tax If using \code{more_memory}, set to the number of
##'     taxa in your alignment to estimate the amount of memory
##'     needed.
##' @param disable_check_sequence Disable checking for completely
##'     undetermined sequences (it normally doesn't make sense to
##'     incude them in the analysis). Default
##'     \code{FALSE}. (\code{-O}).
##' @param mesquite_output Generate output files that can be parsed by
##'     MESQUITE. Default \code{FALSE}. (Not allowed by CIPRES in runs
##'     with bootstrapping or alternative runs are used)
##'     (\code{-mesquite}).
##' @param model The substitution model (\code{-m}).
##' @param n_runs The number of alternative runs on distinct starting
##'     trees (\code{-#} or \code{-N}).
##' @param no_bfgs If \code{TRUE}, disable the BFGS method when
##'     optimizing the GTR rates (default \code{FALSE}).
##' @param intermediate_tree_files If \code{TRUE}, the intermediate
##'     tree will be written to files (\code{-j}).
##' @param convergence_criterion ML search convergence criterion uses
##'     Robinson-Foulds distance. Default \code{FALSE}. Use on trees
##'     with large number of taxa (\code{-D}).
##' @param majority_rule When computing the majority rule consensus
##'     tree (\code{select_analysis="J"}), which method to use?
##' @param protein_matrix Which amino-acid substitution matrix to use
##'     when working with protein sequence alignment?
##' @param bootstrap_type Which type of bootstrapping algorithm to use
##'     (with \code{select_analysis} is \code{fa}, \code{fd}, or
##'     \code{fo})? \code{x} for rapid bootstrapping and \code{b} for
##'     normal bootstrapping.
##' @param bs_seed_value The seed to use for the bootstrapping.
##' @param n_bootstrap_rep If \code{NULL}, the number of bootstrap
##'     replicate will be selected by RAxML according to the algorithm
##'     specified by the option \code{bootstop_type}. Other an integer
##'     specifiying the number of replicates.
##' @param bootstop_type The type of algorithm used to estimate the
##'     number of replicates.
##' @template job_name
##' @template get_email
##' @template dotdotdot
##' @author Francois Michonneau
##' @export
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
                                bs_seed_value = 12345,
                                n_bootstrap_rep = NULL,
                                bootstop_type = c("autoMRE", "autoFC", "autoMR", "autoMRE_IGN"),
                                job_name = NULL,
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

    assertthat::assert_that(is_maxruntime(max_runtime))
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
        bdy$`vparam.seed_value_` <- bs_seed_value
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

    bdy <- add_meta_data(bdy, get_email, job_name)

    bdy <- lapply(bdy, as.character)

    bdy$`tool` <- "RAXMLHPC8_REST_XSEDE"

    res <- cipres_POST(body = bdy, ...)
    cipres_process_results(res)
}
