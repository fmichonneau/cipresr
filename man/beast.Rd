% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beast1.R, R/beast2.R
\name{cipres_submit_beast1}
\alias{cipres_submit_beast1}
\alias{cipres_submit_beast2}
\title{Submit a BEAST or BEAST2 analysis}
\usage{
cipres_submit_beast1(input_file, beast_version = c("1.8.2", "1.8.1", "1.8.0"),
  use_beagle = TRUE, max_runtime = 10, codon_partitioning = FALSE,
  use_seed = NULL, job_name = NULL, get_email = TRUE, note = NULL, ...)

cipres_submit_beast2(input_file, beast_version = c("2.3.0", "2.1.3"),
  max_runtime = 10, use_beagle = TRUE, use_seed = NULL,
  overwrite_logs = TRUE, job_name = NULL, get_email = TRUE, note = NULL,
  ...)
}
\arguments{
\item{input_file}{An XML input file generated by BEAUTI2}

\item{beast_version}{Which version of BEAST2 should be used?}

\item{use_beagle}{Should the BEAGLE library be used?}

\item{max_runtime}{How long should the job run for? (in hours)}

\item{codon_partitioning}{Do the partitions follow the codon positions?}

\item{use_seed}{If \code{NULL} (default), a random seed will be
generated by BEAST, otherwise specify your own seed.}

\item{job_name}{An optional name to identify the job.}

\item{get_email}{Should an email be sent when the job completed?}

\item{note}{An optional character string to record notes about the job.}

\item{...}{arguments to customize the API call. These arguments are passed to the appropriate \code{httr} function (\code{\link[httr]{GET}}, \code{\link[httr]{POST}}, \code{\link[httr]{DELETE}})}

\item{overwrite_logs}{Should existing log files on the server be
overwritten?}
}
\description{
For more information see: \itemize{ \item{For
BEAST1}{\url{http://www.phylo.org/rest/beast2_xsede.html} and the
BEAST2 website \url{http://beast2.org}} \item{For
BEAST2}{\url{http://www.phylo.org/index.php/rest/beast_tg.html}
and the BEAST website \url{http://beast.bio.ed.ac.uk}} }
}
\details{
Specifying the number of partitions and patterns in the alignments
are not required by BEAST, but are used by CIPRES to optimize the
computing parameters for improved run times.
}

