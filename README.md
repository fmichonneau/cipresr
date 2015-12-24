[![Travis-CI Build Status](https://travis-ci.org/fmichonneau/cipresr.svg?branch=master)](https://travis-ci.org/fmichonneau/cipresr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cipresr)](http://cran.r-project.org/package=cipresr)
[![Coverage Status](https://img.shields.io/codecov/c/github/fmichonneau/cipresr/master.svg)](https://codecov.io/github/fmichonneau/cipresr?branch=master)

# cipresr :deciduous_tree:

R client to the CIPRES API.

[CIPRES](http://www.phylo.org/index.php) (Cyber Infrastructure for Phylogenetic
Research) is a popular service that gives researchers easy access to NSF's large
computional resources (XSEDE) for phylogenetic inference.

This package allows researchers to submit jobs to CIPRES directly from R. By
enabling access to large computational resources within the analytical workflow,
`cipresr` intends to facilitate reproducibility, as researchers don't need to
navigate through several web pages and click checkboxes to submit their
analysis.

This package is in active development and can currently be used to submit RAxML,
BEAST (v1.8.x), and BEAST2 analyses. A few other tools will soon be added.

# Installation

The package is not available on CRAN and it needs to be installed from GitHub:

```r
devtools::install_github("richfitz/storr")
devtools::install_github("fmichonneau/cipresr")
```

After the installation, you will need to
[create an account](https://www.phylo.org/restusers/register.action) to use the
CIPRES REST services (this is different from the account you may already have to
access the CIPRES web interface).

Then you can type:

```r
library(cipresr)
cipres_setup()
```

This function will take you through the required steps to set up `cipresr`. We
use Rich FitzJohn's [`storr`](https://github.com/richfitz/storr) package to store
your login credentials on your hard drive. Thus, you only need to enter your
login information once.

Once setup, you can submit your jobs, and get the results once the analysis is
done, using `cipresr`:

```r
cipres_submit_beast2(input_file = "my_analysis.xml", n_patterns = 1000, n_partitions = 5)
cipres_download_all("NGBW-JOB-BEAST2_XSEDE-66666666666666666666", "~/my_analysis_results/")
```

Don't hesitate to open an issue if you'd like to have access to a tool other
than BEAST and RAxML.

# Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you
agree to abide by its terms.
