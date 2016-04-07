# SpecsVerification2

The R package SpecsVerification has been considerably revised and improved. A number of new functions were implemented. An article will soon be submitted for publication in Journal of Statistical Software.

Before updating SpecsVerification on CRAN, I am making the new version as well as the JSS paper draft available here for reviewers and for developers whose packages depend on SpecsVerification. Once the paper has been peer-reviewed and all dependencies have been checked, I will update SpecsVerification on CRAN and on github, and delete this repo.

To install SpecsVerification2 from github, use

    devtools::install_github('sieste/SpecsVerification2/R/SpecsVerification2')

The paper was written in latex and knitr. The source file is `tex/ensemble-verification.Rnw`. To (re)compile the paper, change paths in the `Makefile` as necessary, and

    make paper

Please contact s(dot)siegert(at)exeter(dot)ac(dot)uk with any questions or feedback.

