This issue is submitted by a new maintainer.

## R CMD check results

There is a new NOTE because an additional repository has been specified in order
to suggest cmdstan.  This is the same approach used in brms:
https://cran.r-project.org/web/packages/brms/index.html

# CRAN status issues

Checks are currently failing on r-patched. We were unable to reproduce this on a
local r-patched install on Ubuntu. It seems the exact same issue affects quite a
few packages that depend on rstan, e.g. 
https://cran.r-project.org/web/checks/check_results_baggr.html
https://cran.r-project.org/web/checks/check_results_bakR.html
