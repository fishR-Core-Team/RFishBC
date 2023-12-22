[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10426347.svg)](https://doi.org/10.5281/zenodo.10426347)
[![CRAN Status](http://www.r-pkg.org/badges/version/RFishBC)](http://www.r-pkg.org/pkg/RFishBC)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/fishr-core-team/RFishBC/workflows/R-CMD-check/badge.svg)](https://github.com/fishr-core-team/RFishBC/actions)
[![Codecov test coverage](https://codecov.io/gh/fishr-core-team/RFishBC/branch/master/graph/badge.svg)](https://codecov.io/gh/fishr-core-team/RFishBC?branch=master)
[![CRAN RStudio mirror downloads rate](http://cranlogs.r-pkg.org/badges/RFishBC)
![CRAN RSTudio mirror downloads total](http://cranlogs.r-pkg.org/badges/grand-total/RFishBC)](http://www.r-pkg.org/pkg/RFishBC)
[![Rdoc](http://www.rdocumentation.org/badges/version/RFishBC)](http://www.rdocumentation.org/packages/RFishBC)


## RFishBC <img src="man/figures/logo.png" align="right" height="200" hspace="15" />

The **RFishBC** package helps fisheries scientists collect measurements from calcified structures and back-calculate estimated lengths at previous ages. **RFishBC** is intended to replace much of the functionality provided by the now out-date fishBC software.

### Use

General desriptions for using **RFishBC** are under the *Vignettes* tab at the top of [this page](https://fishr-core-team.github.io/RFishBC/). I suggest at least quickly reading each vignette to get an overall feel for **RFishBC** and then following the workflow suggestions in the last vignette. Detailed descriptions for each function are under the *Reference* tab at the top of [that page](https://fishr-core-team.github.io/RFishBC/).

### Installation

The [most recent stable version (on CRAN)](https://cloud.r-project.org/package=RFishBC) of **RFishBC** may be installed with

```r
install.packages("RFishBC")
```

The most recent development version may be installed from GitHub with

```r
if (!require('remotes')) install.packages('remotes'); require('remotes')
remotes::install_github('fishr-core-team/RFishBC',ref="dev")
```

You may need to have R Tools installed on your system to use the two lines above. See the instructions for ([R Tools for Windows](https://cran.r-project.org/bin/windows/Rtools/) or [R Tools for Mac OS X](https://cran.r-project.org/bin/macosx/tools/)). Additionally, you may need the X11 graphics functionality if using Mac OS.

### Questions / Comments / Problems or Contributions
Report questions, comments, or bug reports on the [issues page](https://github.com/fishR-Core-Team/RFishBC/issues).

We are always looking for others to contribute to **RFishBC**. Please feel free to make a pull request via GitHub or to contact the maintainers.

Please adhere to the [Code of Conduct](https://fishr-core-team.github.io/RFishBC/CODE_OF_CONDUCT.html).

