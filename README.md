[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1218245.svg)](https://doi.org/10.5281/zenodo.1218245)
[![CRAN Status](http://www.r-pkg.org/badges/version/RFishBC)](http://www.r-pkg.org/pkg/RFishBC)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/droglenc/RFishBC?branch=master&svg=true)](https://ci.appveyor.com/project/droglenc/RFishBC)
[![Travis-CI Build Status](https://travis-ci.org/droglenc/RFishBC.svg?branch=master)](https://travis-ci.org/droglenc/RFishBC)
[![Coverage Status](https://img.shields.io/coveralls/droglenc/RFishBC.svg)](https://coveralls.io/r/droglenc/RFishBC?branch=master)
[![CRAN RStudio mirror downloads rate](http://cranlogs.r-pkg.org/badges/RFishBC)
![CRAN RSTudio mirror downloads total](http://cranlogs.r-pkg.org/badges/grand-total/RFishBC)](http://www.r-pkg.org/pkg/RFishBC)
[![Rdoc](http://www.rdocumentation.org/badges/version/RFishBC)](http://www.rdocumentation.org/packages/RFishBC)


## RFishBC <img src="man/figures/logo.png" align="right" height="200" hspace="15" />

The **RFishBC** package helps fisheries scientists collect measurements from calcified structures and back-calculate estimated lengths at previous ages. **RFishBC** is intended to replace much of the functionality provided by the now out-date fishBC software.

### Use

General desriptions for using **RFishBC** are under the *Vignettes* tab at the top of the page. I suggest at least quickly reading each vignette to get an overall feel for **RFishBC** and then following the workflow suggestions in the last vignette. Detailed descriptions for each function are under the *Reference* tab at the top of the page.

### Installation

**RFishBC** is now available on CRAN and can be installed as usual from there. The most recent development version (on GitHub) of **RFishBC** may be installed by running the two lines below in your R console.

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
devtools::install_github('droglenc/RFishBC')
```

You may need to have R Tools installed on your system to use the two lines above. See the instructions for ([R Tools for Windows](https://cran.r-project.org/bin/windows/Rtools/) or [R Tools for Mac OS X](https://cran.r-project.org/bin/macosx/tools/)). Additionally, you may need the X11 graphics functionality if using Mac OS.
