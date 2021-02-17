# Introduction to SNA with R

<!-- badges: start -->
[![R-CMD-check](https://github.com/mbojan/isnar/workflows/R-CMD-check/badge.svg)](https://github.com/mbojan/isnar/actions)
<!-- badges: end -->

Functions and datasets accompanying the workshop I teach.

## Installation

In general, use:

``` r
devtools::install_github("mbojan/isnar")
```

Windows users might pick up the latest Windows binary build from AppVeyor:

1. Go to <https://ci.appveyor.com/project/mbojan/isnar/build/artifacts>

2. Download the ZIP archive (`isnar_X.Y-Z.zip` where `XYZ` designate package version).

3. In R/RStudio call:

	```r
	install.packages(file.choose(), repos=NULL)
	```

    File explorer will pop up.

4. Navigate and open the downloaded ZIP file.

5. DONE.
