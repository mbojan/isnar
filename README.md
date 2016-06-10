# Introduction to SNA with R

[![Build Status](https://travis-ci.org/mbojan/isnar.png?branch=master)](https://travis-ci.org/mbojan/isnar)
[![Build Status](https://ci.appveyor.com/api/projects/status/nnietkuq6na1lfuv?svg=true)](https://ci.appveyor.com/project/mbojan/isnar)

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
