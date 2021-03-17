
# unirel

<!-- badges: start -->
<!-- badges: end -->

The goal of unirel is to calculate and compare various unidimensional 
reliability coefficients. 

+ Provides the following reliability coefficients
  + coefficient alpha
  + Unidimensional confirmatory factor analysis reliability, commonly referred to as composite reliability
  + Gilmer-Feldt reliability coefficient
  + Feldt's classical congeneric reliability coefficient
+ Collects and compare reliability coefficients provided by other packages
  + Two versions of GLB (greatest lower bounds) offered by the package psych
  + Guttman's lamdas offered by the package Lambda4 

## Installation

You can install the released version of unirel from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("unirel")
```

## Example

The most typical use would be the unirel function comparing 13 reliability 
coefficients:

``` r
library(unirel)
unirel(Graham1)
## compare various unidimensional reliability coefficients
```
You can also get each coefficient separately.
``` r
joreskog(Graham1)
## obtain composite (congeneric) reliability (unidimensional CFA reliability)
```
