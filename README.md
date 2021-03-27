
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
  + Hancock's H
  + Ten Berge and Zegers's mu series
+ Collects and compare reliability coefficients provided by other packages
  + Two versions of GLB (greatest lower bounds) offered by the package psych
  + Guttman's lamdas offered by the package Lambda4 

## Installation

You can download and install it from Github using the devtools package:

``` r
install.packages("devtools")
devtools::install_github("eunscho/unirel")
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
alpha(Graham1)
## obtain coefficient alpha
joreskog(Graham1)
## obtain composite (congeneric) reliability (unidimensional CFA reliability)
gilmer(Graham1)
## obtain the Gilmer-Feldt coefficient
feldt(Graham1)
## obtain Feldt's classical congeneric reliability
hancock(Graham1)
## obtain Hancock's H (maximal reliability)
heise(Graham1)
## obtain Heise-Borhnstedt's Omega
kaiser(Graham1)
## obtain Kaiser-Caffrey's alpha
mu2(Graham1)
## obtain Ten Berge and Zegers' mu2
mu3(Graham1)
## obtain Ten Berge and Zegers' mu3
mu4(Graham1)
## obtain Ten Berge and Zegers' mu4
```
## Troubleshooting

Sometimes an error message appears.

``` r
Error in standardizedsolution(fit) :
```
The solution is to activate the lavaan package.

``` r
library(lavaan)
```
