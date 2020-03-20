# Blbglm Project

<!-- badges: start -->
<!-- badges: end -->

Logistic regression with bag of little bootstraps.

This is an R Package Project from 141C Class:

## The Group Member Contains:

Ren, Yiqi (Richie) - yqren@ucdavis.edu

Zhang, Xuecheng (Kristin) - hcczhang@ucdavis.edu

Chow, Jordan - jorchow@ucdavis.edu

Dowling, Brennan - bpdowling@ucdavis.edu

## Responsibility:

Yiqi Ren - R Code

Xuecheng Zhang - Description/Comment of R Function

Jordan Chow - Description/Comment of R Function

Brennan - Vignette of the Usage

## Example

This is a basic sample which shows you how to solve a common problem:

``` r
library(Project)
fit<-blbglm(income~age+`hours-per-week`,data=adult,m=3,B=100)
coef.blbglm(fit)
#>     (Intercept)              age `hours-per-week` 
#>     -4.84324643       0.04210891       0.04694057  
confint.blbglm(fit)
#>      (Intercept)        age `hours-per-week`
#>2.5%    -4.954553 0.04077618       0.04485663
#>97.5%   -4.740632 0.04374523       0.04891164
sigma.blbglm(fit)
#> 1.772809
sigma.blbglm(fit,ci=TRUE)
#>[1] 1.772809
#>          [,1]
#>2.5%  1.765737
#>97.5% 1.780342
testdata<-tibble(age=c(25,30),`hours-per-week`=c(50,30))
predict.blbglm(fit,testdata)
#>[1] 0.1910047 0.1023151
predict.blbglm(fit,testdata,confidence = T)
#>           [,1]       [,2]
#>2.5%  0.1861783 0.09904505
#>97.5% 0.1960103 0.10546168
```


`
