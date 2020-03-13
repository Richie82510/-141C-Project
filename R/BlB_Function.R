
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#-----------------------------------------------
#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @details

#' Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"
#' @export

#---------------------------------

#this part can use to test functions

library(caTools)
library(readr)
library(tidyverse)
adult <- read_csv("adult.csv")%>%as_tibble()
adult$income<-ifelse(adult$income == ">50K",1,0)
adult$`native-country` <- as.factor(adult$`native-country`)
adult$`marital-status` <- as.factor(adult$`marital-status`)
adult$workclass <- as.factor(adult$workclass)
adult$race<-as.factor(adult$race)
adult$education<-as.factor(adult$education)
adult$occupation<-as.factor(adult$occupation)
adult$relationship<-as.factor(adult$relationship)
adult$gender<-as.factor(adult$gender)
adult$income<-as.factor(adult$income)

#fit model
fit<-blbglm(income~age+`hours-per-week`,data=adult,m=3,B=100)
#compute coefficients
coef.blblm(fit)
#compute sigm
sigma.blbglm(fit)
#confidence interval for sigma
sigma.blbglm(fit,ci=TRUE)
#confidence interval for coefficients
confint.blbglm(fit)
#----------------------------



#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}

#' compute the regression estimates for a blb dataset
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, family = binomial(),weights = freqs)
  list(sigma = blbsigma(fit), coef = blbcoef(fit))
}
#
#' compute the estimates
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

blbsigma <- function(fit) {
  sigma(fit)
}

blbcoef<-function(fit){
  fit$coefficients
}

blbglm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula,data_list= data_list)
  class(res) <- "blbglm"
  invisible(res)
}

#' estimate the regression estimates based on given number of repetitions
coef.blbglm <- function(fit) {
  map(fit$estimates,~map(.,"coef"))%>%
    map(.,~reduce(.,rbind))%>%
    map(.,~apply(.,2,mean))%>%
    reduce(`+`)/length(fit$estimates)
}

sigma.blbglm <- function(fit,ci=FALSE) {
  est<-map(fit$estimates,~map(.,"sigma")%>%reduce(.,rbind))
    sigma<-est%>%
    map(.,~apply(.,2,mean))%>%
    reduce(`+`)/length(fit)
    print(sigma)
  #confidence interval
  if(ci)
    est%>%map(.,~apply(.,2,function(x)quantile(x,c(0.025,0.975))))%>%reduce(`+`)/length(fit)
}

confint.blbglm <- function(fit) {
  map(fit$estimates,~map(.,"coef")%>%reduce(.,rbind))%>%
    map(.,~apply(.,2,function(x)quantile(x,c(0.025,0.975))))%>%
    reduce(`+`)/length(fit)
}

