#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @details

#' Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"
#' @export


#' split data
#'
#' split data into m parts of approximated equal sizes
#'
#' @param data an data frame containing the variables in the function.
#' @param m split data into m parts，default number is 10
#'
#' @return list
#'
#' @example
#' split_data(adult,m=10)
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}



#' compute the regression estimates for a blb dataset
#'
#' glm_each_boot can compute the logistic regression estimates for
#' a bag og litte bootstraps, it will run B times.
#'
#'@param formula an object of class "formula" (or one that can be coerced to that class)
#'@param data the sub_data from original data set
#'@param n the size of original data set
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}



#' logistic regression estimates
#'
#' estimate the regression estimates based on given number of repetitions
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class)
#' @param data the sub_data from original data set
#' @param freqs the frequence from glm_each_boot function
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, family = binomial(),weights = freqs)
  list(sigma = blbsigma(fit), coef = blbcoef(fit))
}




#' compute the estimates
#'
#' compute the estimates for each subsample and fit model for each subsmple B times
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class)
#' @param data the sub_data from original data set
#' @param n the size of original data set
#' @param B the subsample repeat B times
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

#' compute sigma
#'
#' cumpute sigma from each model
#'
#' @param fit the logistic regression model for each subsample
blbsigma <- function(fit) {
  sigma(fit)
}

#' compute the coefficients
#'
#' compute the coefficients from each model
#'
#' @param fit the logistic regression model for each subsample
blbcoef<-function(fit){
  fit$coefficients
}


#' find the logistic regression with bag of little bootstraps
#'
#' blbglm is used to fit logistic models. It can be used to carry out regression,
#' single stratum analysis of variance and analysis of covariance
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class)
#' @param  data an data frame, list or environment containing the variables in the model.
#' @param  m an optional element，split data into m parts，default number is 10
#' @param  B an optional element, eachoots run B times,dedefault number is 5000
#'
#' @export
#' @examples
#' require (adult)
#' blbglm(income~age+`hours-per-week`,data=adult,m=3,B=100)
blbglm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}

#' @export
#' @method coef blbglm
coef.blbglm <- function(fit) {
  map(fit$estimates,~map(.,"coef"))%>%
    map(.,~reduce(.,rbind))%>%
    map(.,~apply(.,2,mean))%>%
    reduce(`+`)/length(fit$estimates)
}

#' @export
#' @method sigma blbglm
sigma.blbglm <- function(fit,ci=FALSE) {
  est<-map(fit$estimates,~map(.,"sigma")%>%reduce(.,rbind))
  sigma<-est%>%
    map(.,~apply(.,2,mean))%>%
    reduce(`+`)/length(fit$estimates)
  print(sigma)
  #confidence interval
  if(ci)
    est%>%map(.,~apply(.,2,function(x)quantile(x,c(0.025,0.975))))%>%reduce(`+`)/length(fit$estimates)
}

#' @export
#' @method confint blbglm
confint.blbglm <- function(fit) {
  map(fit$estimates,~map(.,"coef")%>%reduce(.,rbind))%>%
    map(.,~apply(.,2,function(x)quantile(x,c(0.025,0.975))))%>%
    reduce(`+`)/length(fit$estimates)
}

#' @export
#' @method predict blbglm
predict.blbglm<-function(fit,testdata,confidence=FALSE){
  coef<-map(fit$estimates,~map(.,"coef")%>%reduce(.,rbind))
  X<-model.matrix(reformulate(attr(terms(fit$formula), "term.labels")), testdata)
  t<-map(coef,~apply(.,1,function(y)X%*%y))%>%
    map(.,rowMeans)%>%
    bind_rows%>%rowMeans()
  if(confidence){
    map(coef,~apply(.,1,function(y)X%*%y))%>%
      map(.,data.frame)%>%
      map(.,~apply(.,1,function(x)exp(x)/(1+exp(x))))%>%
      map(.,~apply(.,2,function(x)quantile(x,c(0.025,0.975))))%>%
      reduce(`+`)/length(coef)
  }
  else
    print(exp(t)/(1+exp(t)))
}

