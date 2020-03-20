#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @details

#' Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"
#' @export


#' Split data
#'
#' Split Data into M Parts of Approximated Equal Sizes
#'
#' @param data a data frame containing the variables in the function.
#' @param m split data into m parts，default number is 10.
#'
#' @return a list.
#'
#' @example
#' split_data(read.csv("adult.csv"),m=10)
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}



#' Compute Regression Estimates for a BLB Dataset
#'
#' glm_each_boot computes the logistic regression estimates for
#' a bag og little bootstraps, it will run B times.
#'
#'@param formula an object of class "formula" (or one that can be coerced to that class).
#'@param data the sub_data from original data set.
#'@param n the size of original data set.
#'@examples
#'form = income ~ hours.per.week + age + gender
#'glm_each_boot(form, vignette_sample, nrow(vignette_sample))
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}



#' Logistic regression estimates
#'
#' Estimate Regression Estimates Based on a Given Number of Repetitions.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class).
#' @param data the sub_data from original data set.
#' @param freqs the frequency from glm_each_boot function.
#' @examples
#' form = income ~ hours.per.week + age + gender
#' freq <- rmultinom(1, nrow(vignette_sample), rep(1, nrow(vignette_sample)))
#' glm1(form, vignette_sample, freq)
#' ## $sigma
#' ## [1] 1.17456
#' ##
#' ## $coef
#' ##    (Intercept) hours.per.week            age     genderMale
#' ##    -4.94910851     0.05414013     0.02121227     0.64940167
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, family = binomial(),weights = freqs)
  list(sigma = blbsigma(fit), coef = blbcoef(fit))
}




#' Compute the estimates
#'
#' Compute the Estimates for Each Subsample and Fit mMdel for Each Subsmple B Times.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class).
#' @param data the sub_data from original data set.
#' @param n the size of original data set.
#' @param B the subsample repeat B times.
#' @examples
#' form = income ~ hours.per.week + age + gender
#' glm_each_subsample(form, vignette_sample, nrow(df), B=100)
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

#' Compute sigma
#'
#' Compute Sigma from Each Model
#'
#' @param fit the logistic regression model for each subsample.
#' @examples
#' blbsigma(lm(income~age, vignette_sample))
#' ## [1] 0.4159924
blbsigma <- function(fit) {
  sigma(fit)
}

#' Compute the coefficients
#'
#' Compute Coefficients from Each Model
#'
#' @param fit logistic regression model for each subsample.
#' @examples
#' blbcoef(lm(income~age, vignette_sample))
#' ## (Intercept)         age
#' ## 0.013823722 0.005805649
blbcoef<-function(fit){
  fit$coefficients
}


#' Find the Logistic Regression with Bag of Little Bootstraps
#'
#' \code{blbglm} is used to fit logistic models. It can be used to carry out regression,
#' single stratum analysis of variance and analysis of covariance.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class).
#' @param  data an data frame, list or environment containing the variables in the model.
#' @param  m an optional element，split data into m parts，default number is 10.
#' @param  B an optional element, each boot run B times, the default number is 5000.
#'
#' @return a list containing the estimates and formula.
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


#' Compute the Bootstrap Coefficients
#'
#' \code{coef.blbglm} is used to find the coefficient estimates from the bag of little bootstraps
#'  by dividing the sum of each bootstrap sample coefficients by the number of samples.
#'
#' @param fit a vector containing the logistic regression models for each bootstrap sample.
#' @examples
#' coef.blbglm()
#'
#' @return a list of bootstrap coefficient estimates calculated from the BLB estimates
#' @export
#' @method coef blbglm
coef.blbglm <- function(fit) {
  map(fit$estimates,~map(.,"coef"))%>%
    map(.,~reduce(.,rbind))%>%
    map(.,~apply(.,2,mean))%>%
    reduce(`+`)/length(fit$estimates)
}


#' Compute Sigma and Its Confidence Interval for Each Bootstrap Model
#'
#' \code{sigma.blbglm} is used to compute the overall sigma and its 95 percent CI for the whole dataset.
#'
#' @param fit a list of models for the bootstrap samples.
#' @param ci logical. If TRUE, the function will return the 95 percent confidence intervals for each bootstrap model.
#' @export
#' @method sigma blbglm
#' @examples
#' ## NO CI
#' sigma.blbglm(blbfit,ci=FALSE)
#' ## [1] 1.689036
#' ## With CI
#' sigma.blbglm(blbfit,ci=TRUE)
#' ## [1] 1.689036
#' ##           [,1]
#' ## 2.5%  1.517758
#' ## 97.5% 1.816267
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


#' Compute Confidence Interval for Each BLB Coefficient
#'
#' \code{confint.blbglm} is used to compute the 95 percent confidence interval
#'  for each coefficient using Bag of Little Bootstraps.
#'
#' @param fit a list of bootstrap models for the BLB subsamples.
#'
#' @export
#' @method confint blbglm
#' @examples
#' confint.blbglm(blbfit)
#' ##       (Intercept) hours.per.week        age genderMale
#' ## 2.5%   -11.895876     0.04655235 0.01985707  0.4567024
#' ## 97.5%   -4.948412     0.15050993 0.07731212  2.8073899
confint.blbglm <- function(fit) {
  map(fit$estimates,~map(.,"coef")%>%reduce(.,rbind))%>%
    map(.,~apply(.,2,function(x)quantile(x,c(0.025,0.975))))%>%
    reduce(`+`)/length(fit$estimates)
}


#' Apply logistic regression to Bags of Little Bootstraps (BLB)
#'
#' \code{predict.blbglm} predict.blbglm predicts the probability of obtaining a response variable,
#' and returns a 95 percent confidence interval for the probability if confidence = TRUE.
#'
#' @param fit a list of fitted models computed using BLB (e.g. fit<-blbglm(...)).
#' @param testdata a tibble, data frame, or named vector containing the data.
#' @param confidence logical. If TRUE, a confidence interval will be returned.
#'
#' @export
#'
#' @method predict blbglm
#'
#' @examples fit<-blbglm(income~age+`hours-per-week`,data=adult)
#' testdata<-tibble(age=c(25,30), `hours-per-week`=c(50,100))
#' predict.blbglm(fit,testdata,confidence=T)
#'              [,1]           [,2]
#'2.5%     0.1863978     0.09907537
#'97.5%   0.1960761      0.10568005
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
