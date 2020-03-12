
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

blbcoef<-function(formula,data){
  m<-10
  r<-1000
  n<-nrow(data)
  groups<-sample(seq_len(m),n,replace=TRUE)
  map(seq_len(m), ~write_csv(adult[groups==.,], str_c("part",.,".csv")))

  calc_coef<-function(subsample,freqs){
    log.model <- glm(formula, family = binomial(), weights=freqs, subsample)
    log.model$coefficients
  }
  each_boot<-function(i,ind_part){
    subsample=read_csv(str_c("part",ind_part,".csv"))
    freqs=rmultinom(1,n,rep(1,nrow(subsample)))
    calc_coef(subsample,freqs)
  }
  #calculate Confidence interval for each subsample
  indCI<-function(x){
    val<-NULL
    pci<-NULL
    for (i in 1:length(x[[1]])) {
      for (j in 1:length(x)) {
        val[j]<-x[[j]][i]
      }
      pci[[i]]<-val%>%quantile(c(0.025, 0.975),na.rm = TRUE)
    }
    pci
  }
  #using a function to combine all samples' confidence intervals
  reduceCI<-function(x){
    val<-NULL
    pci<-NULL
    for (i in 1:length(x[[1]])) {
      for (j in 1:length(x)) {
        val[j]<-x[[j]][i]
      }
      pci[[i]]<-reduce(val, `+`) / length(val)
    }
    pci
  }
  #save all confidence intervals of all subsamples into cilist
  cilist <- map(seq_len(m),~{
    map(seq_len(r), each_boot, ind_part =.)%>%
      indCI(.)
    })
  #combine all subsample together
  reduceCI(cilist)
  }
