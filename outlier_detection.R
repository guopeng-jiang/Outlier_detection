## remove outliers ##

de_outlirs <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

de_outlirs_iter = function(x){
  i = 1 
  repeat{
    x = de_outlirs(x)
    print(paste0("Outlier count:", sum(is.na(x))))
    i = i+1
    
    if (i == 10){
      break
    }
  }
  return(x)
}
