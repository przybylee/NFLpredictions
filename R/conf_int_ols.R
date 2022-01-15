#' Obtain a confidence interval using OLS
#'
#' @param X A design matrix
#' @param y Values of the outputs
#' @param cont Contrasts
#' @param d Value of cont*beta under null hypothesis
#' @param a Confidence level between 0 and 1
#'
#' @return A data frame with the estimate of cont*beta-d with lower and upper bounds of
#' the confidence interval
#' @export
#'
#' @examples
conf_int_ols=function(X,y,cont,d=0,a=0.05){
  reg <- stats::lm(y ~ X+0)
  cont = as.matrix(cont)
  X.X <- t(X)%*%X
  df = nrow(X) - stats::anova(reg)[1,1]
  cb.d=t(cont)%*%MASS::ginv(X.X)%*%t(X)%*%y-d
  sigsq <- anova(reg)[2,3]
  var.cb <- sigsq*t(cont)%*%MASS::ginv(X.X)%*%cont
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(var.cb)
  lower <- cb.d - tquant%*%std.dev
  upper <- cb.d + tquant%*%std.dev
  df <- data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
  return(df)
}
