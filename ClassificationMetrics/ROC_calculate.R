ROC_calculate <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$phat >= threshold & df$yobs == 1) / sum(df$yobs == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$phat >= threshold & df$yobs == 0) / sum(df$yobs == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$phat >= threshold & df$yobs == 0) * cost_of_fp + 
      sum(df$phat < threshold & df$yobs == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}


