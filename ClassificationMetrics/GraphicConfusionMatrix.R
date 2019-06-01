GraphConfusionMatrix <- function(yobs,phat, threshold) {
  
  df=data.frame(yobs,phat)
  colnames(df)=c("yobs","phat")  
  v <- rep(NA, nrow(df))
  v <- ifelse(df$phat >= threshold & df$yobs == 1, "TP", v)
  v <- ifelse(df$phat >= threshold & df$yobs == 0, "FP", v)
  v <- ifelse(df$phat < threshold & df$yobs == 1, "FN", v)
  v <- ifelse(df$phat < threshold & df$yobs == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=yobs, y=phat)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}
