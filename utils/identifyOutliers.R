identifyOutliers <-  function(x) {
  qnt <- quantile(x, probs=c(.25, .75))
  H <- 1.5 * IQR(x)
  outlier <- (x < (qnt[1] - H)) | (x > qnt[2] + H)
  outlier
}