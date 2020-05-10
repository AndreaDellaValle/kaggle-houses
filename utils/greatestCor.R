correlations <- function(numCor) {
  # trying to print the greater correlations only
  greaterCorrelationCoefficent <- which(numCor >= 0.75 & numCor < 1, arr.ind = TRUE)
  
  corCoefficentList <- numCor[numCor >= 0.75 & numCor < 1.0]
  
  relatedCatList = greaterCorrelationCoefficent[,"col"]
  
  range <- nrow(greaterCorrelationCoefficent)
  
  relationList = data.frame(
    cat1=character(),
    cat2=character(),
    corCoeff=integer(),
    stringsAsFactors = FALSE
  )
  
  foreach(i = 1:range) %do% {
    currentRow <- relatedCatList[i]
    cat1 = colnames(numCor)[currentRow]
    cat2 = rownames(greaterCorrelationCoefficent)[i]
    corCoefficent = corCoefficentList[i]
    
    relationList[i,] = c(cat1, cat2, corCoefficent)
  }
  return(relationList)
}