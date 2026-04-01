# Normality Checker 
Normality_checker <- function(data, alpha = 0.05, plot = TRUE, na.rm = TRUE){
  
# Pre-processing 
  if(!is.numeric(as.matrix(data))){
    stop("Input data must be numeric.")
  }
  
  if(na.rm){
    if(sum(colSums(is.na(data)))){
    data <- na.remove(data)
    message("---NA Removed---")
   }
  }
  
  if(is.vector(data)){
    if(length(data) <= 4){
      stop("For Lilliefors Test, Sample size must be greater than 4.")
    }
  }
  
  if(is.matrix(data) | is.data.frame(data)){
    if(nrow(data) <= 4){
      stop("For Lilliefors Test, Sample size must be greater than 4.")
    }
  }
  
  
  
# package 
  library(nortest)
  library(tseries)
  
# input check 
  if (is.matrix(data) | is.data.frame(data)){
   # Matrix Form 
    result_list <- list()
    par(mfrow = c(ncol(data),2))
    for (i in 1:ncol(data)){
      x <- data[,i]
      
      # Visualize
    if(plot){
      plot(density(x), main = "Distrbution of Data", xlab = "x", ylab = "density")
      abline(v = 0, col = "grey", lty = 2, lwd = 1.2)
      qqnorm(x, pch = 3, col = "blue")
      qqline(x, col = 'red', lty = 2)
    }
      
      # Numeric Result
      result<- tryCatch({c(Lilliefors = nortest::lillie.test(x)$p.value,
                           Shapiro_Wilk = shapiro.test(x)$p.value,
                           Jarque_Bera = jarque.bera.test(x)$p.value)
      })
      decision <- ifelse(result > alpha, "Normal", "Not Normal")
      
      result_list[[i]] <- data.frame(
        Test = names(result),
        p_value = result,
        Decision = decision
      )
      
    }
    return(result_list)
  }
else{
# Vector
  # Visualize
if(plot){
  par(mfrow = c(1,2))
  plot(density(data), main = "Distrbution of Data", xlab = "x", ylab = "density")
  abline(v = 0, col = "grey", lty = 2, lwd = 1.2)
  qqnorm(data, pch = 3, col = "blue")
  qqline(data, col = 'red', lty = 2)
}
  
  # Numeric Result
  result<- c(nortest::lillie.test(data)$p.value,
             shapiro.test(data)$p.value,
             jarque.bera.test(data)$p.value)
  
  decision <- ifelse(result > alpha, "Normal", "Not Normal")
  
  out <- data.frame(
    Test = c("Lilliefors", "Shapiro-Wilk", "Jarque-Bera"),
    P_Value = result,
    Decision = decision
    )
  par(mfrow = c(1,1))
  return(out)
  }
}


