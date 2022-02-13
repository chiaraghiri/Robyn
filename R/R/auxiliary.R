# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function format_unit, get_rsq

# Format unit
format_unit <- function(x_in) {
  x_out <- sapply(x_in, function(x) {
    if (abs(x) >= 1000000000) {
      x_out <- paste0(round(x / 1000000000, 1), " B")
    } else if (abs(x) >= 1000000 & abs(x) < 1000000000) {
      x_out <- paste0(round(x / 1000000, 1), " M")
    } else if (abs(x) >= 1000 & abs(x) < 1000000) {
      x_out <- paste0(round(x / 1000, 1), " K")
    } else {
      x_out <- round(x, 0)
    }
  }, simplify = TRUE)
  return(x_out)
}

# Calculate R-squared
get_rsq <- function(true, predicted, p = NULL, df.int = NULL) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  if (!is.null(p) & !is.null(df.int)) {
    n <- length(true)
    rdf <- n - p - 1
    rsq <- 1 - (1 - rsq) * ((n - df.int) / rdf)
  }
  return(rsq)
}

# Robyn colors
robyn_palette <- function() {
  list(
    fill = rep(c("#21130d","#351904","#543005","#8C510A","#BF812D","#DFC27D","#F6E8C3"
        ,"#F5F5F5","#C7EAE5","#80CDC1","#35978F","#01665E","#043F43", "#04272D"), 2),
    colour = rep("#000000", 24)
  )
}

#calculate AIC and AICc
compute_AIC<-function(model_fit){
  #tll <- (1-fit1$dev.ratio)*fit1$nulldev
  tLL <- - deviance(model_fit)
  k <- model_fit$df
  n <- model_fit$nobs
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  AICc

  AIC<- -tLL+2*k
  return(list(AIC=AIC,AICc=AICc))
}

