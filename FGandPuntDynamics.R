library(nnet)
predict_FG_prob <- function(fp) {
  c0 <- -6.361913
  c1 <- 0.106210
  logit <- c0 + c1 * fp
  return(exp(logit) / (1 + exp(logit))) 
}

predict_decision_probs <- function(fp, ydstogo) {
  a0 <- -1.373438
  a1 <- 0.0854324
  a2 <- -0.19078054
  
  b0 <- -6.125479
  b1 <- 0.1747688
  b2 <- -0.04108412
  
  log_go <- a0 + a1 * fp + a2 * ydstogo
  log_punt <- b0 + b1 * fp + b2 * ydstogo
  
  exp_go <- exp(log_go)
  exp_punt <- exp(log_punt)
  exp_fg <- 1  
  
  total <- exp_fg + exp_go + exp_punt
  return(c(FG = exp_fg / total, Punt = exp_punt / total, GoForIt = exp_go / total))
}

