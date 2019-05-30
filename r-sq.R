r.sq <- function(raw,fit) {
  # y <- raw
  # y_bar <- mean(raw)
  # y_hat <- fit
  # r_sq <- sum((y_hat-y_bar)^2) / sum((y-y_bar)^2)
  r_sq <- cor(raw,fit)^2
  return(r_sq)
}