###############################################################
# write function to apply TFPW then calculate Pettitt's tests #
###############################################################
#Author: Bailey Anderson
#Date: 20/02/2020

tfpw_pettitt <- function (x) 
{
  x = x
  pval = NULL
  if (is.vector(x) == FALSE) {
    stop("Input data must be a vector")
  }
  if (any(is.finite(x) == FALSE)) {
    x <- x[-c(which(is.finite(x) == FALSE))]
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  n <- length(x)
  if (n < 3) {
    stop("Input vector must contain at least three values")
  }
  V <- rep(NA, n * (n - 1)/2)
  k = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      k = k + 1
      V[k] = (x[j] - x[i])/(j - i)
    }
  }
  slp <- median(V, na.rm = TRUE)
  t = 1:length(x)
  xt <- (x[1:n]) - ((slp) * (t))
  ro <- acf(xt, lag.max = 1, plot = FALSE)$acf[-1]
  a = 1:(length(xt) - 1)
  b = 2:(length(xt))
  xp <- (xt[b] - (xt[a] * ro))
  l <- length(xp)
  q = 1:l
  tf <- (xp[1:l] + ((slp) * (q)))
  plot(tf)
  
  # start pettitt test #
  na.fail(tf)
  n <- length(tf)
  DNAME <- deparse(substitute(tf))
  k <- 1:n
  r <- rank(tf)
  Uk <- sapply(k, function(tf) 2 * sum(r[1:tf]) - tf * (n + 1))
  Uka <- abs(Uk)
  U <- max(Uka)
  K <- k[Uka == U]
  pval <- 2 * exp((-6 * U^2)/(n^3 + n^2))
  if (is.ts(tf)) {
    fr <- frequency(tf)
    st <- start(tf)
    ed <- end(tf)
    Uk <- ts(Uk, start = st, end = ed, frequency = fr)
  }
  attr(Uk, "nm") <- "Uk"
  names(K) <- "probable change point at time K"
  retval <- list(nobs = n, statistic = c(`U*` = U), estimate = K, 
                 p.value = pval, data.name = DNAME, alternative = "two.sided", 
                 data = Uk, method = "Pettitt's test for single change-point detection")
  class(retval) <- c("htest", "cptest")
  return(retval)
}

