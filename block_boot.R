set.seed(42)
d <- read.csv("data/Sales_Volume_Chart_data.csv", check.names=FALSE)
names(d)[1] <- "Year"
d <- d[!is.na(d$`Value (Actual)`), c("Year","Format","Value (Actual)")]
names(d)[3] <- "Units"
B <- 5000
nw_se <- function(X, e, L) { # Newey-West (Bartlett) SE for slope
  n <- nrow(X); XtXi <- solve(crossprod(X)); u <- X*e
  S <- crossprod(u)
  for (l in 1:L) { w <- 1-l/(L+1); G <- crossprod(u[(l+1):n,,drop=FALSE], u[1:(n-l),,drop=FALSE]); S <- S + w*(G+t(G)) }
  sqrt(diag(XtXi %*% S %*% XtXi))[2]
}
mbb_resid <- function(fit, y, x, L) { # circular moving-block bootstrap of residuals
  n <- length(y); r <- resid(fit); f <- fitted(fit); nb <- ceiling(n/L)
  replicate(B, { s <- sample(n, nb, TRUE); idx <- as.vector(sapply(s, function(i) ((i-1+0:(L-1)) %% n)+1))[1:n]
    coef(lm(f + r[idx] ~ x))[2] })
}
out <- list()
for (f in sort(unique(d$Format))) {
  z <- d[d$Format==f & d$Units>0, ]; z <- z[order(z$Year),]
  if (nrow(z) < 8) { out[[f]] <- data.frame(Format=f, note="n<8"); next }
  pk <- z$Year[which.max(z$Units)]; last <- max(z$Year)
  a <- z[z$Year>=pk,]; trough <- a$Year[which.min(a$Units)]
  regime <- "peak->end"; w <- a
  if (nrow(a)>=8 && a$Units[nrow(a)] > 1.5*min(a$Units) && last-trough >= 8) { w <- z[z$Year>=trough,]; regime <- "trough->end" }
  if (nrow(w) < 8) { out[[f]] <- data.frame(Format=f, note="regime n<8"); next }
  x <- w$Year; y <- log(w$Units); n <- length(y)
  fit <- lm(y ~ x); X <- model.matrix(fit); e <- resid(fit)
  b <- coef(fit)[2]; se_ols <- summary(fit)$coef[2,2]
  rho <- cor(e[-1], e[-n])
  Lb <- max(3, round(n^(1/3)*2)); Ln <- max(2, floor(4*(n/100)^(2/9)*3))
  se_nw <- nw_se(X, e, min(Ln, n-2))
  pairs <- replicate(B, { i <- sample(n, n, TRUE); if (length(unique(x[i]))<3) NA else coef(lm(y[i]~x[i]))[2] })
  blk <- mbb_resid(fit, y, x, Lb)
  g <- function(v) 100*(exp(v)-1)
  q <- function(v) g(quantile(v, c(.025,.975), na.rm=TRUE))
  ci_ols <- g(confint(fit)[2,]); ci_nw <- g(b + c(-1,1)*qt(.975,n-2)*se_nw)
  ci_p <- q(pairs); ci_b <- q(blk)
  out[[f]] <- data.frame(Format=f, regime=regime, from=min(x), to=max(x), n=n, pct_per_yr=round(g(b),1), rho_resid=round(rho,2), block_len=Lb,
    OLS_lo=round(ci_ols[1],1), OLS_hi=round(ci_ols[2],1), NW_lo=round(ci_nw[1],1), NW_hi=round(ci_nw[2],1),
    pairs_lo=round(ci_p[1],1), pairs_hi=round(ci_p[2],1), block_lo=round(ci_b[1],1), block_hi=round(ci_b[2],1),
    width_ratio_block_vs_OLS=round(diff(ci_b)/diff(ci_ols),2),
    sign_robust=( (ci_b[1]>0)|(ci_b[2]<0) ), note="")
}
ok <- Filter(function(r) "pct_per_yr" %in% names(r), out)
res <- do.call(rbind, ok)
print(res[,c("Format","regime","from","to","n","pct_per_yr","rho_resid","OLS_lo","OLS_hi","block_lo","block_hi","NW_lo","NW_hi","width_ratio_block_vs_OLS","sign_robust")], row.names=FALSE)
skipped <- Filter(function(r) !"pct_per_yr" %in% names(r), out); print(do.call(rbind, skipped))
write.csv(res, "results/block_boot_results.csv", row.names=FALSE)
cat("\nMedian width ratio block/OLS:", median(res$width_ratio_block_vs_OLS), "\n")
cat("Sign robust under block bootstrap:", sum(res$sign_robust), "of", nrow(res), "\n")
