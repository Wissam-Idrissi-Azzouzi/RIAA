set.seed(2024)
df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- pmax(df$Value..Actual., 0)   # negatives are data revisions, clipped to 0
get_series <- function(fmt) { s <- df[df$Format == fmt, c("Year","Value")]; s[order(s$Year), ] }

aicc <- function(rss, n, k) {           # k = number of mean-function parameters
  K <- k + 1
  if (n - K - 1 <= 0 || rss <= 0) return(NA_real_)
  n * log(rss / n) + 2 * K + 2 * K * (K + 1) / (n - K - 1)
}

# ---------- Bass sales-rate curve (generalised: free launch time t0) ----------
bass_curve <- function(m, p, q, t0, yr) {
  tau <- yr - t0; e <- exp(-(p + q) * tau)
  m * ((p + q)^2 / p) * e / (1 + (q / p) * e)^2
}
fit_bass <- function(yr, v, nstart = 60) {
  y <- sqrt(v); first <- min(yr); total <- sum(v)
  obj <- function(th) {
    f <- bass_curve(exp(th[1]), exp(th[2]), exp(th[3]), first - exp(th[4]), yr)
    if (any(!is.finite(f))) return(1e9)
    sum((y - sqrt(f))^2)
  }
  lower <- c(log(total * 0.5), log(1e-4), log(1e-3), log(0.01))
  upper <- c(log(total * 30), log(1.5),  log(3),    log(25))
  best <- NULL
  for (i in 1:nstart) {
    th0 <- lower + runif(4) * (upper - lower)
    r <- tryCatch(optim(th0, obj, method = "L-BFGS-B", lower = lower, upper = upper), error = function(e) NULL)
    if (!is.null(r) && is.finite(r$value) && (is.null(best) || r$value < best$value)) best <- r
  }
  if (is.null(best)) return(NULL)
  th <- best$par; tol <- 0.02 * (upper - lower)
  list(m = exp(th[1]), p = exp(th[2]), q = exp(th[3]), t0 = first - exp(th[4]), rss = best$value,
       atbound = any(th - lower < tol | upper - th < tol),
       predict = function(yy) bass_curve(exp(th[1]), exp(th[2]), exp(th[3]), first - exp(th[4]), yy))
}

# ---------- Symmetric bell (Gaussian) baseline ----------
fit_gauss <- function(yr, v, nstart = 40) {
  y <- sqrt(v); ymax <- max(v)
  fun <- function(th, yy) exp(th[1]) * exp(-0.5 * ((yy - th[2]) / exp(th[3]))^2)
  obj <- function(th) sum((y - sqrt(fun(th, yr)))^2)
  lower <- c(log(ymax * 0.3), min(yr) - 10, log(0.5)); upper <- c(log(ymax * 3), max(yr) + 10, log(60))
  best <- NULL
  for (i in 1:nstart) {
    th0 <- lower + runif(3) * (upper - lower)
    r <- tryCatch(optim(th0, obj, method = "L-BFGS-B", lower = lower, upper = upper), error = function(e) NULL)
    if (!is.null(r) && is.finite(r$value) && (is.null(best) || r$value < best$value)) best <- r
  }
  th <- best$par
  list(rss = best$value, mu = th[2], sigma = exp(th[3]), predict = function(yy) fun(th, yy))
}

# ---------- Exponential decay to a floor (log-scale fit; floor F in [0, min observed]) ----------
fit_floor <- function(yr, v, nstart = 60) {
  tau <- yr - min(yr); ly <- log(v); vmin <- min(v); vmax <- max(v)
  fun <- function(th, tt) vmin * th[3] + exp(th[1]) * exp(-exp(th[2]) * tt)
  obj <- function(th) { f <- fun(th, tau); if (any(!is.finite(f) | f <= 0)) return(1e9); sum((ly - log(f))^2) }
  lower <- c(log(vmax * 0.2), log(1e-3), 0); upper <- c(log(vmax * 5), log(3), 1)
  best <- NULL
  for (i in 1:nstart) {
    th0 <- lower + runif(3) * (upper - lower)
    r <- tryCatch(optim(th0, obj, method = "L-BFGS-B", lower = lower, upper = upper), error = function(e) NULL)
    if (!is.null(r) && is.finite(r$value) && (is.null(best) || r$value < best$value)) best <- r
  }
  th <- best$par; y0 <- min(yr)
  list(rss = best$value, floor = vmin * th[3], k = exp(th[2]),
       predict = function(yy) fun(th, yy - y0))
}
exp_fit <- function(yr, v) { ok <- v > 0; fit <- lm(log(v[ok]) ~ yr[ok]); a <- coef(fit)[1]; b <- coef(fit)[2]
  list(rss = sum(resid(fit)^2), n = sum(ok), cagr = exp(b) - 1, predict = function(yy) exp(a + b * yy)) }
