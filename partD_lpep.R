source("R/07_lifecycle_models/common.R")
s <- get_series("LP/EP"); w <- s[s$Year >= 2006 & s$Value > 0, ]; yr <- w$Year; v <- w$Value; n <- length(v); ly <- log(v)
cat("LP/EP recovery window 2006-2024 (n =", n, "). Last 8 years ($M) and YoY growth:\n")
tail8 <- tail(w, 8); tail8$yoy_pct <- round(100 * c(NA, diff(tail8$Value) / head(tail8$Value, -1))[1:8], 1); print(tail8, row.names = FALSE)

# ---- model definitions on the log scale ----
logi_fun <- function(K, r, t0, yy) K / (1 + exp(-r * (yy - t0)))
gomp_fun <- function(K, b, c, yy) K * exp(-b * exp(-c * (yy - 2006)))
fit_generic <- function(objf, lower, upper, nstart = 80) {
  best <- NULL
  for (i in 1:nstart) {
    th0 <- lower + runif(length(lower)) * (upper - lower)
    r <- tryCatch(optim(th0, objf, method = "L-BFGS-B", lower = lower, upper = upper), error = function(e) NULL)
    if (!is.null(r) && is.finite(r$value) && (is.null(best) || r$value < best$value)) best <- r
  }
  best
}
vmax <- max(v)
fit_logi <- function(yy, lly, nstart = 80, Kfix = NULL) {
  if (is.null(Kfix)) {
    obj <- function(th) { f <- logi_fun(exp(th[1]), exp(th[2]), th[3], yy); if (any(!is.finite(f) | f <= 0)) return(1e9); sum((lly - log(f))^2) }
    b <- fit_generic(obj, c(log(vmax * 1.001), log(0.01), 2000), c(log(5000), log(2), 2080), nstart)
    list(rss = b$value, K = exp(b$par[1]), r = exp(b$par[2]), t0 = b$par[3], par = b$par,
         predict = function(y2) logi_fun(exp(b$par[1]), exp(b$par[2]), b$par[3], y2))
  } else {
    obj <- function(th) { f <- logi_fun(Kfix, exp(th[1]), th[2], yy); if (any(!is.finite(f) | f <= 0)) return(1e9); sum((lly - log(f))^2) }
    b <- fit_generic(obj, c(log(0.01), 2000), c(log(2), 2200), nstart)
    list(rss = b$value, K = Kfix, r = exp(b$par[1]), t0 = b$par[2], predict = function(y2) logi_fun(Kfix, exp(b$par[1]), b$par[2], y2))
  }
}
fit_gomp <- function(yy, lly, nstart = 80) {
  obj <- function(th) { f <- gomp_fun(exp(th[1]), exp(th[2]), exp(th[3]), yy); if (any(!is.finite(f) | f <= 0)) return(1e9); sum((lly - log(f))^2) }
  b <- fit_generic(obj, c(log(vmax * 1.001), log(0.1), log(0.005)), c(log(5000), log(30), log(2)), nstart)
  list(rss = b$value, K = exp(b$par[1]), predict = function(y2) gomp_fun(exp(b$par[1]), exp(b$par[2]), exp(b$par[3]), y2))
}

ex <- exp_fit(yr, v); lg <- fit_logi(yr, ly); gp <- fit_gomp(yr, ly)
cmp <- data.frame(Model = c("Exponential (no ceiling)", "Logistic (capped)", "Gompertz (capped)"),
  params = c(2, 3, 3), RSS_log = round(c(ex$rss, lg$rss, gp$rss), 4),
  AICc = round(c(aicc(ex$rss, n, 2), aicc(lg$rss, n, 3), aicc(gp$rss, n, 3)), 1),
  ceiling_K = c(NA, round(lg$K, 1), round(gp$K, 1)),
  proj_2029 = round(c(ex$predict(2029), lg$predict(2029), gp$predict(2029)), 1),
  proj_2034 = round(c(ex$predict(2034), lg$predict(2034), gp$predict(2034)), 1))
cat("\n=== LP/EP: capped-growth vs exponential (fit on 2006-2024, log scale) ===\n"); print(cmp, row.names = FALSE)
cat("Logistic: r =", round(lg$r, 3), " inflection year =", round(lg$t0, 1), "\n")

# ---- Profile over the ceiling K: how well does the data pin the ceiling? ----
Ks <- c(46, 48, 50, 55, 60, 70, 80, 100, 125, 150, 200, 300, 344, 500, 1000, 3000)
prof <- data.frame()
for (K in Ks) { f <- fit_logi(yr, ly, nstart = 30, Kfix = K)
  prof <- rbind(prof, data.frame(K = K, RSS = f$rss, LR = n * log(f$rss / lg$rss), dAICc_vs_best = aicc(f$rss, n, 3) - aicc(lg$rss, n, 3),
                                  proj_2029 = f$predict(2029), proj_2034 = f$predict(2034))) }
prof$inside_95 <- prof$LR <= qchisq(0.95, 1)
prof[, c("RSS","LR","dAICc_vs_best","proj_2029","proj_2034")] <- round(prof[, c("RSS","LR","dAICc_vs_best","proj_2029","proj_2034")], 3)
cat("\n=== Profile of the ceiling K (K fixed, other logistic params refit) ===\n"); print(prof, row.names = FALSE)
cat("inside_95 = ceilings the data cannot rule out at 95% (likelihood-ratio). If it runs up to the largest K tried, the data cannot pin a ceiling.\n")

# ---- Residual bootstrap of the best logistic ----
set.seed(11); B <- 300; fit_hat <- log(lg$predict(yr)); res <- ly - fit_hat
bs <- data.frame(); fails <- 0
for (b in 1:B) {
  ys <- fit_hat + sample(res, n, replace = TRUE)
  f <- tryCatch(fit_logi(yr, ys, nstart = 10), error = function(e) NULL)
  if (is.null(f) || !is.finite(f$rss)) { fails <- fails + 1; next }
  bs <- rbind(bs, data.frame(K = f$K, p29 = f$predict(2029), p34 = f$predict(2034)))
}
q <- function(x) round(quantile(x, c(0.05, 0.5, 0.95)), 1)
cat("\n=== Residual bootstrap of the logistic (", nrow(bs), "successful fits, ", fails, "failed ) ===\n", sep = "")
cat("Ceiling K    5/50/95%:", q(bs$K), "\n2029 value   5/50/95%:", q(bs$p29), "\n2034 value   5/50/95%:", q(bs$p34), "\n")
cat("Share of bootstrap fits with ceiling pushed to the 5000 upper limit:", round(mean(bs$K > 4900), 3), "\n")
