df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

skewness <- function(x){n<-length(x);m<-mean(x);s<-sd(x);(sum((x-m)^3)/n)/s^3}
bp_test <- function(fit,x){e2<-resid(fit)^2; aux<-lm(e2~x); f<-summary(aux)$fstatistic; pf(f[1],f[2],f[3],lower.tail=FALSE)}
reset_test <- function(fit,data){data$fit2<-fitted(fit)^2; aug<-lm(update(formula(fit),.~.+fit2),data=data); tryCatch(anova(fit,aug)[2,"Pr(>F)"], error=function(e) NA)}
iqr_flag <- function(x){q<-quantile(x,c(.25,.75));iqr<-q[2]-q[1];sum(x<q[1]-1.5*iqr | x>q[2]+1.5*iqr)}

formats <- unique(df$Format)
results <- data.frame()

for (fmt in formats) {
  full <- df[df$Format==fmt & df$Value>0, c("Year","Value")]
  full <- full[order(full$Year),]
  if (nrow(full) < 8) {
    results <- rbind(results, data.frame(Format=fmt, Regime="insufficient data", n=nrow(full),
      Window=NA,CAGR=NA,R2=NA,SlopeP=NA,DW=NA,Shapiro_p=NA,BP_p=NA,RESET_p=NA,MaxCooksD_pct=NA))
    next
  }
  peak_idx <- which.max(full$Value)
  peak_year <- full$Year[peak_idx]; last_year <- max(full$Year)

  if (peak_year == last_year) {
    # never declined - use ALL data
    win <- full; regime <- "still rising (all data)"
  } else {
    after_peak <- full[full$Year > peak_year, ]
    trough_idx <- which.min(after_peak$Value)
    trough_year <- after_peak$Year[trough_idx]; trough_val <- after_peak$Value[trough_idx]
    last_val <- full$Value[full$Year==last_year]
    # genuine recovery = trough is not the last year itself, AND current value meaningfully above trough
    if (trough_year < last_year && last_val > 1.5*trough_val && (last_year - trough_year) >= 6) {
      win <- full[full$Year >= trough_year, ]; regime <- paste0("recovery since trough ", trough_year)
    } else {
      win <- full[full$Year >= peak_year, ]; regime <- paste0("decline since peak ", peak_year)
    }
  }
  n <- nrow(win)
  if (n < 8) {
    results <- rbind(results, data.frame(Format=fmt, Regime=paste(regime,"[n<8, skipped]"), n=n,
      Window=paste0(min(win$Year),"-",max(win$Year)),CAGR=NA,R2=NA,SlopeP=NA,DW=NA,Shapiro_p=NA,BP_p=NA,RESET_p=NA,MaxCooksD_pct=NA))
    next
  }
  fit <- lm(log(Value) ~ Year, data=win)
  s <- summary(fit); res <- resid(fit)
  cagr <- (exp(coef(fit)[2])-1)*100
  slope_p <- s$coefficients["Year","Pr(>|t|)"]
  dw <- sum(diff(res)^2)/sum(res^2)
  sw_p <- tryCatch(shapiro.test(res)$p.value, error=function(e) NA)
  bp_p <- tryCatch(bp_test(fit, win$Year), error=function(e) NA)
  reset_p <- reset_test(fit, win)
  cooksd <- cooks.distance(fit)
  cooks_pct <- max(pf(cooksd, 2, n-2))*100

  results <- rbind(results, data.frame(
    Format=fmt, Regime=regime, n=n, Window=paste0(min(win$Year),"-",max(win$Year)),
    CAGR=round(cagr,1), R2=round(s$r.squared,2), SlopeP=round(slope_p,4),
    DW=round(dw,2), Shapiro_p=round(sw_p,3), BP_p=round(bp_p,3), RESET_p=round(reset_p,3),
    MaxCooksD_pct=round(cooks_pct,1)
  ))
}
options(width=220)
print(results, row.names=FALSE)
write.csv(results, "results/all17_fulldata_results.csv", row.names=FALSE)
