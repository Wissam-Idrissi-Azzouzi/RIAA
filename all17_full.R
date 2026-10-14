df <- read.csv("data/Sales_Volume_Chart_data.csv", stringsAsFactors = FALSE)
names(df)[1] <- "Year"
df <- df[!is.na(df$Value..Actual.), ]
df$Value <- df$Value..Actual.

skewness <- function(x){n<-length(x);m<-mean(x);s<-sd(x);(sum((x-m)^3)/n)/s^3}
excess_kurt <- function(x){n<-length(x);m<-mean(x);s<-sd(x);(sum((x-m)^4)/n)/s^4-3}
bp_test <- function(fit,x){e2<-resid(fit)^2; aux<-lm(e2~x); f<-summary(aux)$fstatistic; pf(f[1],f[2],f[3],lower.tail=FALSE)}
reset_test <- function(fit,data){data$fit2<-fitted(fit)^2; aug<-lm(update(formula(fit),.~.+fit2),data=data); anova(fit,aug)[2,"Pr(>F)"]}
iqr_flag <- function(x){q<-quantile(x,c(.25,.75));iqr<-q[2]-q[1];sum(x<q[1]-1.5*iqr | x>q[2]+1.5*iqr)}

formats <- unique(df$Format)
results <- data.frame()

for (fmt in formats) {
  sub_all <- df[df$Format==fmt & df$Value>0, c("Year","Value")]
  sub_all <- sub_all[order(sub_all$Year),]
  sub <- tail(sub_all, 10)   # last 10 recorded years for THIS format
  n <- nrow(sub)
  if (n < 8) {
    results <- rbind(results, data.frame(Format=fmt, n=n, Note="INSUFFICIENT DATA (n<8) - skipped",
      CAGR=NA,R2=NA,SlopeP=NA,DW=NA,Shapiro_p=NA,BP_p=NA,RESET_p=NA,
      MaxCooksD_pct=NA,DFFITS_flags=NA,IQR_resid_flags=NA,SkewRaw=NA,SkewLog=NA,SkewResid=NA))
    next
  }
  fit <- lm(log(Value) ~ Year, data=sub)
  s <- summary(fit)
  res <- resid(fit)
  cagr <- (exp(coef(fit)[2])-1)*100
  r2 <- s$r.squared
  slope_p <- s$coefficients["Year","Pr(>|t|)"]
  dw <- sum(diff(res)^2)/sum(res^2)
  sw_p <- tryCatch(shapiro.test(res)$p.value, error=function(e) NA)
  bp_p <- tryCatch(bp_test(fit, sub$Year), error=function(e) NA)
  reset_p <- tryCatch(reset_test(fit, sub), error=function(e) NA)
  cooksd <- cooks.distance(fit)
  cooks_pct <- max(pf(cooksd, 2, n-2))*100
  dfft <- dffits(fit)
  dfft_flags <- sum(abs(dfft) > 1 | abs(dfft) > 2*sqrt(2/n))
  iqr_flags <- iqr_flag(res)

  results <- rbind(results, data.frame(
    Format=fmt, n=n, Note=paste0(min(sub$Year),"-",max(sub$Year)),
    CAGR=round(cagr,1), R2=round(r2,2), SlopeP=round(slope_p,4),
    DW=round(dw,2), Shapiro_p=round(sw_p,3), BP_p=round(bp_p,3), RESET_p=round(reset_p,3),
    MaxCooksD_pct=round(cooks_pct,1), DFFITS_flags=dfft_flags, IQR_resid_flags=iqr_flags,
    SkewRaw=round(skewness(sub$Value),2), SkewLog=round(skewness(log(sub$Value)),2), SkewResid=round(skewness(res),2)
  ))
}
options(width=200)
print(results, row.names=FALSE)
write.csv(results, "results/all17_results.csv", row.names=FALSE)
