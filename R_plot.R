library(readr)

args <- commandArgs(trailingOnly = T)

out_dir <- toString(args[4])
name_plot <- toString(args[5])
mask <- toString(args[6])
name_df <- toString(args[7])

"%+%" <- function(...){
  paste0(...)
}

LD_txt <- read_csv(name_df)  
  
LD_txt <- LD_txt[order(LD_txt$Len, decreasing = F),] 
x <- LD_txt$Len
y <- LD_txt$D


a <- LD_txt$Len
b <- LD_txt$`R^2`

wind <- as.integer(round(length(x)/50, 0))
if (wind == 0) {wind <- 2}
# name_plot1 <- out_dir %+% "/" %+% name_plot %+% "LD_plot_" %+% "_" %+% mask %+% "_" %+% wind  %+% ".png"
# name_plot2 <- out_dir %+% "/" %+% name_plot %+% "r2_plot_" %+% "_" %+% mask %+% "_" %+% wind  %+% ".png"
# name_plot3 <- out_dir %+% "/" %+% name_plot %+% "LD_hist_" %+% "_" %+% mask %+% "_" %+% wind  %+% ".png"
# name_plot4 <- out_dir %+% "/" %+% name_plot %+% "Len_hist_" %+% "_" %+% mask %+% "_" %+% wind  %+% ".png"
name_plot5 <- name_plot %+% "_Mix_" %+% "_" %+% mask %+% "_" %+% wind  %+% ".png"

# #----------------------------------------------------------------
# png(file= name_plot1, width=1500, height=900, res=120)
# plot(x, y, col=grey(.7),  main = "LD plot of " %+% name_plot %+% ". " %+% "Mask = " %+% mask %+% ". Window = " %+% wind, xlab ="Distance", ylab = "LD", ylim=c(0, max(y)))
# grid()
# f <- rep(1/wind, wind)
# y_lag <- filter(y, f, sides=1)
# lines(x, y_lag, col="red")
# dev.off()
# 
# png(file= name_plot2, width=1500, height=900, res=120)
# plot(a, b, col=grey(.7),  main = "r**2 plot of " %+% name_plot %+% ". " %+% "Mask = " %+% mask %+% ". Window = " %+% wind, xlab ="Distance", ylab = "r**2")
# grid()
# q <- rep(1/wind, wind)
# b_lag <- filter(b, q, sides=1)
# lines(a, b_lag, col="blue")
# #abline(v=1, col="orange", lty=2)
# dev.off()
# 
# png(file= name_plot3, width=1500, height=900, res=120)
# hist(y, breaks = "Sturges", main = "LD histogram of " %+% name_plot %+% ". " %+% "Mask = " %+% mask, xlab ="LD", ylab = "Frequency", pch=16, col = "orange")
# dev.off()
# 
# png(file= name_plot4, width=1500, height=900, res=120)
# hist(x, breaks = "Sturges", main = "Len histogram of " %+% name_plot %+% ". " %+% "Mask = " %+% mask, xlab ="Len", ylab = "Frequency", pch=16, col = "green")
# dev.off()

png(file= name_plot5, width=1700, height=1000, res=120)
par(mfrow=c(2,2))
plot(x, y, col=grey(.7),  main = "D plot", xlab ="Distance", ylab = "D", ylim=c(-0.25, 0.25))

f <- rep(1/wind, wind)
y_lag <- filter(y, f, sides=1)
lines(x, y_lag, col="red")

plot(a, b, col=grey(.7),  main = "R^2 plot", xlab ="Distance", ylab = "R^2", ylim=c(0, 1))

q <- rep(1/wind, wind)
b_lag <- filter(b, q, sides=1)
lines(a, b_lag, col="blue")

hist(y, breaks = "Sturges", main = "D histogram", xlab ="LD", ylab = "Frequency", pch=16, col = "orange")
hist(x, breaks = "Sturges", main = "Len histogram", xlab ="Len", ylab = "Frequency", pch=16, col = "green") 
dev.off()

# #-------------------------------------------------------------------------
# df1 <- data.frame(x, y)
# if (length(df1$x) > 50000) { df1 <- df1[1:50000,]}
# #if (length(df1$x) > 50000) {df1 <- df1[sample(nrow(df1), 50000), ]}
# sink("Kendall_test.txt")
# cor.test(as.integer(df1$y), as.integer(df1$x), method = "kendall")
# sink()
