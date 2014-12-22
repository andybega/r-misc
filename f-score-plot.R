# Plot F2-score values against precision and recall
# Assumes one understands what precision and recall are.

x <- y <- seq(0, 1, 0.05)

df <- expand.grid(x=x, y=y)

# Harmonic mean
df$harm_mean <- with(df, 2 * (x * y) / (x + y))
harm_mean <- matrix(df$harm_mean, ncol=21, nrow=21)

png("~/Desktop/f-score-countour.png", width=512, height=512)
contour(x, y, harm_mean,
	xlab="Recall", ylab="Precision", main=expression(paste(italic(F)[1], "-Score values")))
dev.off()

filled.contour(x, y, harm_mean)

library(ggplot2)

p <- ggplot(data=df, aes(x=x, y=y)) 

p + stat_contour(geom="polygon", aes(z=harm_mean, fill=..level..))

p + stat_contour(aes(z=harm_mean)) + theme_bw()

p + geom_tile(aes(fill = harm_mean)) + stat_contour(aes(z=harm_mean))

# Regular mean for comparison
df$reg_mean <- with(df, (x + y) / 2)
reg_mean <- matrix(df$reg_mean, ncol=21, nrow=21)

contour(x, y, reg_mean)