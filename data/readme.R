# some kind of R package that deals with panel data
# associated plot methods that respect two-column identifiers
# understand non-conventional dates, like year, etc. 
# maybe just add add'l attr and otherwise inhert from DF or data table


# to do:
#   - name for package
#   - start with manual functions, then make methods later
#   - plot-missing, make sure all 3 labels show up in plot, always

library("rio")
library("ggplot2")

setwd("~/Work/R_utilities/data")
samb <- import("SambnisImp.csv")

# Argh, imputed GDP per capita values!
df <- subset(samb, year %in% c(1998, 1999, 2000), select = c("cowcode", "year", "ln_gdpen"))
df$gdpen <- exp(df$ln_gdpen)
ggplot(df, aes(x = gdpen)) +
  geom_histogram() + 
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  labs(x = "GDP per capita (F&L 2003)") +
  theme_bw() +
  ggtitle("GDP p.c. for select years; imputation problems?")
ggsave(file="gdppc-by-year-problems.png", heigh = 6, width = 5)


source("plot_missing.R")
source("state-panel.R")

samb$date <- as.Date(paste(samb$year, "06", "30", sep = "-"))

p <- plot_missing(samb, "cowcode", "date", "ln_gdpen", "year") + theme_bw() +
  theme(legend.position = "bottom")

# add state membership info
# look at plot_missing source code for experimental stuff
p 
ggsave(file="SambnisImp-missing-plot2.png", height = 10, width = 8)

p <- plot_missing(samb, "cowcode", "date", "ln_gdpen", "year", checkSS=TRUE) + theme_bw() +
  theme(legend.position = "bottom")
ggsave(file="SambnisImp-missing-plot.png", height = 10, width = 6)


noodles <- function(x, space, time, data) {
	require(ggplot2)
	p <- ggplot(data = data, aes_string(x = time, y = x, group = space)) +
	  geom_line(alpha = 0.3) + theme_bw()
  plot(p)
}

noodles("ln_gdpen", "cowcode", "year", data = samb)
ggsave(file="SambnisImp-lngdpen-noodles.png", height=6, width=6)
