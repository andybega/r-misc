library("ggplot2")
library("dplyr")

plot_missing <- function(data, space, time, var, time_unit = "month", checkSS=FALSE) {
  # space = country, province name or ID
  # time  = dates
  if (!is.factor(data[, space])) data[, space] <- as.factor(data[, space])
  space_range <- unique(data[, space])
  time_range  <- seq.Date(from = min(data[, time]),
                          to   = max(data[, time]),
                          by   = time_unit
                          )
  full_mat <- expand.grid(space_range, time_range)
  colnames(full_mat) <- c(space, time)
  data[, "z"] <- !complete.cases(data[, var])
  data <- data[, c(space, time, "z")]
  full_mat <- dplyr::left_join(full_mat, data, by = c(space, time))
  full_mat$z[full_mat$z==TRUE]  <- "Mssng. values"
  full_mat$z[full_mat$z==FALSE] <- "Complete" 
  full_mat$z[is.na(full_mat$z)] <- "No obs."
  full_mat$z <- factor(full_mat$z, levels = c("Complete", "Mssng. values", "No obs."))

  if (checkSS) {
    # set ss type GW or COW
    target <- state_panel(min(full_mat$date), max(full_mat$date), by = time_unit, useGW = FALSE)
    target$id <- NULL
    target$.ss <- 1
    target$cowcode <- as.integer(as.character(target$cowcode))
    colnames(target) <- c(space, time, ".ss")
    full_mat[, space] <- as.integer(as.character(full_mat[, space]))
    full_mat <- left_join(full_mat, target)
    full_mat$.ss <- ifelse(is.na(full_mat$.ss), "out ss", "in ss")
    full_mat$.ssmssng <- paste(full_mat$z, full_mat$.ss, sep = " ")
    lvls <- as.vector(outer(c("Complete", "Mssng. values", "No obs."), c("in ss", "out ss"), paste, sep = " "))
    full_mat$.ssmssng <- factor(full_mat$.ssmssng, levels = lvls)
    full_mat[, space] <- factor(full_mat[, space], levels = rev(sort(unique(full_mat[, space]))))

    p <- ggplot(full_mat, aes_string(x = time, y = space, fill = ".ssmssng")) +
      geom_tile() + 
      scale_x_date(expand=c(0, 0)) +
      scale_fill_manual("", drop = FALSE, 
        values = c("Complete in ss" = hcl(195, l=65, c=100), "Complete out ss" = hcl(15, l=20, c=100),
                   "Mssng. values in ss"  = hcl(15, l=65, c=100), "Mssng. values out ss"  = hcl(80, l=85, c=100),
                   "No obs. in ss"   = hcl(15, l=85, c=100), "No obs. out ss"   = hcl(15, l=97, c=0))) +
      guides(fill = guide_legend(ncol = 2)) + theme(legend.position = "bottom")
  } else {
      p <- ggplot(full_mat, aes_string(x = time, y = space, fill = "z")) + 
    geom_tile() +
    scale_fill_manual("", values = c("Complete" = hcl(195, l=65, c=100), 
                                     "Missing"  = hcl(15, l=65, c=100), 
                                     "Absent"   = hcl(15, l=97, c=0))) +
    scale_x_date(expand=c(0, 0))
  }
  p
}


test_df <- data.frame(
  ccode=c(rep(1, 3), rep(2, 4)), 
  year=c(rep(seq(2000, 2002), 2), 2003),
  x1=c(1,1,NA,1,NA,1,1)
  )
test_df$year <- as.Date(paste0(test_df$year, "-06-15"))

plot_missing(test_df, "ccode", "year", "x1", "year") + theme_bw()
