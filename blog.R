library(tidyverse)
library(forestplot)
library(readxl)




# # simulation data
# simulation <- tibble(rnorm(20, 8, 1), rnorm(20, 6, 2),
#      rnorm(20, 7, 1.5), rnorm(20, 9, 3),
#      rnorm(20, 0.3, 0.03), rnorm(20, 0.3, 0.1),
#      rnorm(20, 0.03, 0.02), rnorm(20, 0.041, 0.01),
#      rnorm(20, 0.15, 0.07), rnorm(20, 0.12, 0.06))
# for (i in seq_along(simulation)) {
#   simulation[[i]]
#   simulation[[i]] <- abs(simulation[[i]])
#   print(simulation[[i]])
# }
# write.csv(simulation, "222.csv")


# reference data
ref <- tibble(
  V = 7.769933759,
  V2 = 9.583607374,
  CL = 0.250863168,
  CL2	= 0.041241008,
  CL3	= 0.137454759
)


# dataset
df <- read_xlsx("simulation.xlsx")
df

# standadization
df_CI <- tibble(mean = NA, lower = NA, upper = NA)
for (i in seq_along(df)) {
  if (i <= 2) {
    df_CI <- rbind(df_CI, c(t.test(df[[i]])[[5]], t.test(df[[i]])[[4]][[1]], t.test(df[[i]])[[4]][[2]]) / ref$V)
  } else if (i <= 4) {
    df_CI <- rbind(df_CI, c(t.test(df[[i]])[[5]], t.test(df[[i]])[[4]][[1]], t.test(df[[i]])[[4]][[2]]) / ref$V2)
  } else if (i <= 6) {
    df_CI <- rbind(df_CI, c(t.test(df[[i]])[[5]], t.test(df[[i]])[[4]][[1]], t.test(df[[i]])[[4]][[2]]) / ref$CL)
  } else if (i <= 8) {
    df_CI <- rbind(df_CI, c(t.test(df[[i]])[[5]], t.test(df[[i]])[[4]][[1]], t.test(df[[i]])[[4]][[2]]) / ref$CL2)
  } else {
    df_CI <- rbind(df_CI, c(t.test(df[[i]])[[5]], t.test(df[[i]])[[4]][[1]], t.test(df[[i]])[[4]][[2]]) / ref$CL3)
  }
}
df_CI <- filter(df_CI, !is.na(mean))
df_CI

# for row names
rownames <- cbind(c(NA , names(df)),
                  c("Effect", paste(trunc(df_CI$mean * 100, 2), "%", sep = "")),
                  c("95% CI", paste("(", trunc(df_CI$lower * 100, 2), "%-", trunc(df_CI$upper * 100, 1), "%)", sep = "")))
test_data <- rbind(rep(NA, 3), df_CI)
forestplot(rownames,
           test_data[,c("mean", "lower", "upper")],
           zero = 1,
           grid = structure(c(0.8, 1.2), gp = gpar(col = "steelblue", lty = 2)),
           clip = c(0.5, 1.25),
           xticks = c(0.5, 0.8, 1, 1.2, 1.5),
           txt_gp = fpTxtGp(ticks = gpar(cex = 1), xlab = gpar(cex = 1.5), cex = 1.2), # configure fontsize
           boxsize = 0.3,
           graph.pos = 2, # position of the plot
           col=fpColors(box = "blue"),
           line.margin = .1,
           fn.ci_norm = fpDrawCircleCI,
           lty.ci = 7,   # Confidential Intervals
           lwd.ci = 3,
           ci.vertices.height = 0.15)





?forestplot
