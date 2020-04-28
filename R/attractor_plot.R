
# === Preamble ======================================================

## Load packages
library(Rcpp)
library(tidyverse)


## Make C++ function for speed
cppFunction(
    'DataFrame createTrajectory(int n, double x0, double y0,
    double a, double b, double c, double d) {
    // create the columns
    NumericVector x(n);
    NumericVector y(n);
    x[0]=x0;
    y[0]=y0;
    for(int i = 1; i < n; ++i) {
    x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
    y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
    }
    // return a new data frame
    return DataFrame::create(_["x"]= x, _["y"]= y);
    }
    '
)


## Set parameters
points <- 20e6
a <- -1.5
b <- 1.5
c <- 1.5
d <- 1.56



# === Data frame creation ===========================================

## Populate data frame
traj_df <- createTrajectory(points, 0, 0, a, b, c, d)


## Plot trajectories
tmp <- traj_df %>%
    ggplot() +
    aes(x, y) +
    geom_point(color="white", shape=46, alpha=.01) +
    theme_void() +
    theme(panel.background = element_rect(fill = "black"))

ggsave(
    plot = last_plot(),
    filename = paste0("traj_plot_", Sys.Date(), ".png"),
    device = "png",
    width = 10,
    height = 10,
    units = "in",
    dpi = 500
)





