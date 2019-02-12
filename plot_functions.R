## Plot example


par(pty = "s", mar = c(0.5, 0.5, 0.5, 0.5))
plot.new()
plot.window(
    xlim = c(-10, 10),
    ylim = c(-10, 10)
)
# abline(h = 0, v = 0, col = "grey")


for (i in seq_len(4)) {
    axis(
        side = i, 
        col = "grey", 
        labels = FALSE, 
        at = seq(-10, 10), 
        lwd = 0.5)
    
}