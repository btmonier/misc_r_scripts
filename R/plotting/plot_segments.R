#------------------------------------------------------------------------------
# Title:   Generate Line Segments for Plots
# Author:  Brandon Monier
# Created: 2018-11-13
#
# Learn how the `segments()` function works. This script draws 30 random
# segments across a 10 x 10 plot, from 4.5 to 6.5. It kind of looks like a
# bar code.
#------------------------------------------------------------------------------

for (i in 1:10) {
    par(pty = "s")
    plot(
        x = 1:10,
        y = 1:10,
        pch = "",
        main = paste(
            "Segment Test -", i
        )
    )

    x_seq <- sample(seq(1, 10, 0.1), size = 30)

    # Segment 1
    segments(
        x0 = x_seq,
        y0 = 4.5,
        x1 = x_seq,
        y1 = 6.5
    )
}
