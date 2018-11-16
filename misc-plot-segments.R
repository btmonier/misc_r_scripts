#--------------------------------------------------------------------- 
# Title:         Generate line segments for plots 
# Author:        Brandon Monier 
# Created:       2018-11-13 at 17:57:59 
# Last Modified: 2018-11-13 at 18:01:41
#--------------------------------------------------------------------- 
 
#--------------------------------------------------------------------- 
# Detailed purpose:
#    The main purpose of this script is to better understand how the
#    `segments()` function in R works. This simple script will
#    generate 30 random segments across a square 10 x 10 plot at a
#    min/max of 4.5 to 6.5, respectively. 
#
#    It kind of looks like a bar code...
#--------------------------------------------------------------------- 

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
    
    x_seq <- sample(seq(1, 10, 0.1),size = 30)
    
    # Segment 1
    segments(
        x0 = x_seq, 
        y0 = 4.5, 
        x1 = x_seq, 
        y1 = 6.5
    )
}
