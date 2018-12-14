library(ggplot2)
library(grid)

line1<-rnorm(100,mean=15-seq(1,6,by=.05),sd=1)
line2<-rnorm(100,mean=4+seq(1,21,by=.2),sd=.5)
time<-seq(1,100,by=1)
data<-data.frame(time,line1,line2)

ggplot(data,aes(time)) +
  # The actual lines
  geom_line(aes(y=line1),size=1.6) +
  geom_line(aes(y=line2),size=1.6) +
  theme_bw() +
  
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  scale_x_continuous(minor_breaks=0,breaks=seq(0,100,10),limits=c(0,100)) +
  scale_y_continuous(minor_breaks=0,breaks=seq(0,26,4),limits=c(0,25)) +
  theme(axis.ticks=element_blank()) +
  
  # Dispose of the legend
  theme(legend.position="none") +
  
  # Set title and axis labels, and format these and tick marks
  ggtitle("Some Random Data I Made") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=20)) +
  ylab("Data Label") +
  xlab("Days Since Beginning") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  
  # Big bold line at y=0
  geom_hline(yintercept=0,size=1.2,colour="#535353") +
  
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) +
  annotate("text",x=14.7,y=15.3,label="Line 1",colour="#f8766d") +
  annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")



