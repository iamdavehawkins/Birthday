# TODO: Add comment
# 
# Author: iamdavehawkins
###############################################################################

library(ggplot2)
library(reshape2)
library(scales)

# Read in csv [Count, Year, Year, ...]
bd <- read.csv("./birthday.csv")

# Melt data so Year becomes variable
mbd <- melt(bd, id = "Count")

theme_set(theme_bw(15))

#Cumulative Plot
png('lines.png')
ggplot(mbd, aes(x = strptime(value, format = "%H:%M"), 
						y = Count, 
						color = variable))+
		geom_line(size=1)+
		ylab("# of wall posts")+
		xlab("Time")+
		ggtitle("Facebook Birthday Wall Posts by Time of Day")+
		scale_x_datetime(labels = date_format("%I:%M %p"),
				breaks = date_breaks("4 hours"))+
		theme(legend.position = c(.15, .85),
				legend.title = element_blank())+
		scale_color_manual(values = c("Blue","Green","Orange","Purple","Red"))+
		ylim(0,100)
dev.off()

# Make Histogram
mbd$hour <- as.character(format(strptime(mbd$value, format = "%H:%M"), "%H"))
freq <- aggregate(Count ~ variable + hour, data = mbd, FUN=length)

png("histogram.png")
ggplot(freq, aes(x=hour, y=Count, fill=variable, color=variable))+
		geom_bar(stat="identity", position="dodge")+
		facet_grid(variable~.)
dev.off()

# Plot data as points and a 3rd-order polynomial smoother
png("curvefit.png")
ggplot(mbd, aes(x = strptime(value, format = "%H:%M"), 
						y = Count, 
						color = variable))+
		geom_point(size=1)+
		stat_smooth(formula = y ~ poly(x, 3), method=lm,
				se=FALSE)+
		ylab("# of wall posts")+
		xlab("Time")+
		ggtitle("Facebook Birthday Wall Posts by Time of Day")+
		scale_x_datetime(labels = date_format("%I:%M %p"),
				breaks = date_breaks("4 hours"))+
		theme(legend.position = c(.15, .85),
				legend.title = element_blank())+
		scale_color_manual(values = c("Blue","Green","Orange","Purple","Red"))+
		ylim(0,100)
dev.off()

# Multiple windows
png("facets.png")
ggplot(mbd, aes(x = strptime(value, format = "%H:%M"), 
						y = Count))+
		geom_point(size = 1)+
		stat_smooth(formula = y ~ poly(x, 3), method=lm,
				se=FALSE)+
		ylab("# of wall posts")+
		xlab("Time")+
		ggtitle("Birthday Wall Posts by Time of Day\n3rd-order polyfit")+
		scale_x_datetime(labels = date_format("%I:%M %p"),
				breaks = date_breaks("15 hours"))+
		theme(legend.position = c(.15, .85),
				legend.title = element_blank())+
		facet_grid(.~variable)
dev.off()

