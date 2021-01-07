library(xts)
library(zoo)
library(tidyverse)
library(ggplot2)
library(stringr)
data <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-total-female-births.csv",
								sep=",", header=T)

data$Date <- as.Date(data$Date)
data$Label <- format(data$Date, "%b %d")
data$month <- as.factor(months(data$Date))

empty_btw <- 2
to_add_btw <- data.frame(matrix(NA, empty_btw*nlevels(data$month), ncol(data)) )
colnames(to_add_btw) <- colnames(data)
to_add_btw$month <- rep(levels(data$month), each=empty_btw)
data <- rbind(data, to_add_btw)
data$month <- factor(data$month, levels=c("January", "February", "March", "April", "May", "June",
											 "July", "August", "September", "October", "November", "December"))
data <- data %>% mutate(month_id = as.integer(month))
data <- data %>% arrange(month_id) %>% select(-month_id)
data$id <- seq(1, nrow(data))

empty_bar <- 10
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))
ymin <- 20

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- data
text_start <- base_data %>% 
	group_by(month) %>% 
	summarize(head= head(id, 1)) 

angle_label <- data.frame(angle = c(0, -30, -60, -90, 60, 30, 0, -30, -55 , -85, 65, 35),
													hjust = c(rep(0, 4), rep(1, 2), rep(1, 4), rep(0, 2)),
													month = c("January", "February", "March", "April", "May", "June",
																		"July", "August", "September", "October", "November", "December"))
text_start <- text_start %>% inner_join(angle_label, by = "month")

ends <- 388
points <- data.frame(id = c(rep(1, 11), rep(ends, 11)), 
										 value = rep(c(25, 30, 35, 40, 45, 50,
										 					55, 60, 65, 70, 75), 2))

p1<- ggplot(data, aes(x=factor(id), y=Births))+ 
	geom_point(size=0.1, color="#ee86a0")+
	geom_segment(aes(x=id, xend=id, y=ymin, yend=Births), alpha=0.2, size=0.1)+
	ylim(c(0, 80)) +
	theme_void()+
	theme(axis.text = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(),
				panel.grid = element_blank())+
	xlab("")+
	ylab("")


p2 <- p1 + geom_segment(aes(x=1, y= 25, xend = ends, yend = 25), 
												colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 24, xend = ends, yend = 24), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 30, xend = ends, yend = 30), 
							 colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 29, xend = ends, yend = 29), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 28, xend = ends, yend = 28), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 35, xend = ends, yend = 35), 
							 colour = "grey", alpha=0.05, size=0.1 , linetype="dotted")+
	geom_segment(aes(x=1, y= 34, xend = ends, yend = 34), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 33, xend = ends, yend = 33), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 40, xend = ends, yend = 40), 
							 colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 39, xend = ends, yend = 39), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 38, xend = ends, yend = 38), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 45, xend = ends, yend = 45), 
							 colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 44, xend = ends, yend = 44), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 43, xend = ends, yend = 43), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 60, xend = ends, yend = 60), 
							 colour = "grey", alpha=0.05, size=0.05, linetype="dotted")+
	geom_segment(aes(x=1, y= 59, xend = ends, yend = 59), 
							 colour = "grey", alpha=0.04, size=0.05, linetype="dotted")+
	geom_segment(aes(x=1, y= 58, xend = ends, yend = 58), 
							 colour = "grey", alpha=0.03, size=0.05, linetype="dotted") +
	
	geom_segment(aes(x=1, y= 70, xend = ends, yend = 70), 
							 colour = "grey", alpha=0.05, size=0.05, linetype="dotted")+
	geom_segment(aes(x=1, y= 69, xend = ends, yend = 69), 
							 colour = "grey", alpha=0.04, size=0.05, linetype="dotted")+
	geom_segment(aes(x=1, y= 68, xend = ends, yend = 68), 
							 colour = "grey", alpha=0.03, size=0.05, linetype="dotted") +
	
	geom_segment(aes(x=1, y= 75, xend = ends, yend = 75), 
							 colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 74, xend = ends, yend = 74), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 73, xend = ends, yend = 73), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 65, xend = ends, yend = 65), 
							 colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 64, xend = ends, yend = 64), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 63, xend = ends, yend = 63), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")+
	
	geom_segment(aes(x=1, y= 50, xend = ends, yend = 50), 
							 colour = "grey", alpha=0.05, size=0.05, linetype="dotted")+
	geom_segment(aes(x=1, y= 49, xend = ends, yend = 49), 
							 colour = "grey", alpha=0.04, size=0.05, linetype="dotted")+
	geom_segment(aes(x=1, y= 48, xend = ends, yend = 48), 
							 colour = "grey", alpha=0.03, size=0.05, linetype="dotted") +
	
	geom_segment(aes(x=1, y= 55, xend = ends, yend = 55), 
							 colour = "grey", alpha=0.05, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 54, xend = ends, yend = 54), 
							 colour = "grey", alpha=0.04, size=0.1, linetype="dotted")+
	geom_segment(aes(x=1, y= 53, xend = ends, yend = 53), 
							 colour = "grey", alpha=0.03, size=0.1, linetype="dotted")
	
p3 <- p2 + coord_polar() +
	geom_text(data=label_data, aes(x=id, y=80, label=Label, hjust=hjust), 
						color="black", fontface="bold",alpha=0.6, 
						size=2, angle=label_data$angle, inherit.aes = FALSE) +
	geom_point(data = points, aes(x=id, y=value), fill=NA, alpha=0.05, size=0.1)

p4 <- p3 + geom_text(data = text_start, aes(x= head, y=78, label=str_to_upper(month), angle=angle,
						hjust = hjust), colour = "black", alpha=0.8, size=5, fontface="bold") +
	annotate("text", x = rep(394,11), y = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75), 
					 label = c("25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75") , color="grey", size=3 , 
					 angle=4, fontface="bold", hjust=0.5)
	

ggsave("donutplot.png", plot= p4, path = path, width=50, height=50, units="cm", dpi=500, limitsize=F)


