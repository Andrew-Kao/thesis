require(RItools)
library(ggplot2)

data <- read.csv("wuvc_nc_balance_data.csv")
buffer.sizes <- rev(c(50,100,200,300,400,500,750,1000,3000))

# Figure A1

chi.sq <- sapply(14:22, function(x)
  xBalance(treatment ~ voted.2000 + age + gender + democrat + republican + dish + popdens2000 + pcthisp2000, data = subset(data, data[,x] == 1 & district == 1), report = "chisquare.test")$overall)

chi.sq.val <- rev(unlist(chi.sq[1,]))

df <- data.frame(cbind(buffer.sizes, chi.sq.val))

ggplot(df, aes(x = buffer.sizes, y= chi.sq.val)) + geom_point() + geom_smooth(span = 1, se = FALSE) + labs(title = "North Carolina Congressional District 1") + xlab("Buffer Sizes (meters)") + ylab(bquote(chi^2)) + theme_bw() + theme(legend.background = element_rect(fill = "white")) +  theme(plot.title = element_text(hjust = 0.5)) 

# Figure A3

chi.sq <- sapply(14:22, function(x)
  xBalance(treatment ~ voted.2000 + age + gender + democrat + republican + dish + popdens2000 + pcthisp2000, 
           strata = list(nostrat=NULL), data = subset(data, data[,x] == 1 & district == 3), report = "chisquare.test")$overall); chi.sq.val <- rev(unlist(chi.sq[1,]))

df <- data.frame(cbind(buffer.sizes, chi.sq.val))

ggplot(df, aes(x = buffer.sizes, y= chi.sq.val)) + geom_point() + geom_smooth(span = 1, se = FALSE) + labs(title = "North Carolina Congressional District 3") + xlab("Buffer Sizes (meters)") + ylab(bquote(chi^2)) + theme_bw() + theme(legend.background = element_rect(fill = "white")) +  theme(plot.title = element_text(hjust = 0.5)) 

