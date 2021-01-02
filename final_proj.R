# Data preparation & preprocessing
## Data Preparation

# URL:<https://www.kaggle.com/residentmario/ramen-ratings>

setwd("E:/Final_proj")
ramen.rating <- read.csv("ramen-ratings.csv",
                         header=TRUE)
head(ramen.rating)

# Processing Data 
data<-ramen.rating[!(ramen.rating$Style=="")&!(ramen.rating$Stars=="Unrated"), ]

# Analysis of categorical Variable

table(data$Style)

s<-data.frame(table(data$Style))
colnames(s)<-c("Style","Freq")

library(plotly)


fig <- plot_ly(s, labels = ~Style, values = ~Freq, type = 'pie')
fig <- fig %>% layout(title = 'Ramen Package-Styles Distribution Under Data',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

# One numerical variable
table(data$Stars)

stf<-data.frame(table(data$Stars))
colnames(stf)<-c("Rate","Freq")

##convert factors to numeric
stf$Rate<-as.numeric(paste(stf$Rate))

##graph for numerical variables
colnames(stf)<-c("Rate","Freq")
p <- plot_ly(stf, x = ~Rate, y = ~Freq,
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2)))
p <- p %>% layout(title = 'Rates Scatter',
                  yaxis = list(zeroline = FALSE),
                  xaxis = list(zeroline = FALSE))
p


# Analysis of Multivariate data 
library(tidyverse)
d<-as_tibble(data)
d.top.ten<-d[!(!is.na(d$Top.Ten) & d$Top.Ten==""), ]
table(d.top.ten$Style,d.top.ten$Top.Ten)

d.top.ten<-data.frame(d.top.ten)
colnames(d.top.ten)<-c("Num","Brand","Variety","Style","Country","Stars","Top.Ten")
p <- plot_ly(d.top.ten, x = ~Style, y = ~Top.Ten) 
p



# Examing the distribution of numerical data
d.stars<-table(data$Stars)
plot(d.stars,xlab="Stars",ylab="Frequencies",main="Distribution of Ramen Stars")
# Left Skewed distribution


# Random sampling of the data: Central Limit Theorem
library(sampling)
data$Stars<-as.numeric(paste(data$Stars))
x <- data$Stars
par(mfrow=c(1,1))
hist(x, prob = TRUE, 
     xlim=c(0,6), ylim = c(0, 4),main="Distribution of Stars",xlab="stars")


par(mfrow = c(3,2))
samples <- 3000
xbar <- numeric(samples)
for (size in c(10, 20, 30, 40, 50, 60)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(x, size, replace = FALSE))
  }
  
  hist(xbar, prob = TRUE, 
       xlim=c(0,6), ylim = c(0, 4),
       main = paste("Sample Size =", size),xlab="Stars")
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}
par(mfrow = c(1,1))


# Sampling Methods 
## simple random sampling

library(sampling)
set.seed(100)
s<-srswor(100,nrow(data))
sample.2<-data[s!=0,]
table(sample.2$Stars)
plot(table(sample.2$Stars),xlab="Stars",ylim=c(0,16),ylab="Frequencies",main="Simple Random Sampling For Ramen Rating")

## Systematic sampling
set.seed(100)

N <- nrow(data)

n <- 70   #sample size=70
k <- ceiling(N / n) 	#take a value upward
r <- sample(k, 1)		#Take one number out of k.
s <- seq(r, by = k, length = n)
sample.1 <- data[s, ]
table(sample.1$Stars)
plot(table(sample.1$Stars),xlab="Stars",ylab="Frequencies",ylim=c(0,16),main="Systematic Random Sampling For Ramen Rating")

