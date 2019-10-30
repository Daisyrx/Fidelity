
pacman::p_load(tidyverse,magrittr,tibbletime)

FHKCX <- read.csv("FHKCX.csv")
View(FHKCX)
# FHKCX_close <- FHKCX %>% select(Date,Close)
# View(FHKCX_close)

MSCI <- read.csv("MSCI.csv")
View(MSCI)

FHKCX$Date <- as.Date(FHKCX$Date, "%Y-%m-%d")
MSCI$Date <- as.Date(MSCI$Date,"%b %d, %Y")

data <- merge(FHKCX,MSCI,by="Date")
View(data)

# Take a look at the ggplot of the fund and the index
ggplot(data) + geom_line(aes(data$Date,data$Close),stat = "identity",size = 0.5)
ggplot(data) + geom_line(aes(data$Date,data$Price),stat = "identity",size = 0.5, colour = "grey")

# Normalize with roling
rolling_mean <- rollify(mean, window = 5)
rolling_sd <- rollify(sd, window = 5)
data %<>% mutate(rollingz_FHKCX = (Close - rolling_mean(Close))/rolling_sd(Close))
data %<>% mutate(rollingz_MSCI = (Price - rolling_mean(Price))/rolling_sd(Price))

# moving z score
ggplot(data) + geom_line(aes(data$Date,data$rollingz_FHKCX),stat = "identity",size = 0.5) +
  geom_line(aes(data$Date,data$rollingz_MSCI),stat = "identity",size = 0.5, colour = "grey") +
  xlab("Date") + 
  ylab("Price")

# take a look at year 2018
ggplot() + geom_line(aes(data$Date[1008:1258],data$rollingz_FHKCX[1008:1258]),stat = "identity",size = 0.5) +
  geom_line(aes(data$Date[1008:1258],data$rollingz_MSCI[1008:1258]),stat = "identity",size = 0.5, colour = "grey") +
  xlab("Date") + 
  ylab("Price")

# Normalize without rolling
z.FHKCX <- (data$Close - mean(data$Close))/sd(data$Close)
z.MSCI <- (data$Price - mean(data$Price))/sd(data$Price)

ggplot(data) + geom_line(aes(Date,z.FHKCX),stat = "identity",size = 0.5) +
  geom_line(aes(Date,z.MSCI),stat = "identity",size = 0.5, colour = "grey") +
  xlab("Date") + 
  ylab("Price")




