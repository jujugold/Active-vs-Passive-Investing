library(readr)
SPXESG <- read_csv("SPXESG.csv")
View(SPXESG)

data <- SPXESG
remove(SPXESG)

library(stats)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(systemfit) #for seemingly unrelated regression
library(multcomp)  #for hypothesis tests of models coefficients


summary(data)
original <- data
data$Date <- as.Date(data$Date,"%m/%d/%Y")

summary(data)


typeof(data$Date)
plot(data[data$Security=="SPXESG",]$Date,data[data$Security=="SPXESG",]$Close)


## making the graphs
data %>%
  ggplot( aes(x=Date, y=Close, group=Security, color=Security)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Closing price of SPXESG,
          SPY, and VIX") +
  theme_ipsum() +
  ylab("Closing price $") +
  xlab("Year")

library(readxl)
Buffett_bet <- read_excel("Buffett bet.xlsx")
View(Buffett_bet)

Buffett_bet %>%
  ggplot( aes(x=Year, y=Return, group=Fund, color=Fund)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = scales::pretty_breaks(n = 5))+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ggtitle("Returns of S&P 500 vs 5 Fund-of-Funds") +
  theme_ipsum() +
  ylab("Return") +
  xlab("Year")




#regressions 
ols <- lm(data[data$Security=="SPXESG",]$Close ~ data[data$Security=="VIX",]$Close + data[data$Security=="SPY",]$Close)
summary(ols)

ols. <- lm(data[data$Security=="SPY",]$Close ~ data[data$Security=="VIX",]$Close)

ols1 <- data[data$Security=="SPXESG",]$Close ~ data[data$Security=="VIX",]$Close

ols2 <- data[data$Security=="SPY",]$Close ~ data[data$Security=="VIX",]$Close

ols1high <- lm(data[data$Security=="SPXESG",]$High ~ data[data$Security=="VIX",]$High)

ols2high <- lm(data[data$Security=="SPY",]$High ~ data[data$Security=="VIX",]$High)

ols1high <- lm(data[data$Security=="SPXESG",]$High ~ data[data$Security=="VIX",]$High)

daily.range <- data$High - data$Low

data$daily.range <- daily.range

ols1range <- lm(data[data$Security=="SPXESG",]$daily.range ~ data[data$Security=="VIX",]$daily.range)

ols2range <- lm(data[data$Security=="SPY",]$daily.range ~ data[data$Security=="VIX",]$daily.range)

cor(data$Close[data$Security=="SPXESG"],data$Close[data$Security=="SPY"])

summary(ols1)
summary(ols2)


#proving significant difference
system <- list( ESGols1 = ols1, SPYols2 = ols2 )

fitsur <- systemfit(system,  method="SUR", data = data)

summary(fitsur)

olscompare <- glht(fitsur,linfct = c('ESGols1_Close - SPYols2_Close = 0'))


summary(ols1high)
summary(ols1range)
summary(ols2range)


summary(ols2high)

#CAGR 

CAGR <- function(beginning, ending, time) {
  values <- ((ending/beginning)**(1/time) - 1)
  return(values)
}

#found locations of start and end of  data
CAGR(data[1,]$Close,data[645,]$Close,2.56)
CAGR(data[646,]$Close,data[1290,]$Close,2.56)

CAGR(data[1,]$Close,data[321,]$Close,2.56)
CAGR(data[646,]$Close,data[967,]$Close,2.56)

data[646,]$Close
data[1290,]$Close



