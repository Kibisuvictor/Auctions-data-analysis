## import data
library(tidyverse)
auctions <- read.csv("auction_data.csv")
head(auctions)
str(auctions)

### question 1: Descriptive statistics
summary(auctions)
table(auctions$d_donation)
table(auctions$saturday)

### correlogram
library(corrplot)
M <-cor(auctions, use = "complete.obs")
M
## plot the correlogram
corrplot(M)

## question 2
### regress price on saturday and donation
f1 <- lm(price~saturday+d_donation+ saturday*d_donation, data = auctions)
summary(f1)

## question 3 adding opening price
df1 <- auctions
df1$o_price_squared <- auctions$opening_price^2
f2 <- lm(price~saturday+d_donation+opening_price+o_price_squared+ saturday*d_donation, data = df1)
summary(f2)

### question 4
library(jtools)
library(marginaleffects)
## plot predicted values based on opening price
effect_plot(f2, pred = opening_price)
## marginal effects
# meplot()
library(sjPlot)
plot_model(f2, type = "pred", terms = "opening_price")

## question 5
f3 <- lm(price~d_donation+opening_price+ shipping_fee+ saturday*d_donation, data = auctions)
summary(f3)

## question 6
auctions %>% ggplot(aes(price))+
  geom_histogram(fill = "coral")+
  labs(title = "Distribution of price")

auctions %>% ggplot(aes(log(price)))+
  geom_histogram(fill = "coral")+
  labs(title = "Distribution of natural logarithm of price")

## question 7
f4 <- lm(log(price)~d_donation+log(opening_price)+ log(shipping_fee), data = auctions)
summary(f4)

## changing the inf to NA
df2 <- auctions
df2$ln_price <- log(df2$price)
df2$ln_opening_price <- log(df2$opening_price)
df2$ln_shipping_fee <- log(df2$shipping_fee)

summary(df2)
# replace
df2[df2 == "-Inf"] <- NA
## fit model
f5 <- lm(ln_price ~ ln_opening_price+ln_shipping_fee+d_donation, data = df2)
summary(f5)
