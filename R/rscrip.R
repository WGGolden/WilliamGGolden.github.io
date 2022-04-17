library(MASS)
library(forecast)
library(tidyverse)
library(prophet)
library(reshape2)



# Loading packages

df1 <- read.csv("./Data/Raw/2000-2009.csv")
df2 <- read.csv("./Data/Raw/2010-Present.csv")

# cleaning data

df1 <- df1 %>% 
  select(PRCP,DATE)

df1 <- rename(df1, ds=DATE)

df1 <- rename(df1, y=PRCP)



df2 <- df2 %>% 
  select(PRCP,DATE)

df2 <- rename(df2, ds=DATE)

df2 <- rename(df2, y=PRCP)

df <- rbind(df1,df2)

ggplot(df,aes(x=ds,y=y))+
  geom_line()+
  labs(x="Year",
       y="Recorded Percipitation")

# We thank thee oh god for a prophet

m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

plot(m, forecast)

prophet_plot_components(m, forecast)



