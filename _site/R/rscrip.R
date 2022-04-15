suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(gmodels))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(Kmisc))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(corrplot))


####

library(MASS)
library(forecast)
library(tidyverse)
library(prophet)
library(reshape2)



# Loading packages

df1 <- read.csv("./Data/Raw/2000-2009.csv")
df2 <- read.csv("./Data/Raw/2010-Present.csv")

# cleaning data

df <- df1 %>% 
  select(PRCP,DATE)

df %>% 
  mutate(DATE=as.Date(df$DATE,"%Y-%m-%d"))

df <- df %>% rename(ds=DATE) %>% 
  rename(value=PRCP)

ggplot(df,aes(x=ds,y=value))+
  geom_line()

# Lambda thingy

df <- df %>% filter(!df$value==0)

lam = BoxCox.lambda(df$value, method = "loglik")

df$y = BoxCox(df$value, lam)

df.m <- melt(df, measure.vars=c("value", "y"))

ggplot(df.m,aes(x=ds,y=value))+
  geom_line()

# We thank thee oh god for a prophet

m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

plot(m, forecast)

prophet_plot_components(m, forecast)

# Revert

inverse_forecast <- forecast
inverse_forecast <- column_to_rownames(inverse_forecast, var = "ds")
inverse_forecast$yhat_untransformed = InvBoxCox(forecast$yhat, lam)


