# Configura ambiente de trabalho ----
rm(list = ls()); cat("\014")

source(paste0("./R/tsglm.R"))


# importa os pacotes
pkg <- c("lmtest","nortest","MASS","zoo","imputeTS","forecast"); load_packages(pkg)

# Load data ----

sample_data <- read.csv('./data/sample_data.csv')
head(sample_data)
summary(sample_data)

ts_cases <- ts(sample_data$Cases, start = c(2015,1), frequency = 12)
ts_pop <- ts(sample_data$Population, start = c(2015,1), frequency = 12)
ts_rainfall <- ts(sample_data$Rainfall, start = c(2015,1), frequency = 12)
ts_incidence <- (ts_cases/ts_pop)*10^5

# Train/test split ----
n_tot <- length(ts_cases)
n_train <- 48
n_test <- 12

y_train <- head(ts_cases, n_train)
y_test <- tail(ts_incidence, n_test)

xreg_lag <- lag_matrix(y = ts_rainfall, lags = 1, label =  "xreg")
xreg_lag <- head(xreg_lag, n_tot)
xreg_train <- head(xreg_lag, n_train)
xreg_test <- tail(xreg_lag, n_test)

pop_train <- head(ts_pop, n_train)

# Modelling ----
mmod <- tsglm(
  y = y_train,
  lags = 1,
  xreg = xreg_train,
  offset = log(pop_train)
)
# Model diagnosis ----
# Summary
mmod
# Forecast ----
newxreg <- cbind(xreg_test, rep(log(10^5), n_test))
fct_test <- predict.tsglm(object = mmod, h = n_test, xreg = newxreg)
# Accuracy ----
library(forecast)
accuracy(y_test, fct_test)

# Plot ----
library(ggplot2)
data.frame(
  "Date" = rep(seq(as.Date("2019-01-01"), by = "month", along = y_test), 2),
  "Series" = c(rep("Observed", length(y_test)), rep("Forecast", length(fct_test))),
  "Incidence" = c(y_test, fct_test)
) %>%
  ggplot(
    mapping = aes(x = Date, y = Incidence, group = Series, col = Series)
  ) +
  geom_line() +
  labs(
    y = "Incidence (cases/100.000 hab.)",
    x = "Month",
    title = "Dengue incidence in Crato, Brazil, 2019",
    subtitle = "Forecasts from a Generalized linear model with Negative Binomial distribution and rainfall impact") +
  geom_point()
