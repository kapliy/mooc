library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

mod <- bats(tstrain)
pred <- forecast(mod)

accuracy(pred, testing$visitsTumblr)
acc = sum(testing$visitsTumblr <= pred$upper) / nrow(testing)
