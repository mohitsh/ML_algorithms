load('psub.RData')
dtrain <- subset(psub,ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)

# input variable age, sex, class of worker,  level of education
# output is Personal income

# Reference Levels
# sex: Male
# class of worker: Employee of a private for-profit 
# level of education: no high school diploma
model <- lm(log(PINCP, base=10) ~ AGEP+SEX+COW+SCHL, data = dtrain)


dtest$predLogPINCP <- predict(model, newdata = dtest)
dtrain$predLogPINCP <- predict(model, newdata = dtrain)

# line of perfect prediction
g <- ggplot(data=dtest, aes(x=predLogPINCP, y=log(PINCP, base = 10)))
g <- g + geom_point(alpha = 0.2, color='black')
g <- g + geom_smooth(aes(x=predLogPINCP, y=log(PINCP, base = 10)))
g <- g + geom_line(aes(x=log(PINCP, base = 10), y = log(PINCP, base = 10)), color='blue', linetype = 2)
g <- g + scale_x_continuous(limits=c(4,5))
g <- g + scale_y_continuous(limits=c(3.5,5.5))


# Residuals income = f(predicted log outcome)

g1 <- ggplot(data=dtest, aes(x=predLogPINCP, y=predLogPINCP-log(PINCP, base = 10)))
g1 <- g1 + geom_point(alpha = 0.2, color = 'black')
g1 <- g1 + geom_smooth(aes(x = predLogPINCP, y = predLogPINCP - log(PINCP, base = 10)),
                       color = 'black')


g1








