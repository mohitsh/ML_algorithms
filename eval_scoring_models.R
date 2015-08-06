d <- data.frame(y=(1:10)^2, x=1:10)
model <- lm(y~x, data=d)
d$prediction <- predict(model,newdata = d)

library(ggplot2)

g <- ggplot(data=d) + geom_point(aes(x=x,y=y))
g <- g + geom_line(aes(x=x,y=prediction), color='blue')
g <- g + geom_segment(aes(x=x, y=prediction, yend=y, xend = x))
g <- g + scale_y_continuous('')

