install.packages("igraph")

library(ggplot2)
library(igraph)
vif <- function(x){1/(1-summary(x)$r.squared)}

data <- read.csv("team_stat.csv")


fit1 <- lm(run~average, data = data)
cor.test(data$average, data$run)
summary(fit1)
plot(x = data$average, y = data$run, main = "타율-득점", xlab = "타율", ylab = "득점")
abline(fit1)

fit2 <- lm(run~obp, data = data)
cor.test(data$obp, data$run)
summary(fit2)
plot(x = data$obp, y = data$run, main = "출루율-득점", xlab = "출루율", ylab = "득점")
abline(fit2)


fit3 <- lm(run~slg, data = data)
cor.test(data$slg, data$run)
summary(fit3)
plot(x = data$slg, y = data$run, main = "장타율-득점", xlab = "장타율", ylab = "득점")
abline(fit3)


fit4 <- lm(run~ops, data = data)
cor.test(data$ops, data$run)
summary(fit4)
plot(x = data$ops, y = data$run, main = "OPS-득점", xlab = "OPS", ylab = "득점")
abline(fit4)


fit5 <- lm(run~woba, data = data)
cor.test(data$woba, data$run)
summary(fit5)
plot(x = data$woba, y = data$run, main = "wOBA-득점", xlab = "wOBA", ylab = "득점")
abline(fit5)


fit6 <- lm(run~wrc, data = data)
cor.test(data$wrc, data$run)
summary(fit6)
plot(x = data$wrc, y = data$run, main = "wRC-득점", xlab = "wRC", ylab = "득점")
abline(fit6)


data$bb2 <- data$bb-data$ibb
fit7 <- lm(run~bb2+hp+X1b_hit+X2b_hit+X3b_hit+hr, data = data)
summary(fit7)
data$new1 <- data$bb2*0.27+data$hp*0.21+data$X1b_hit*0.36+data$X2b_hit*0.59+data$X3b_hit*2.25+data$hr*1.47
fit7.1 <- lm(run~new1, data = data)
summary(fit7.1)
vif(fit7)


data2 <- read.csv("versus.csv")
net <- igraph::graph.data.frame(data2)
plot(net, vertex.size = degree(net, mode = "all")*0.1)


data3 <- read.csv("report.csv")
barplot(data3$cor, data = data3)
ggplot(data = data3, aes(x = stat, y = cor)) +
  geom_bar(stat = 'identity') +
  ggtitle('Comparing Correlation') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(0, 1))

ggplot(data = data3, aes(x = stat, y = r2)) +
  geom_bar(stat = 'identity') +
  ggtitle('Comparing R-squared') +
  theme(plot.title = element_text(hjust = 0.5))
