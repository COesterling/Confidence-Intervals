download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")
head(nc)

m <- na.omit(nc$mage)
x <- rnorm(m)
t.test(x, mu = mean(m))
mean(m)
sd(m)

summary(nc)
boxplot(nc$weight ~ nc$habit)
by(nc$weight, nc$habit, mean)
by(nc$weight, nc$habit, length)
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical", order = c("smoker", "nonsmoker"))

download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")

head(atheism)
nrow(atheism)
atheism$nationality

us12 <- subset(atheism, nationality == "United States" & year == "2012")
head(us12)
table(us12$response)
50/1002

inference(us12$response, est = "proportion", type = "ci", method = "theoretical",
          success = "atheist")
atheism$nationality

arge12 <- subset(atheism, nationality == "Argentina" & year == "2012")
aust12 <- subset(atheism, nationality == "Austria" & year == "2012")
inference(arge12$response, est = "proportion", type = "ci", method = "theoretical",
          success = "atheist")
inference(aust12$response, est = "proportion", type = "ci", method = "theoretical",
          success = "atheist")

n <- 1000
p <- seq(0, 1, 0.01)
me <- 2 * sqrt(p * (1-p)/n)
plot(me ~ p, ylab = "Margin of Error", xlab = "Population Proportion")

par(mfrow = c(2, 2))

p <- 0.1
n <- 400
p_hats <- rep(0, 5000)
for(i in 1:5000) {
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}
hist(p_hats, main = "p = 0.1, n = 400", xlim = c(0, 0.18))

l <- 0.2
k <- 1040
p_hats <- rep(0, 5000)
for(i in 1:5000) {
  samp <- sample(c("atheist", "non_atheist"), k, replace = TRUE, prob = c(l, 1-l))
  p_hats[i] <- sum(samp == "atheist")/k
}
hist(p_hats, main = "p = 0.2, n = 1040", xlim = c(0, 0.28))

p <- 0.2
n <- 400
p_hats <- rep(0, 5000)
for(i in 1:5000) {
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}
hist(p_hats, main = "p = 0.2, n = 400", xlim = c(0, 0.28))

head(nc)

inference(y = nc$weeks, est = "mean", type = "ci", null = 0, 
          method = "theoretical")

inference(y = nc$weeks, est = "mean", type = "ci", null = 0, 
          method = "theoretical", conflevel = 0.90)

inference(y = nc$gained, x = nc$mature, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

young <- subset(nc, mature == 'younger mom')
mature <- subset(nc, mature =='mature mom')
head(young)
summary(young$mage)
summary(mature$mage)

inference(y = nc$weeks, x = nc$marital, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")


head(atheism)
spain5 <- subset(atheism, nationality == 'Spain'& year == '2005')
spain12 <- subset(atheism, nationality == 'Spain'& year == '2012')
spain <-subset(atheism, nationality == 'Spain')
head(spain12)
inference(y = spain5$response, est = "proportion", type = "ci", 
          null = 0, method = "theoretical", success = 'atheist')
inference(y = spain12$response, est = "proportion", type = "ci", 
          null = 0, method = "theoretical", success = 'atheist')
inference(y = spain$year, x = spain$response, est = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical", 
          order = c("atheist","non-atheist"))

us5 <- subset(atheism, nationality == 'United States'& year == '2005')
us12 <- subset(atheism, nationality == 'United States'& year == '2012')
inference(y = us5$response, est = "proportion", type = "ci", 
          null = 0, method = "theoretical", success = 'atheist')
inference(y = us12$response, est = "proportion", type = "ci", 
          null = 0, method = "theoretical", success = 'atheist')
