#Setting up the working directory
getwd
setwd("~/Desktop/SPRING 2017/Foundation of Analytics/Project")
dir()
Indian_Census_Final <- read.csv("~/Desktop/SPRING 2017/Foundation of Analytics/Project/Indian Census Final.csv", stringsAsFactors = T)
Indian_Census_Final

options(max.print = 9999999)
save(Indian_Census_Final, file = "Indian_Census_Final.RData")

is.na(Indian_Census_Final)
na.omit(Indian_Census_Final)
View(Indian_Census_Final)

#Categorical Data
table(Religion1.Name)
table(Religion2.Name)
table(Religion3.Name)
table(c(Religion1.Name, Religion2.Name, Religion3.Name))


#Barplot
barplot(table(Indian_Census_Final$Religion1.Name), col ="cyan", ylim = c(0, 500), xlab = "Religion", ylab = "Frequency", las =2)

barplot(table(Indian_Census_Final$Religion2.Name), col ="cyan", ylim = c(0, 400), xlab = "Religion", ylab = "Frequency", las =2)

barplot(table(Indian_Census_Final$Religion3.Name), col ="cyan", ylim = c(0, 180), xlab = "Religion", ylab = "Frequency", las =2)


barplot(table(c(Indian_Census_Final$Religion1.Name, Indian_Census_Final$Religion2.Name, Indian_Census_Final$Religion3.Name)),
names.arg = c("Buddhists", "Christians", "Hindus", "Muslims", "Others", "Sikhs"), col ="cyan", 
ylim = c(0, 600), xlab = "Religion", ylab = "Frequency", las =2)

#Pie Chart
pie(table(c(Indian_Census_Final$Religion1.Name, Indian_Census_Final$Religion2.Name, Indian_Census_Final$Religion3.Name)),
labels = c("Buddhists", "Christians", "Hindus", "Muslims", "Others", "Sikhs"), col =hcl(c(0, 60, 120)))

data <-table(c(Religion1.Name, Religion2.Name, Religion3.Name))
data
slice.labels <- c("Buddhists", "Christians", "Hindus", "Muslims", "Others", "Sikhs")
slice.labels
slice.percents <- round(data/sum(data)*100)
slice.percents
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep = "")

pie(data, labels =slice.labels, col =hcl(c(0, 60, 120)))

#Numerical Data
info <- aggregate(Indian_Census_Final[, c("Males")], list(Indian_Census_Final$State), sum)
info

Census.State <- info$Group.1
Census.State

Census.Male <- info$x
Census.Male

Census.info <- data.frame(Census.State, Census.Male)
Census.info

Census.info <- data.frame(State = Census.State, Male = Census.Male)
Census.info

Census.orig <- Census.info

Census.info$Males <- Census.info$Male/10000
Census.info

mean(Census.info$Males)
median(Census.info$Males)
range(Census.info$Males)
diff(range(Census.info$Males))
var(Census.info$Males)
sd(Census.info$Males)
fivenum(Census.info$Males)
summary(Census.info$Males)

#Barplot
barplot(Census.info$Males, names.arg = Census.info$State, xlab = "States", ylab = "Male Population", col = rainbow(34), las = 2)

#Dot Chart
dotchart(Census.info$Males, labels = Census.info$State, xlab = "States", ylab = "Male Population")

#Histogram : Number of States whose Male population is between a particular number 
hist(Census.info$Males, col = hcl(0), ylim = c(0,20))

#BoxPlot
boxplot(Census.info$Males, horizontal = TRUE, xaxt = "n", xlab = "Male Population")
axis(side = 1, at = fivenum(Census.info$Males), labels = TRUE, las = 2)

#To remove Outliers
boxplot(Census.info$Males, horizontal = TRUE, xaxt = "n", xlab = "Male Population", outline = FALSE)
axis(side = 1, at = fivenum(Census.info$Males), labels = TRUE, las = 2)


#Central Limit Theorem
info <- aggregate(Indian_Census_Final[, c("Population")], list(Indian_Census_Final$State), sum)
info

Census.State <- info$Group.1
Census.State

Census.Population <- info$x
Census.Population

Census.info <- data.frame(Census.State, Census.Population)
Census.info

Census.info <- data.frame(State = Census.State, Population = Census.Population)
Census.info

Census.orig <- Census.info

Census.info$Population <- Census.info$Population/10000
Census.info

mean <- mean(Census.info$Population)
mean
sd <- sd(Census.info$Population)
sd

x <- rnorm(Census.info$Population, mean = 3007.095, sd = 3751.181)
x
hist(x, prob=TRUE, xlim = c(-6000, 12000), ylim = c(0, 0.00020), col="blue")

#Sample size = 20
samples <- nrow(Indian_Census_Final)
sample.size <- 20
xbar <- numeric(samples)
xbar
for(i in 1: samples){
  xbar[i] <- mean(rnorm(sample.size, mean = 3007.095, sd = 3751.181))
}

hist(xbar, prob=TRUE, breaks=15, xlim = c(0, 6700), ylim = c(0, 0.0006), main="Sample size of 20", col=hcl(0))
mean <-mean(xbar)
mean
sd <- sd(xbar)
sd


#Sample size = 50
sample <- nrow(Indian_Census_Final)
sample.size <- 50
xbar <- numeric(sample)
xbar
for(i in 1: sample){
  xbar[i] <- mean(rnorm(sample.size, mean = 3007.095, sd = 3751.181))
}

hist(xbar, prob=TRUE, breaks=15, xlim = c(1500, 5000), ylim = c(0, 0.001), main="Sample size of 50", col=hcl(0))
mean <- mean(xbar)
mean
sd <- sd(xbar)
sd

#Sample size = 70
sample <- nrow(Indian_Census_Final)
sample.size <- 70
xbar <- numeric(sample)
xbar
for(i in 1: sample){
  xbar[i] <- mean(rnorm(sample.size, mean = 3007.095, sd = 3751.181))
}

hist(xbar, prob=TRUE, breaks=15, xlim = c(2000, 4500), ylim = c(0, 0.0012), main="Sample size of 70", col=hcl(0))
mean <- mean(xbar)
mean
sd <- sd(xbar)
sd

#Sample size = 100
sample <- nrow(Indian_Census_Final)
sample.size <- 100
xbar <- numeric(sample)
xbar
for(i in 1: sample){
  xbar[i] <- mean(rnorm(sample.size, mean = 3007.095, sd = 3751.181))
}

hist(xbar, prob=TRUE, breaks=15, xlim = c(2000, 4500), ylim = c(0, 0.0015), main="Sample size of 100", col=hcl(0))
mean <- mean(xbar)
mean
sd <- sd(xbar)
sd

# Various Sampling Methods
library(sampling)

#Sampling Methods With Replacement
s <- srswr(6, nrow(Census.info))
s[s!=0]
rows <- (1:nrow(Census.info))[s!=0]
rows <- rep(rows, s[s!=0])
rows
sample.1 <- Census.info[rows, ]
head(sample.1)
table(sample.1$Population)

#Sampling Methods Without Replacement
s <- srswor(6, nrow(Census.info))
sample.2 <- Census.info[s!=0, ]
head(sample.2)
table(sample.2$Population)

#Systematic Sampling
N <- nrow(Census.info)
N
n <- 5
k <- ceiling(N/n)
k

#Random item from first group
r <- sample(k, 1)
r

#Select every kth item
s <- seq(r, by = k, length = n)
sample.3 <- Census.info[s, ]
head(sample.3)
table(sample.3$Population)


#Stratified Sampling
data.2 <- data.frame(State = Indian_Census_Final$State, Population = Indian_Census_Final$Population)
data.2
st.2 <- strata(data.2, stratanames = c("State"), size = rep(5,12), method = "srswor", description = TRUE)

#Clustering
c1 <- cluster(data.2, c("State"), size=20, method="srswor")
c1.sample <- getdata(data.2, c1)
(c1.sample)
table(c1.sample$State)


#Confidence interval
set.seed(150)

pop.mean <- 3007.095
pop.sd <- 3751.181

x <- rnorm(nrow(Indian_Census_Final), mean = pop.mean, sd = pop.sd)
x <- as.integer(x)

sample.size <- 50

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

sample.data <- sample(x, size = sample.size)
head(sample.data)

xbar <- mean(sample.data)
xbar

# 95.44% Confidence Interval
cat("95.44% Conf Interval = ", xbar - 2*sd.sample.means, "-", xbar + 2*sd.sample.means, "\n")

# For Confidence Level 80 and 90
conf <- c(80, 90)
alpha <- 1- conf/100
result <- data.frame()
for(i in alpha){ 
  # Conf Level, alpha, CI_Lower, CI_Upper
  result <- rbind(result, c(100*(1-i), i, 
                            xbar - qnorm(1-i/2)*sd.sample.means, 
                            xbar + qnorm(1-i/2)*sd.sample.means))  
}
colnames(result) <- c("Conf Level", "alpha", "CI_Lower", "CI_Upper")
result

#poplation mean lies between the range of both the confidence intervals