# R and RStudio - A primer

## Calculations - arithmetic operators

1 + 2
1 - 2
1 * 2
1 / 2

sqrt(4)
pi
exp(1)
factorial(5)

2^2
2^2 + 24 * (34/3456) * pi

cat("\014") # disregard this; clears console


## Objects

x <- 3 # note the arrow
x
x * x
factorial(x)
x + 45

cat("\014") 


## Scalars
### scalars - single element, 'zero'-dimensional vector

x
big.number <- 34^6
big.number

cat("\014") 


## Vectors
### vectors - a single set of elements in a particular order (i.e. a one-dimensional array)

c(1, 2, 3, 4, 5)
v <- c(3, 6, 8, 12, 23, 43, 56, 101, 225, 5689)
v
v[1]
v[3]
v[1:4] # note the colon

length(v)
head(v)
tail(v)
head(v, 4)

cat("\014") 


## Characters
'Brandon'
c('Arjun', 'Brandon', 'Heike', 'Nina', 'Janice', 'Jerry', 'Vince')
v.name <- c('Arjun', 'Brandon', 'Heike', 'Nina', 'Janice', 'Jerry', 'Vince')
length(v.name)

cat("\014") 


## Logical Operators

1 < 2
1 > 2
1 == 1
1 != 1

over.six <- nchar(v.name) > 6
v.name[over.six]

under.six <- nchar(v.name) < 6
v.name[under.six]

equal.six <- nchar(v.name) == 6
v.name[equal.six]

cat("\014")


## Vector arithmetic

v <- c(10, 20, 30, 40, 50, 60)
v
v + 1
v * 2

w1 <- c(2:7) # note the colon
w1

v*w1

w2 <- c(5, 10)
w2
v*w2

w3 <- seq(5, 20, 5)
w3
v*w3

cat("\014") 


## Matrices

v <- 1:12
matrix(v, nrow = 3, ncol = 4)
matrix(v, 4, 3)

matrix(0, 3, 4)
matrix('myc', 3, 4)

m <- matrix(1:3, 3, 4, byrow = TRUE)
m
dim(m)
length(m)
head(m)
tail(m)

m <- matrix(1:12, 3, 4)
m[2, 4]
m[2,  ]
m[ , 4]

cat("\014")


## Data frames
### Data frame = matrix + column names

matrix(0, 3, 4)

x <- c(11, 12, 14)
y <- c(19, 20, 21)
z <- c(15, 25, 35)
df <- data.frame(x, y, z)
df

mean(df$x) # note the $ operator
median(df$x)
sd(df$x)

cat("\014")


## STATISTICS - regression/correlation

set.seed(965)
x <- 1:20 + rnorm(20, sd = 3) # note 'rnorm()' function
z <- 1:20/4 + rnorm(20, sd = 2)
y <- -2*x + x*z/5 + 3 + rnorm(20, sd = 4)

df.coreg <- data.frame(x, y, z)
head(df.coreg)

cat("\014")


### Correlation

head(df.coreg)
cor(df.coreg$x, df.coreg$y)
cor(df.coreg)
round(cor(df.coreg), 3)

cat("\014")


### Linear regression

lmod <- lm(df.coreg$y ~ df.coreg$x)
lmod
summary(lmod)

lmod2 <- lm(df.coreg$y ~ df.coreg$x + df.coreg$z)
lmod2
summary(lmod2)

cat("\014")


### Interactions

lmod3 <- lm(df.coreg$y ~ df.coreg$x + df.coreg$z + df.coreg$x:df.coreg$z)
lmod3
summary(lmod3)

cat("\014")


### ANOVA and post-hoc

df.n15 <- read.delim("C:/Users/Brandon Monier/OD/School Work/Graduate School Research/PhD Stuff/Projects/2014 - BTM - Wheat N15/N15 Wheat Project - R Data Sets/N15 Wheat Project-Sehgal-d15N Reads.txt") # RStudio expedites this process greatly greatly
head(df.n15)
summary(df.n15)


aov1 <- aov(d15N ~ plant, data = df.n15)
aov1
summary(aov1)
model.tables(aov1, 'means')

TukeyHSD(aov1)

cat("\014")


### Data subsetting


df.n15.sub <- 
  df.n15[df.n15$plant %in% c('T. aestivum (AABBDD)', 'Ae. tauschii (DD)'), ]
aov2 <- aov(d15N ~ plant, data = df.n15.sub)
aov2
summary(aov2)

df.n15.sub2 <- 
  df.n15[df.n15$plant %in% c('T. aestivum (AABBDD)', 'T. monococcum (AA)'), ]
aov3 <- aov(d15N ~ plant, data = df.n15.sub2)
aov3
summary(aov3)

cat("\014")

## Anscombe's Quartet

library(ggplot2)
library(gridExtra)

data(anscombe)
head(anscombe, 3)
dim(anscombe)

# mean
sapply(5:8, function(x) round(mean(anscombe[, x]), 1))
sapply(1:4, function(y) mean(anscombe[, y]))

# variance
sapply(5:8, function(x) round(var(anscombe[, x]), 2))
sapply(1:4, function(y) var(anscombe[, y]))

# correlation
sapply(1:4, function(x) cor(anscombe[, x], anscombe[, x+4]))

# linear regression
lmod4 <- lm(y1 ~ x1, data = anscombe)
summary(lmod4)

lmod5 <- lm(y2 ~ x2, data = anscombe)
summary(lmod5)

# visualization...
p1 <- 
  ggplot(anscombe) + 
  geom_point(aes(x1, y1), color = "darkorange", size = 3) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 20, 2)) + 
  scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + 
  labs(title = "dataset 1")

p2 <- 
  ggplot(anscombe) + 
  geom_point(aes(x2, y2), color = "darkorange", size = 3) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 20, 2)) + 
  scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + 
  labs(title = "dataset 2")

p3 <- 
  ggplot(anscombe) + 
  geom_point(aes(x3, y3), color = "darkorange", size = 3) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 20, 2)) + 
  scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + 
  labs(title = "dataset 3")

p4 <- 
  ggplot(anscombe) + 
  geom_point(aes(x4, y4), color = "darkorange", size = 3) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 20, 2)) + 
  scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + 
  labs(title = "dataset 4")

grid.arrange(p1, p2, p3, p4, ncol=2)

cat('\014')


## GRAPHICS

nd <- rnorm(10000)

stem(nd) # stem and leaf 'plot'

hist(nd) # histogram

density(nd)
nd.d <- density(nd)
plot(nd.d) # density plot

hist(nd, probability = TRUE)
lines(nd.d) # histogram with density overlay

boxplot(nd) # boxplot

nd.bp <- boxplot(nd) # delving into our graphics
summary(nd.bp)
nd.bp$stats
nd.bp$stats[1]

barplot(nd)
barplot(rnorm(10))

pairs(df.coreg)
pairs(df.coreg, pch = 2)
pairs(df.coreg, pch = 3)

qqnorm(nd) # visualization for normality testing
qqline(nd)

par(mfrow = c(2, 2))

hist(nd, freq = FALSE)
lines(nd.d)
barplot(rnorm(10))
boxplot(nd)
qqnorm(nd)
qqline(nd)

par(mfrow = c(1, 1))

cat('\014')


## ggplot and the 'grammar of graphics'
library(ggplot2)

### PRELIM
treatment <- c('x', 'y')
outcome <- c(20, 15)
exp.df <- data.frame(treatment, outcome)
exp.df

### BAR GRAPHS
ggplot(data = exp.df, aes(x = treatment, y = outcome)) +
  geom_bar(stat = 'identity')

ggplot(data = exp.df, aes(x = treatment, y = outcome, fill = treatment)) +
  geom_bar(stat = 'identity')

ggplot(data = exp.df, aes(x = treatment, y = outcome, fill = treatment)) +
  geom_bar(stat = 'identity', color = 'black')

ggplot(data = exp.df, aes(x = treatment, y = outcome, fill = treatment)) +
  geom_bar(stat = 'identity', color = 'black') +
  guides(fill = FALSE)

ggplot(data = exp.df, aes(x = treatment, y = outcome, fill = treatment)) +
  geom_bar(stat  = 'identity', 
           color = 'black', 
           fill  = 'cornflower blue', 
           width = 0.6) +
  guides(fill = FALSE) +
  xlab('Mycorrhizal Treatment') + ylab('Biomass (g)') +
  ggtitle('Average Biomass Production of X and Y Treatments')

### LINE GRAPHS

ggplot(data = exp.df, aes(x = treatment, y = outcome, group = 1)) +
  geom_line()

ggplot(data = exp.df, aes(x = treatment, y = outcome, group = 1)) +
  geom_line() +
  geom_point()

ggplot(data = exp.df, aes(x = treatment, y = outcome, group = 1)) +
  geom_line(color = 'cornflower blue',
            linetype = 'dashed',
            size = 3.0) +
  geom_point(size = 4.0, fill = 'white') +
  expand_limits(y = 0) +
  xlab('Mycorrhizal Treatment') + ylab('Biomass (g)') +
  ggtitle('Average Biomass Production of X and Y Treatments')

cat('\014')


## More Variables...

### PRELIM
myc.treatment <- factor(c('NM', 'NM', 'Myc', 'Myc'))
nut.treatment <- factor(c('A', 'B', 'A', 'B'))
biomass <- c(10, 15, 20, 25)
exp.df2 <- data.frame(myc.treatment, nut.treatment, biomass)
exp.df2

### BAR GRAPHS
ggplot(data = exp.df2, aes(x    = nut.treatment, 
                           y    = biomass, 
                           fill = myc.treatment)) +
  geom_bar(stat = 'identity', position = position_dodge())


ggplot(data = exp.df2, aes(x    = nut.treatment, 
                           y    = biomass, 
                           fill = myc.treatment)) +
  geom_bar(stat     = 'identity', 
           position = position_dodge(),
           color    = 'black',
           size     = 1.0) +
  scale_fill_manual(values = c('#454545', '#F5F5F5'),
                    name   = 'Mycorrhizal\nTreatment') +
  xlab('Nutrient Treatment') + 
  ylab('Biomass (g)') +
  ggtitle('Average Biomass Production of A and B Treatments') +
  theme_bw()


## Means and Error Bars

### PRELIM
myc.treatment <- factor(c('NM', 'NM', 'Myc', 'Myc'))
nut.treatment <- factor(c('A', 'B', 'A', 'B'))
biomass <- c(10, 15, 20, 25)
se <- c(2.8, 1.3, 3.4, 2.4)
exp.df3 <- data.frame(myc.treatment, nut.treatment, biomass, se)
exp.df3

### BAR GRAPHS
ggplot(data = exp.df3, aes(x    = nut.treatment, 
                           y    = biomass, 
                           fill = myc.treatment)) +
  geom_bar(stat     = 'identity', 
           position = position_dodge(),
           color    = 'black',
           size     = 1.0) +
  geom_errorbar(aes(ymin = biomass - se, ymax = biomass + se),
                size     = 0.9,
                width    = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(values = c('#454545', '#F5F5F5'),
                    name   = 'Mycorrhizal\nTreatment') +
  xlab('Nutrient Treatment') + 
  ylab('Biomass (g)') +
  ggtitle('Average Biomass Production of A and B Treatments') +
  theme_bw()


## Distributions with ggplot2

### PRELIM
treatment <- factor(rep(c('A', 'B'), each=200))
outcome <- c(rnorm(200),rnorm(200, mean = 0.5))
exp.df4 <- data.frame(treatment, outcome)
head(exp.df4)

### HISTOGRAMS
ggplot(exp.df4, aes(x = outcome)) +
  geom_histogram(binwidth = 0.2)

ggplot(exp.df4, aes(x = outcome)) +
  geom_histogram(binwidth = 0.2, color = 'black', fill = 'cornflower blue')

ggplot(exp.df4, aes(x = outcome)) +
  geom_density()

ggplot(exp.df4, aes(x = outcome)) +
  geom_histogram(aes(y    = ..density..),
                 binwidth = 0.2, 
                 color    = 'black', 
                 fill     = 'cornflower blue') +
  geom_density(size = 0.7)

ggplot(exp.df4, aes(x = outcome, fill = treatment)) +
  geom_histogram(binwidth = 0.2, position = 'identity', alpha = 0.5)

ggplot(exp.df4, aes(x = outcome, fill = treatment)) +
  geom_density(alpha = 0.5)

### BOX PLOTS
ggplot(exp.df4, aes(x = treatment, y = outcome)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shape = 1, size = 4)


## Linear Regression - Visualized with ggplot2

### PRELIM
treatment <- rep(c('A', 'B'), each = 50)
x <- 1:100 + rnorm(100, sd = 10)
y <- 1:100 + rnorm(100, sd = 10)
exp.df5 <- data.frame(treatment, x, y)
head(exp.df5)

### SCATTER PLOTS
ggplot(exp.df5, aes(x = x, y = y)) +
  geom_point(shape = 1)

ggplot(exp.df5, aes(x = x, y = y)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm)

ggplot(exp.df5, aes(x = x, y = y)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, se = FALSE)

ggplot(exp.df5, aes(x = x, y = y, shape = treatment)) +
  geom_point(size = 3)
