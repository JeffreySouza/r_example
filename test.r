
## ------------------------------------------------------------------------
## Number of times to repeat the experiment
numRepeats = 40000

## Sample size of each experiment (number of X_i Gaussians to simulate)
n = 10

## Mean and standard deviation of the X_i Gaussians
mu = 10
sigma = 2

## Set up a vector (aka, a 1D array) to hold our Z and T statistics
zStats = numeric(numRepeats)
tStats = numeric(numRepeats)

## Simulate!
for(i in 1:numRepeats) {
  x = rnorm(n, mean = mu, sd = sigma)
  ## Compare the code here to the Z and T formulas above
  zStats[i] = (mean(x) - mu) / (sigma / sqrt(n))
  tStats[i] = (mean(x) - mu) / (sd(x) / sqrt(n))
}

## ------------------------------------------------------------------------
## Clamp data just so the histogram function doesn't complain
zStats = zStats[zStats > -6 & zStats < 6]
tStats = tStats[tStats > -6 & tStats < 6]

## ------------------------------------------------------------------------
## Plot a histogram of the Z statistics
hist(zStats, freq = FALSE, xlim = c(-6,6), ylim = c(0,0.4),
     breaks = seq(-6,6,0.2), main = "Normalized Mean: Gaussian Z",
     col = "lightblue")

## Compare this to a N(0, 1) density
t = seq(-6, 6, 0.02)
lines(t, dnorm(t), lwd = 3, col = 'red')

## Plot a histogram of the T statistics
hist(tStats, freq = FALSE, xlim = c(-6,6), ylim = c(0,0.4),
     breaks = seq(-6,6,0.2), main = "Normalized Mean: Student's T",
     col = "lightblue")

## Compare this to a t(n - 1) density
lines(t, dt(t, df = n - 1), lwd = 3, col = 'red')

## ------------------------------------------------------------------------
## Comparing Gaussian Z to Student T density
plot(t, dnorm(t), type = 'l', lwd = 3, col = 'red',
     xlim = c(-6,6), ylim = c(0,0.4),
     main = "Gaussian Z vs. Student's T")
lines(t, dt(t, df = n - 1), lwd = 3, col = 'blue')
legend("topright", c("Gaussian Z", "Student T"),
       col = c("red", "blue"), lwd = 3)

## ------------------------------------------------------------------------
x = seq(-6,6,0.1)
y = dnorm(x)
plot(x, y, type = 'l', lwd = 3, col = 'red',
     main = "Critical Value of a Gaussian Z vs. Student's T")

criticalValue = qnorm(0.975)
x = seq(-criticalValue, criticalValue, 0.1)
y = dnorm(x)
polygon(c(-criticalValue, x, criticalValue), c(0, y, 0), col = "red")
text(-0.1, 0.15, format(0.95, digits = 3))

x = seq(-6,6,0.1)
y = dt(x, df = n - 1)
lines(x, y, type = 'l', lwd = 3, col = 'blue')

criticalValue = qt(0.975, df = n - 1)
x = seq(-criticalValue, criticalValue, 0.1)
y = dt(x, df = n - 1)
polygon(c(-criticalValue, x, criticalValue), c(0, y, 0), col = rgb(0,0,1,0.5))

legend("topright", c("Gaussian Z", "Student T"),
       col = c("red", "blue"), lwd = 3)

## ------------------------------------------------------------------------
## True speed of light (in km/s minus 299,000 km/s)
trueSpeed = 792.458

## Let's test the sample mean from just the 4th run (this was the
## closest to correct)
x = morley$Speed[morley$Expt == 4]
(sampleMean = mean(x))
sampleSigma = sd(x)
n = length(x)

## Use a one-sided t test to get a p-value
(tStat = (sampleMean - trueSpeed) / (sampleSigma / sqrt(n)))
(pValue = 1 - pt(tStat, df = n - 1))

## We reject the null hypothesis if this p-value is < 0.05 (alpha)
(pValue < 0.05)

## All of these steps can be done with a single R command. (This is
## what you would use in practice!)
t.test(x - trueSpeed, alternative = "greater")

## ------------------------------------------------------------------------
## Let's reduce it down to just two of the three species
iris2 = iris[iris$Species == "virginica" | iris$Species == "versicolor",]
iris2$Species = factor(iris2$Species, levels = c("versicolor", "virginica"))

## First, let's do some boxplots of the data
boxplot(Sepal.Length ~ Species, data = iris2, ylab = "Sepal Length")

## ------------------------------------------------------------------------
## Set up the two lists of data
vers = iris2$Sepal.Length[iris2$Species == "versicolor"]
virg = iris2$Sepal.Length[iris2$Species == "virginica"]
n = length(vers)
m = length(virg)

## Assuming equal variances, we start with computing the pooled variance
(sp = ((n - 1) * var(vers) + (m - 1) * var(virg)) / (n + m - 2) * (1/n + 1/m))

## Now, we construct the t-statistic
(tStat = (mean(vers) - mean(virg)) / sqrt(sp))

## Finally, we get a p-value by looking up in the t distribution.
## Note this is a two-sided test because of our alternate hypothesis.
## In the two sided test we have to calculate the probability of being "more
## extreme" to the right of |t| and to the left of -|t|
(p = (1 - pt(abs(tStat), df = m + n - 2)) + pt(-abs(tStat), df = m + n - 2))

## This is how to do this hypothesis test step-by-step.
## In practice it is easiest to use the built-in "t.test" function in R
t.test(vers, virg, var.equal = TRUE)

## ------------------------------------------------------------------------
(p = pt(tStat, df = m + n - 2))

## Again, the simple t.test way looks like this:
t.test(vers, virg, var.equal = TRUE, alternative = "less")
