# 1
xbar = 94
sn = .75
n = 10
t = qt(c(.025, .975), df =n-1)

ci = xbar + t*sn/sqrt(n)
print(ci)

# 2
n = 16
t = qt(c(.025, .975), df = n-1)
ci = c(1.6, 7.8)

a = matrix(c(1, t[1]/sqrt(n), 1, t[2]/sqrt(n)), nrow = 2, ncol = 2, byrow = TRUE)
b = matrix(ci, nrow=2, ncol = 1, byrow = FALSE)
ans = solve(a, b)

xbar = mean(ci)
sn = ans[2]

t = qt(c(.005, .995), df = n-1)
ci = xbar + t*sn/sqrt(n)

print(ci)

# 3b
x = iris$Sepal.Width[iris$Species == "virginica"]

xbar = mean(x)
sn = var(x)
n = length(x)
t = qt(c(.025, .975), df = n-1)

ci = xbar + t*sn/sqrt(n)
print(ci)

# 3c
y = x[1:10]

xbar = mean(y)
sn = var(y)
n = length(y)
t = qt(c(.025, .975), df = n-1)

ci = xbar + t*sn/sqrt(n)
print(ci)
