# CS3130 hw7.r
#
# Name:	Jeff Souza
# uNID:	u0402450

#----------------------------------------------------------------------#
# Question 1a: hypothesis					       #
#                                                                      #
#    null hypothesis:                                                  #
#       Women have the same resting blood pressure as men              #
#                                                                      #
#    alternate hyopthesis:					       #
#       Women have on average lower resting blood pressure than men    #
#                                                                      #
#----------------------------------------------------------------------#

cardiac = read.csv("cardiac.dat")

#----------------------------------------------------------------------#
# Question 1b: boxplots     					       #
#                                                                      #
#----------------------------------------------------------------------#

# setup our data
#    basebp = base blood pressure
#    gender: male = 0, female = 1
maleBP = cardiac$basebp[cardiac$gender == 0]
femaleBP = cardiac$basebp[cardiac$gender == 1]

# make the boxplots
boxplot(maleBP,femaleBP,col=c("cadetblue1","pink"),names=c("Male","Female"),main="Blood Pressure")

#----------------------------------------------------------------------#
# Question 1c: t-test						       #
#                                                                      #
#----------------------------------------------------------------------#

n = length(maleBP)
m = length(femaleBP)

sp = ((n-1) * var(maleBP) + (m-1) * var(femaleBP)) / (n+m-2)*(1/n+1/m)
tStat = (mean(maleBP) - mean(femaleBP)) / sqrt(sp)
p = (1-pt(abs(tStat), df = n+m-2)) + pt(-abs(tStat), df=n+m-2)

# answers
tStat   # -3.240228
p       # .001265731

# since p < alpha (.05)
# YES. We reject the null hypothesis

#----------------------------------------------------------------------#
# Question 1da: hypothesis					       #
#                                                                      #
#    null hypothesis:                                                  #
#       People with history of atleast 1 cardiac event have the        # 
#       same average resting blood pressure as those without           #
#                                                                      #
#    alternate hyopthesis:					       #
#       People with history of atleast 1 cardiac event have            #
#	a higher average blood pressure than those without             #
#                                                                      #
#----------------------------------------------------------------------#

#----------------------------------------------------------------------#
# Question 1db: boxplots     					       #
#                                                                      #
#----------------------------------------------------------------------#

# setup our data
normalBP = cardiac$basebp[cardiac$any.event == 1]
afflictedBP = cardiac$basebp[cardiac$any.event == 0]

# make the boxplots
boxplot(normalBP,afflictedBP,col=c("black","red"),names=c("No cardiac event","Had cardiac event"),main="Blood Pressure")

#----------------------------------------------------------------------#
# Question 1dc: t-test						       #
#                                                                      #
#----------------------------------------------------------------------#

n = length(normalBP)
m = length(afflictedBP)

sp = ((n-1) * var(normalBP) + (m-1) * var(afflictedBP)) / (n+m-2)*(1/n+1/m)
tStat = (mean(normalBP) - mean(afflictedBP)) / sqrt(sp)
p = (1-pt(abs(tStat), df = n+m-2)) + pt(-abs(tStat), df=n+m-2)

# answers
tStat   # -0.7797902
p       # .4358462

# since p < alpha (.05)
# NO. We fail to reject the null hypothesis


#----------------------------------------------------------------------#
# Question 1ea: hypothesis					       #
#                                                                      #
#    null hypothesis:                                                  #
#       People with history of atleast 1 cardiac event have the        # 
#       same effieicnecy of heart pumping as those without             #
#                                                                      #
#    alternate hyopthesis:					       #
#       People with history of atleast 1 cardiac event have            #
#	a higher average blood pressure than those without             #
#                                                                      #
#----------------------------------------------------------------------#

#----------------------------------------------------------------------#
# Question 1eb: boxplots     					       #
#                                                                      #
#----------------------------------------------------------------------#

# setup our data
normalEF = cardiac$baseEF[cardiac$any.event == 1]
afflictedEF = cardiac$baseEF[cardiac$any.event == 0]

# make the boxplots
boxplot(normalEF,afflictedEF,col=c("black","red"),names=c("No cardiac event","Had cardiac event"),main="Heart's Pumping Efficiency")

#----------------------------------------------------------------------#
# Question 1ec: t-test						       #
#                                                                      #
#----------------------------------------------------------------------#

n = length(normalEF)
m = length(afflictedEF)

sp = ((n-1) * var(normalEF) + (m-1) * var(afflictedEF)) / (n+m-2)*(1/n+1/m)
tStat = (mean(normalEF) - mean(afflictedEF)) / sqrt(sp)
p = (1-pt(abs(tStat), df = n+m-2)) + pt(-abs(tStat), df=n+m-2)

# answers
tStat   # 5.5142
p       # 5.373585e-08

# since p < alpha (.05)
# YES we reject the null hypothesis


#----------------------------------------------------------------------#
# Question 2a: hypothesis					       #
#    null hypothesis:                                                  #
#       there is no correlation between petal and sepal lengths        #
#    alternative hypothesis:					       #
#       long petal irises have long sepals                             #
#       short petal irises have short sepals                           #
#                                                                      #
#----------------------------------------------------------------------#

#----------------------------------------------------------------------#
# Question 2b: scatterplot					       #
#                                                                      #
#----------------------------------------------------------------------#

x = iris$Petal.Length
y = iris$Sepal.Length

plot(x,y,xlab="Petal Length",ylab="Sepal Length",main="Iris Plot")

#----------------------------------------------------------------------#
# Question 2c: critical value  	        	                       #
#                                                                      #
#----------------------------------------------------------------------#

criticalValue = qnorm(0.95)
criticalValue # 1.644854

#----------------------------------------------------------------------#
# Question 2d: t statistic					       #
#                                                                      #
#----------------------------------------------------------------------#

n = length(x)
r = cor(x,y)
t = r*sqrt((n-2)/(1-r^2))
t # 21.64602 

#----------------------------------------------------------------------#
# Question 2e: p value					       #
#                                                                      #
#----------------------------------------------------------------------#

p = 1 - pt(t, df = 2*n-2) 
p # 0


#----------------------------------------------------------------------#
# Question 2f: hypothesis					       #
#    null hypothesis:                                                  #
#       there is no correlation between sepal length and width         #
#    alternative hypothesis:					       #
#       long sepal irises have skinny sepals                           #
#       short sepal irises have fat sepals                             #
#                                                                      #
#----------------------------------------------------------------------#

#----------------------------------------------------------------------#
# Question 2gb: scatterplot					       #
#                                                                      #
#----------------------------------------------------------------------#

x = iris$Sepal.Width
y = iris$Sepal.Length

plot(x,y,xlab="Sepal Width",ylab="Sepal Length",main="Iris Plot")

#----------------------------------------------------------------------#
# Question 2gc: critical value  	        	                       #
#                                                                      #
#----------------------------------------------------------------------#

criticalValue = qnorm(0.95)
criticalValue # 1.644854

#----------------------------------------------------------------------#
# Question 2gd: t statistic					       #
#                                                                      #
#----------------------------------------------------------------------#

n = length(x)
r = cor(x,y)
t = r*sqrt((n-2)/(1-r^2))
t # -1.440287 

#----------------------------------------------------------------------#
# Question 3a: faithful regression				       #
#                                                                      #
#----------------------------------------------------------------------#

my.regression = function(x,y) {
   slope = cov(x,y) / var(x)
   intercept = (mean(y) - (slope * mean(x)))
   return(list(i=intercept, s=slope))
}

regression = my.regression(faithful$waiting, faithful$eruptions)

intercept = regression$i
intercept # -1.874016

slope = regression$s
slope # .07562795


#----------------------------------------------------------------------#
# Question 3b: estimate eruption				       #
#                                                                      #
#----------------------------------------------------------------------#

time = 82
eruption = time * slope + intercept
eruption # 4.327476


#----------------------------------------------------------------------#
# Question 3c: check regression					       #
#                                                                      #
#----------------------------------------------------------------------#

linearmodel = lm(eruptions ~ waiting, data=faithful)


#----------------------------------------------------------------------#
# Question 3d: p value						       #
#                                                                      #
#----------------------------------------------------------------------#

summary(linearmodel) # p-value < 2.2e-16

# Conclusion: Reject null hypothesis.
#             Wait time is definitely correlated with eruption size.
