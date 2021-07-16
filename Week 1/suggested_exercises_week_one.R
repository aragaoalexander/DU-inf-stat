## Script for the suggested exercises in Week 1 of Duke University's 
## Inferential Statistics course on Coursera.
## Exercises are taken from Open Intro Statistics, 3rd ed.
## List of Exercises: 4.1, 4.3, 4.5, 4.33, 4. 35, 4.37, 4.41
##                    4.9, 4.11, 4.13, 4.15.
## Author: Alexander Aragão

##  Variability in Estimates --------------------------------------------------
# Exercise 4.1
# a) numerical, mean
# b) numerical, mean
# c) categorial,proportion
# d) numerical, mean
# e) numerical, proportion


## Exercise 4.3
cred_min = 8
cred_q1 = 13
cred_median = 14
cred_mean = 13.65
cred_sd = 1.91
cred_q3 = 15
cred_max = 18

# a) average number of credits is the mean, 13.65. Median is 14.
# b) SD = 1.91, IQR is: 2
cred_IQR = cred_q3 - cred_q1
print(cred_IQR)

# c) It's an unusual value, under 2 standard deviations of the mean.
cred_outlier = cred_mean + 2*cred_sd
print(cred_outlier)

ifelse(
  16 - cred_outlier > 0,
  print("Unusual"),
  print("Usual"))

# d) It's not that different.
# e) 
college_sd = cred_sd/sqrt(100)
college_sd

## Exercise 4.5
# a) Sampling distribution.
# b) Approximately normal..
# c)
hen_sd = 18.2/sqrt(45)
hen_sd
# d) It is more variable.
hen_sd2 = 18.2/sqrt(10)
hen_sd2


## Confidence Intervals -------------------------------------------------------

# Exercise 4.9
news_mean = 0.52
news_se = 0.024
confidence = 0.99
z_99 = (-1)*qnorm((1-0.99)/2)
z_99
error_margin_99 = z_99*news_se
error_margin_99*100 #It means that 52% +- 6.2% of users get their news from Twitter.

# Exercise 4.11
relax_sample = 1155
relax_confidence_1 = 0.95
relax_max_95 = 1.92
relax_min_95 = 1.38

# a) It means that 95% of sample means of the hours of relaxation fall between 1.38 and 1.92.
# b) With a large margin, the confidence level is higher.
# c) The margin of error will be lower.

# Exercise 4.13
# a) False.
# b) False.
# c) True.
# d) False
# e) False.
# f) True.
# g) False

## Exercise 4.15
exc_rel_mean = 3.2
exc_rel_sd = 1.97
exc_rel_z90 = (-1)*qnorm((1-0.90)/2)

exc_rel_me = exc_rel_z90*exc_rel_sd/(sqrt(203))
exc_rel_me

exc_rel_max = exc_rel_mean + exc_rel_me
exc_rel_min = exc_rel_mean - exc_rel_me

exc_rel_max
exc_rel_min

## Examining the Central Limit Theorem ----------------------------------------
## Exercise 4.33
# a) The distribution is right skewed.
# b) The shape has a larger skew in lower number of samples, and is closer to
#    a normal distribution in larger samples.
# c)

pen_mu = 10.44
pen_sigma = 9.2


## Exercise 4.35
# a) it's right skewed.

# b) Expect less.
# c) 
prob_over_1.4 = 1-pnorm(1.4, mean = 1.3, sd = 0.3)
prob_over_1.4

# d)



# e) Decrease.

## Exercise 4.37

# 1) B
# 2) A
# 3) C

## Exercise 4.41
# Same as class.
