## Script for the suggested exercises in Week 3 of Duke University's 
## Inferential Statistics course on Coursera.
## Exercises are taken from Open Intro Statistics, 3rd ed.
## Author: Alexander Aragão

##Suggested reading: Chapter 5, Section 5.1, 5.2, 5.3, 5.4 (excluding Section 4.6.2)

##Suggested exercises: (End of chapter exercises from OpenIntro Statistics)

## t-inference: 5.1, 5.3, 5.5, 5.13, 5.17, 5.19, 5.21, 5.23, 5.27, 5.31, 5.35, 5.37
## Power: 5.39
## Comparing three or more means (ANOVA): 5.41, 5.43, 5.45, 5.47, 5.49, 5.51

## 5.6.1 One-sample means with the t-distribution------------------

# 5.1 Identify the critical value
# a) n = 6, CL = 90%
n = 6
alpha = 0.1
df = n-1
tcrit = qt(p = alpha/2, df = df, lower.tail = FALSE)
tcrit

# b) n = 21, CL = 98%

n = 21
alpha = 0.02
df = n-1
tcrit = qt(p = alpha/2, df = df, lower.tail = FALSE)
tcrit

# 5.3 Find the p-value, part I.
# a) HA: mu > mu0, n = 11, T = 1.91
alpha = 0.05
n = 11
df = n-1
t = 1.91
pvalue = pt(q = t, df = df, lower.tail = FALSE)
pvalue
ifelse(pvalue < alpha, "Reject H0", "Not reject H0")

# b) HA: mu < m0, n = 18, T = -3.45 
n = 18
df = n-1
t = -3.45
pvalue = pt(q = t, df = df, lower.tail = TRUE)
pvalue
ifelse(pvalue < alpha, "Reject H0", "Not reject H0")

# b) HA: mu != mU0, n = 7, T = 0.83

n = 7
df = n-1
t = .83
pvalue = pt(q = t, df = df, lower.tail = FALSE)
pvalue
ifelse(pvalue < alpha, "Reject H0", "Not reject H0")

# 5.5 Working backwards, Part I.
alpha = 0.05
cl = 1-alpha
lower = 18.985
upper = 21.015
n = 36
xbar = (upper+lower)/2
xbar

tcrit = qt(p = alpha/2, df = n-1, lower.tail = FALSE)
s = (upper - xbar)*sqrt(n)/tcrit
s

# 5.13 Car insurance savings.
s = 100
margin = 10
cl = 0.95

zcrit = qnorm(p = (1-cl)/2, lower.tail = FALSE)
zcrit

n = (zcrit*s/margin)^2
n
n = ceiling(n)
n

tcrit = qt(p = alpha/2, df = n-1, lower.tail = FALSE)
tcrit


## 5.6.2 Paired Data ---------------------------------------

# 5.17 Paired or Not, Part I
# a) Yes, paired.
# b) Not paired.
# c) Paired
# d) Paired.

# 5.19 Global Warming, Part I

# a) H0: avg_spam_2004 = avg_spam_2009; H1: avg_spam_2004 != avg_spam_2009
# b) 
avg_spam_2004 = 18.5
avg_spam_2009 = 14.9
avg_spam_diff = avg_spam_2004 - avg_spam_2009
avg_spam_diff

# c) It means that H0 wasn't rejected because the p-value wasn't lower than the
# alpha value. Which means the samples was inside expected values.

# d) Yes. Because it wasn't rejected, so it must contain 0.

# 4.45 Spam mail percentages.
# a) H0: spam_2004 - spam_2009 = 0; H1: spam_2004 - spam_2009 != 0
# b)
spam_2004 = 0.23
spam_2009 = 0.16
spam_diff = spam_2004-spam_2009
spam_diff
# c) It means that H0 was rejected. In other words, the p-value was smaller than
# the chosen alpha value (SL), which means that the observed sample mean was rare
# enough to warrant rejecting H0.
# d) No. If H0 was rejected, it means that 0 isn't inside the CL.

# 4.47 It means that when "n" is higher, the Z values are more extreme, which
# leads to the p-value (the shaded are under the curve) being smaller, so H0 is
# more easily rejected.
