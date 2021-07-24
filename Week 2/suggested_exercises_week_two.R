## Script for the suggested exercises in Week 1 of Duke University's 
## Inferential Statistics course on Coursera.
## Exercises are taken from Open Intro Statistics, 3rd ed.
## Author: Alexander Aragão

##Suggested reading: Chapter 4, Section 4.3, 4.4, 4.5, 4.6 (excluding Section 4.6.2)

##Suggested exercises: (End of chapter exercises from OpenIntro Statistics)

##Hypothesis tests: 4.17, 4.19, 4.23, 4.25, 4.27
##Inference for other estimators: 4.43, 4.45
##Decision errors, significance, and confidence: 4.29, 4.31, 4.47

## 4.6.3 Hypothesis testing ---------------------------------------------------

# 4.17. Identify the Hypothesis
# a) H0: mean_sleep = 8 hours; H1: mean_sleep < 8 h
# b) H1: product_year = product_mm; H1: product_year > product_mm

# 4.19 Online Communication
# Errors: H0 should be an equation, H1 and H0 should consider the same point estimate.

# 4.23 Nutrition labels
nl_mean = 130
nl_sample_mean = 134
nl_sample_sd = 17
nl_sample_n = 35

# H0: nl_mean = 130; H1: nl_mean != 130

nl_sample_se = nl_sample_sd/sqrt(nl_sample_n)
nl_sample_se

nl_Z = (nl_sample_mean - nl_mean)/nl_sample_se
nl_Z

nl_pvalue = (1-pnorm(q = (nl_Z)))*2
nl_pvalue
ifelse(nl_pvalue<0.05,"Reject H0","Can't reject H0") #H0 couldn't be rejected, thus the difference isn't statistically significant.

# 4.25 Waiting at and ER, Part III
er_sample_n = 64
er_sample_mean = 137.5
er_sample_sd = 39
er_null_mean = 127

# a) Yes. The observations are independent and the sample is >30, so normality
# can be assumed.
# b) 

er_sample_se = er_sample_sd/sqrt(er_sample_n)
er_Z = (er_sample_mean - er_null_mean)/(er_sample_se)
er_Z

er_pvalue = (1-pnorm(q = er_Z))*2
er_pvalue
ifelse(er_pvalue<0.05,"Reject H0","Can't reject H0") #H0 was rejected, thus, the mean is statistically different.

# c) Yes. The difference wouldn't be statistically significant because the p-value < \alpha

# 4.27 Working backwards, one-sided
wb_sample_sd = 10
wb_sample_n = 70
wb_pvalue = 0.05

wb_sample_se = wb_sample_sd/sqrt(wb_sample_n)
wb_Z = -qnorm(p = wb_pvalue)
wb_Z

wb_sample_mean = wb_Z*wb_sample_se+30
wb_sample_mean

# 4.29 Testing for fibromyalgia
# a) H0: symptoms_before = symptoms_after; H1: symptoms_before < symptoms_after
# b) Concluding that the medicine helps when it doesn't (H0 is incorretly rejected)
# c) Concluding that the medicine doesn't help when it actually helps (H0 is incorrectly not rejected)


# 4.31 Which is higher
# a) (I) is higher.
# b) (II) is higher
# c) (I) is higher
# d) Same.

## 4.6.5 Inference for other estimators ---------------------------------------

# 4.43 Spam Mail counts.
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
