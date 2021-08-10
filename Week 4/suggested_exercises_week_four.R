## Script for the suggested exercises in Week 4 of Duke University's 
## Inferential Statistics course on Coursera.
## Exercises are taken from Open Intro Statistics, 3rd ed.
## Author: Alexander Aragão

##Suggested reading: Chapter 6, Sections 6.1 - 6.6

##Suggested exercises: (End of chapter exercises from OpenIntro Statistics)

## Single proportion: 6.1, 6.3, 6.5, 6.9, 6.11, 6.13, 6.15, 6.19, 6.21
## Comparing two proportions: 6.23, 6.25, 6.27, 6.29, 6.31, 6.33, 6.35
## Inference for proportions via simulation: 6.51, 6.53, 6.55
## Comparing three or more proportions (Chi-square): 6.39, 6.41, 6.43, 6.45, 6.47

## 6.7.1 Inference for a single proportion ------------------

# 6.1 Vegetarian College Students
# a) False. Considering "being vegetarian" to be the "success" condition, it means that in a sample of 60 people,
# there's an expected number of 5 vegetarian, which is less than 10 successes, failing the success-failure test.
# b) True, since the expected number of vegetarians is low.
# c) No, since it falls within 2 standard errors of the expected value.
0.04/sqrt(0.08*0.92/125)

# d) Yes, since it would be more than 2 SEs
0.04/sqrt(0.08*0.92/250)

# e) No. It's 30% less.

# 6.3 Orange tabbies
p = 0.9
# a) Yes, since we have less than 10 failures.
p*30

# b) Yes.
# c) Yes, since it'll have more than 10 successes and 10 failures.
n = 140
n*p
n*(1-p)

# d) Yes, same reasoning as above.

# 6.9 Life after college.
succ = 348
n = 400
pop = 4500

# a) Parameter of interest is the proportion of students who finds jobs after graduation. Point estimate is 348/400 = 
p_hat = succ/n
p_hat

# b) 1. Independence: random sampling happens and 400<10% of 4500 students. 2. Sample size: check, since we have more than 10 successes
# and failures.

# c) The CI is (83.7%, 90.3%), which means that 95% of random samples would result
# in a 95% CI that include the true proportion of students.

se = sqrt(p_hat*(1-p_hat)/n)
lower = p_hat - qnorm(p = 0.025, lower.tail = FALSE)*se
upper = p_hat + qnorm(p = 0.025, lower.tail = FALSE)*se
lower
p_hat
upper

# d) It means that 95% of the time, the true proportion falls inside that interval.
# There's a 5% chance (Type I error) that it doesn't.

# e) For the 99% CL, the interval is (82.7%, 91.3%).

lower = p_hat - qnorm(p = 0.005, lower.tail = FALSE)*se
upper = p_hat + qnorm(p = 0.005, lower.tail = FALSE)*se
lower
p_hat
upper

# f) 99% is wider, so that we have a higher chance of capturing the true proportion in the population.

# 6.11 Study Abroad
# a) Probably not. Since the sample was made using an optional web survey, it's
# probable that it has significant bias, so it probably isn't a representative 
# sample.

# b) CL = 90% is CI = (52.9%, 57.1%)
p_hat = 0.55
n = 1509
se = sqrt(p_hat*(1-p_hat)/n)
lower = p_hat - qnorm(p = 0.05, lower.tail = FALSE)*se
upper = p_hat + qnorm(p = 0.05, lower.tail = FALSE)*se
lower
p_hat
upper

# c) It means that we're 90% sure that the true proportion of the population is
# in that interval.

# d) Yes.

# 6.13 Public option, Part I.
# a) CI is (47.1%, 56.9%) for 95% confidence. Which means that the data doesn't
# provide sufficient enough evidence to conclude that the majority of independts
# oppose the legislation.

p = 0.52
se = sqrt(p*(1-p)/n)
lower = p - qnorm(p = 0.025, lower.tail = FALSE)*se
upper = p + qnorm(p = 0.025, lower.tail = FALSE)*se
lower
p
upper

# b) Yes.

# 6.15 Browsing in the mobile device
# a) H0: p = 38%, HA: p != 38%, SL = 5%
p = 0.17
p0 = 0.38
n = 2254
se = sqrt(p0*(1-p0)/n)
z = (p-p0)/se
z
pnorm(z)

# b) The p-value is extremely small, which means we can safely reject H0. In essence,
# the american proportion is different from 38%.

#c) For a 95% CL, the interval is (15.4%, 18.6%), which doesn't contain 38%.
p = 0.17
n = 2254
se = sqrt(p*(1-p)/n)
lower = p - qnorm(p = 0.025, lower.tail = FALSE)*se
upper = p + qnorm(p = 0.025, lower.tail = FALSE)*se
lower
p
upper

# 6.19 College smokers
# a) Conditions: 1. Independence: random sample (check), n(200) <10% of population.
# 2. Sample size: more than 10% successes and fails in sample.
# CI is (14.4%, 25.6%), CL = 95%.
n = 200
p = 40/200
se = sqrt(p*(1-p)/n)
lower = p - qnorm(p = 0.025, lower.tail = FALSE)*se
upper = p + qnorm(p = 0.025, lower.tail = FALSE)*se
lower
p
upper

# b) n = 1537 students
me = 0.02
z = -qnorm(0.025)
n_calc = z^2*p*(1-p)/me^2
n_calc

# 6.21 Public option, Part II
# n = 6754 respondents.
me = 0.01
z = -qnorm(0.05)
p = 0.52
n_calc = z^2*p*(1-p)/me^2
n_calc

## 6.7.2 Difference of two proportions ---------------------------------------


# 5.45 Coffee, depression and physical activity
# a) H0: All means are equal. HA: At least one pair of means differs from each other.
# b) We can assume random sampling was made, meaning that the observations are independent from each other. Normality may
# not be significant, since the values are never negative, so a right skew can be expected. The variances are almost equal
# between groups.
# c)

cf_n = 50739
cf_k = 5
cf_m1 = 18.7
cf_m2 = 19.6
cf_m3 = 19.3
cf_m4 = 18.9
cf_m5 = 17.5
cf_sd1 = 21.1
cf_sd2 = 25.5
cf_sd3 = 22.5
cf_sd4 = 22
cf_sd5 = 22
cf_n1 = 12215
cf_n2 = 6617
cf_n3 = 17234
cf_n4 = 12290
cf_n5 = 2383

cf_mean <- c(cf_m1, cf_m2, cf_m3, cf_m4, cf_m5)
cf_sd <- c(cf_sd1, cf_sd2, cf_sd3, cf_sd4, cf_sd5)
cf_n <- c(cf_n1, cf_n2, cf_n3, cf_n4, cf_n5)

coffee <- data.frame(cf_mean, cf_sd, cf_n,
                     row.names = c("one_cup_wk", "two_six_cup_wk", "one_cup_day", "two_three_cup_day", "four_cup_day")
)

cf_dft = sum(coffee$cf_n)-1
cf_dfg = nrow(coffee)-1
cf_dfe = cf_dft-cf_dfg
cf_xbar = sum(coffee$cf_mean*coffee$cf_n)/sum(coffee$cf_n)
cf_sse = 25564819
cf_sst = 25575327
cf_ssg = sum(coffee$cf_n*(coffee$cf_mean-cf_xbar)^2)

cf_msg = cf_ssg/cf_dfg
cf_mse = cf_sse/cf_dfe

cf_F = cf_msg/cf_mse
cf_pvalue = pf(cf_F, cf_dfg, cf_dfe, lower.tail = FALSE)

coffee_anova = data.frame(Df = c(cf_dfg, cf_dfe, cf_dft),
                          Sum_Sq = c(cf_ssg, cf_sse, cf_sst),
                          Mean_Sq = c(cf_msg, cf_mse, 0),
                          F_value = c(cf_F, 0, 0),
                          Pr = c(cf_pvalue, 0, 0),
                          row.names = c("coffee","residuals", "total")
)

# d) H0 is rejected, i.e., it means that there's a difference between at least one of the groups.

