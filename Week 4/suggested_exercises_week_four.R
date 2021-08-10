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

# 6.23 Social experiment, Part I.
# There's not enough successes under the provocative scenario, which means that
# the sample size isn't large enough to be significant.

# 6.25 Gender and color preference
# a) False.
# b) True.
# c) True.
# d) True.
# e) False.

# 6.27 Public option, Part III.
# a) CI is (23.3%, 32.7%)
p1 = 0.7
n1 = 819
p2 = 0.42
n2 = 783
se = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
se
z = qnorm(0.025, lower.tail = FALSE)
lower = (p1-p2) - z*se
upper = (p1-p2) + z*se
lower
p1-p2
upper

# b) True.

# 6.29 Offshore drilling, Part I
# a) 
p1 = 104/438
p1
n1 = 438
p2 = 131/389
p2
n2 = 389

# b) pvalue is low (5e-6), which means that we can reject H0 and thus conclude
# that there is a difference between the proportions of "Don't know" for graduates.
p_pool = (p1*n1+p2*n2)/(n1+n2)
p_pool

se = sqrt(p_pool*(1-p_pool)/n1 + p_pool*(1-p_pool)/n2)
se

z = (p1-p2)/se
z

pvalue = pnorm(z)
pvalue

# 6.31 Offshore drilling, Part II
# a)
p1 = 154/438
n1 = 438
p2 = 132/389
n2 = 389
p1
p2

# b) pvalue is high (0.64), which means that we can't reject H0 and thus conclude
# that there is no difference between the proportions of "Support" for graduates.
p_pool = (p1*n1+p2*n2)/(n1+n2)
p_pool

se = sqrt(p_pool*(1-p_pool)/n1 + p_pool*(1-p_pool)/n2)
se

z = (p1-p2)/se
z

pvalue = pnorm(z)
pvalue

# 6.33 Sleep deprived transportation workers
# p-value is relatively low (0.049), so we could reject H0 and conclude that there's
# a difference between sleep deprived truck drivers and the control group for a
# SL = 5%. However, we might be making a Type I error since p-value almost equal to the SL.

p1 = 35/292
n1 = 292
p2 = 35/203
n2 = 203
p_pool = (p1*n1+p2*n2)/(n1+n2)
p_pool

se = sqrt(p_pool*(1-p_pool)/n1 + p_pool*(1-p_pool)/n2)

z = (p1-p2)/se

pvalue = pnorm(z)
pvalue

# 6.35 HIV in sub-Saharan Africa.
# a)
n1 = 120
n2 = 120
n = n1+n2
p1 = 26/120
p2 = 10/120

p_pool = (p1*n1+p2*n2)/(n1+n2)

hiv <- tibble(
  result = c("virologic failure", "no symptoms"),
  nevaripine = c(p1*n1, (1-p1)*n1),
  lopinavir = c(p2*n2, (1-p2)*n2),
  )

# b) H0: p1 = p2, HA: p1 != p2
# c)
# Conditions: 1. Independence: random sampling check, n < 10% of population check.
#             2. Sample size: successes and failures over 10 observations in each treatment.

se = sqrt(p_pool*(1-p_pool)/n1 + p_pool*(1-p_pool)/n2)

z = (p1-p2)/se
z
pvalue = pnorm(z, lower.tail = FALSE)*2
pvalue

#pvalue is 0.004, which very LOW. This means that we CAN reject H0, that is,
# nevaripine and lopinavir have different proportions of virologic failure in
# treated women.

## 6.7.3 Testing for goodness of fit using chi-square --------------------------

# 6.39 True or false, part I.
# a) False, chi-square has one parameter: degrees of freedom.
# b) True.
# c) True.
# d) False, it becomes less skewed (more symmetrical).

# 6.41 
# a) H0: there was no difference between the professor's predictions and what the
# students used in class. HA: There was a difference.
# b)
textbook <- tibble(
  format = c("hard copy", "print", "online"),
  observed = c(71, 30, 25),
  expected = c(0.6*126, 0.25*126, 0.15*126)
)

# c) Conditions:
# 1. Independence: random assignment (not check, since they are all from the same class)
# n < 10% of all students, each result is one cell in the table.
# 2. Sample size check (more than 5 expected cases for each scenario)

# d)
chi = sum((textbook$observed-textbook$expected)^2/textbook$expected)
df = (nrow(textbook)-1)*(ncol(textbook)-2)

pchisq(q = chi, df = df, lower.tail = FALSE)

# e) p-value was 0.31, which menas that we can't reject H0. In other words, there's
# no difference between the professor's expectations and the observations.

# 6.43 Rock-paper-scissors
total = 43+21+35
rps <- data.frame(
  observed = c(43, 21, 35),
  expected = c(total/3, total/3, total/3),
  row.names = c("rock", "paper", "scissors")
)
chi = sum((rps$observed-rps$expected)^2/rps$expected)
chi
df = (nrow(rps)-1)*(ncol(rps)-1)
df
pvalue = pchisq(q = chi, df = df, lower.tail = FALSE)
pvalue
# P-value was 0.02, which is lower than 0.05. At a 95% confidence level, we can
# say that the observations were different from expectations.

## 6.7.4 Testing for independence in two-way tables ----------------------------
# 6.45 Quitters
# a)
quitters <- data.frame(
  patch_support = c(40, 150-40),
  no_support = c(30, 150-30),
  row.names = c("quit smoking", "didn't quit smoking")
)

quitters <- quitters %>%
  mutate (total = patch_support + no_support)

# b)

quitters <- quitters %>%
  mutate (patch_support_exp = (sum(patch_support)*total)/sum(total),
          no_support_exp = (sum(no_support)*total)/sum(total))

quitters <- quitters %>%
    mutate(obs_higher_than_exp_PS = ifelse(patch_support>patch_support_exp, "yes", "no"),
           obs_higher_than_exp_NS = ifelse(no_support>no_support_exp, "yes", "no")
           )

# 6.47 Offshore drilling, Part III

drill <- data.frame(
  college_yes = c(154, 180, 104),
  college_no = c(132, 126, 131),
  row.names = c("support", "oppose", "do not know")
)

chisq <- chisq.test(drill)
chisq$observed
round(chisq$expected,2)
chisq$p.value

#p-value is 0.003, which means that we can reject H0, in other words, there is a
# difference in responses from graduates and non-graduates.

## 6.7.5 Small sample hypothesis testing for a proportion ----------------------

# 6.51 Bullying in schools.
# No, it's not. There's only 9 successes in the sample, meaning that the Sample
# Size condition isn't fullfilled.

# 6.53 The Egyptian Revolution.
# a) H0: p_hs = 0.69, HA: p_hs != 0.69
# b)
p_hs = 17/30
p_hs

# c) Supposing the null hypothesis is true, we'd have 21 successes and 9 failures,
# which doesn't fulfill the sample size condition.
succ = 0.69*30
fail = (1-0.69)*30

# d)
pvalue = pbinom(17, 30, 0.69)*2
pvalue

# pvalue is 0.10, which is higher than 0.05 (CL 95%). This means that we can't reject
# H0, or that we can't reject the hypothesis that the proportion of HS students
# who followed news about Egypt is 69%.

# e) ????

#6.55 Social Experiment, Part II
# a) H0: ppr - pcon = 0, HA: ppr - pcon != 0.
# b)
ppr = 5/20
pcon = 15/25
ppr - pcon
# c) estimated pvalue = 0.04
