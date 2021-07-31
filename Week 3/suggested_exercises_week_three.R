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

# c) HA: mu != mU0, n = 7, T = 0.83

n = 7
df = n-1
t = .83
pvalue = pt(q = t, df = df, lower.tail = FALSE)*2
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



## 5.6.2 Paired Data ---------------------------------------

# 5.17 Paired or Not, Part I
# a) Yes, paired.
# b) Not paired.
# c) Paired
# d) Paired.

# 5.19 Global Warming, Part I
# a) The observations aren't independent, since the same cities
# were observed.
# b) H0: temp_68 - temp_08 = 0, HA: temp_68 - temp_08 < 0
# c) Observations in the groups are independent, since the cities were randomly
# selected, and we expected the sample to not be too asymmetric.
# d)

n = 51
sd_temp = 4.9
mean_temp = 1.1
df = n-1
se_temp = sd_temp/sqrt(n)
se_temp
t_temp = (mean_temp - 0)/se_temp
t_temp

pvalue = pt(t_temp,df,lower.tail = FALSE)
pvalue

# e) Since the p-value was higher than 5% (significance level), we cannot reject
# H0, i.e., these data don't provide sufficient evidence that temperatures were
# warming in continental US.

# f) It's possible that we've made a type II error, for not rejecting H0 when
# it's false.

# g) Since we failed to reject H0, which assumes that the true average difference is 0,
# we can expect 0 to be in the interval.

# 5.21 Global Warming, Part II
#a)

n = 51
sd_temp = 4.9
mean_temp = 1.1
df = n-1

se_temp = sd_temp/sqrt(n)
se_temp

cl = 0.9
sl = 1-cl

tcrit_temp = qt(p = sl/2, df = df, lower.tail = FALSE)
tcrit_temp

lower = mean_temp - tcrit_temp*se_temp
lower
upper = mean_temp + tcrit_temp*se_temp
upper

# 5.23 Gifted children
# a) They might be related, since it may influence how the parents met, or were selected.
# b) 1. The sample was random, from less than 10% of the population but at least 30 2. We assume that the sample is enough for the city.
# H0: mean_diff = 0, HA: mean_diff != 0.

mean_diff = 3.4
sd_diff = 7.5
n = 36

df = n-1
se = sd_diff/sqrt(n)
t_diff = mean_diff/se
t_diff

pvalue_diff = pt(t_diff, df = df, lower.tail = FALSE)*2
pvalue_diff

# Since p-value was relatively small (0.01, which is lower than 0.05), we can reject H0,
# thus concluding, with 95% confidence, that there is a difference between IQs for mothers
# and fathers of gifted children.

## 5.6.3 Difference of two means ---------------------------------------

# 5.27 Friday the 13th, Part I

# a) No idea.
# b) H0: mean_6th - mean_13th = 0, HA: mean_6th - mean_13th != 0
# c) We can assume that the chosen dates were random. The distribution of cars appears symmetrical.
# d)

mean_6th = 128385
s_6th = 7259
n = 10
mean_13th = 126550
s_13th = 7664

se_friday = sqrt((s_6th)^2/n+(s_13th)^2/n)
t_friday = (mean_6th-mean_13th)/se_friday
t_friday

pvalue_friday = pt(q = t_friday, df = n-1, lower.tail = FALSE)*2
pvalue_friday

# e) Since the p-value was high (0.6), we can't reject H0, which means that there's
# not strong enough evidence to support a difference in traffic flow for the dates.

# f) p-value was more than 5%.

# g) Type II error might have happened, for not rejecting H0 when it's false.

# 5.31 Chicken diet and weight, Part I
# a) the distributions appear to be approximately normal, and not skewed, with a single peak each.
# b)

mean_hb = 160.2
s_hb = 38.63
n_hb = 10
mean_ls = 218.75
s_ls = 52.24
n_ls = 12

alpha = 0.05

se_chicken = sqrt((s_hb)^2/n_hb+(s_ls)^2/n_ls)
t_chicken = (mean_hb-mean_ls)/se_chicken
t_chicken

pvalue_chicken = pt(q = t_chicken, df = n_hb-1, lower.tail = TRUE)*2
pvalue_chicken
# Yes, since p-value is small (0.015), there's strong evidence that the chickens are different.

# c) Type 1, since we have rejected H0, but it may be true.
# d) Yes, since the p-value was larger than 0.01.

# 5.35 Gaming and distracted earting, part I.
n = 22
mean_treat = 52.1
sd_treat = 45.1
mean_control = 27.1
sd_control = 26.4

se_eat = sqrt((sd_treat)^2/n+(sd_control)^2/n)
t_eat = (mean_treat-mean_control)/se_eat
t_eat

pvalue_eat = pt(q = t_eat, df = n-1, lower.tail = FALSE)*2
pvalue_eat

# Yes. We can reject H0 with 95% confidence, which means that the means were different for each group.

# 5.37 
# H0: mean = 0, HA: mean > 0

n = 14
mean_t1 = 6.21
sd_t1 = 12.3

se_t1 = sd_t1/sqrt(n)
t_t1 = mean_t1/se_t1
t_t1

pvalue_t1 = pt(q = t_t1, df = n-1, lower.tail = FALSE)
pvalue_t1

# H0 not rejected with 95% confidence for Tr 1.

n = 14
mean_t2 = 2.86
sd_t2 = 7.94

se_t2 = sd_t2/sqrt(n)
t_t2 = mean_t2/se_t2
t_t2

pvalue_t2 = pt(q = t_t2, df = n-1, lower.tail = FALSE)
pvalue_t2

# H0 not rejected with 95% confidence for Tr 2. Treatment failed

n = 14
mean_t3 = -3.21
sd_t3 = 8.57

se_t3 = sd_t3/sqrt(n)
t_t3 = mean_t3/se_t3
t_t3

pvalue_t3 = pt(q = t_t3, df = n-1, lower.tail = TRUE)
pvalue_t3

# H0 not rejected with 95% confidence for Tr 3. Treatment Failed

## 5.6.4 Power calculations ---------------------------------------

# 5.39 Increasing the corn yield

corn_mean = 1215
corn_sd = 94
corn_diff = 40
corn_power = 0.9
alpha = 0.05
z_diff = qnorm(p = .9)
z_diff
z_0 = -qnorm(p = alpha/2)
z_0
z_total = z_diff+z_0
z_total
corn_se = corn_diff/z_total
corn_se
n = (corn_sd^2+corn_sd^2)/corn_se^2
n
n = 2*ceiling(n)
n

## 5.6.5 Comparing means with ANOVA ---------------------------------------

# 5.41 Fill in the blank. Evidence strongly favoring the alternative hypothesis.

# 5.43 Chicken diet and weight, Part III.
# H0: All means are equal. HA: At least one pair of means differs from each other.
# The test results (p-value ~ 0) shows that  H0 is rejected, i.e., at least one pair differs.

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


# 5.47 GPA and major.
# a) H0: There's no difference between GPA of each major. H1: At least one pair of major have a difference in GPA.
# b) The p-value is high, concluding that H0 can't be rejected, which means that there's no difference between GPA in majors.
# c) 198 (dfe+dfg+1)

# 5.49 True/False
# a) False.
# b) True.
# c) True.
# d) False

# 5.51 Prison isolation experiment, Part II
# a) H0: There's no difference in the treatment results between treatments. H1: at least one pair of treatments differ from each other.
# b) H0 is rejected, which means that at least one pair of treatments differ.
# c) 

treatments = data.frame(tr_mean = c(6.21, 2.86, -3.21),
                        tr_sd = c(12.3, 7.94, 8.57),
                        tr_n = c(14, 14, 14),
                        row.names = (c("Tr 1", "Tr 2", "Tr 3")))

tr_MSE = 95.91
tr_alpha = 0.05
k = nrow(treatments)
tr_K = k*(k-1)/2 
tr_alpha_star = tr_alpha/tr_K
tr_alpha_star

tr_SE = sqrt(tr_MSE/14)
tr_SE

tr_df = 39

t_t1_t2 = (6.21-2.86)/tr_SE
t_t1_t2

pvalue_t1_t2 = pt(q = t_t1_t2, df = tr_df, lower.tail = FALSE)*2
pvalue_t1_t2

t_t1_t3 = (6.21+3.21)/tr_SE
t_t1_t3

pvalue_t1_t3 = pt(q = t_t1_t3, df = tr_df, lower.tail = FALSE)*2
pvalue_t1_t3

t_t2_t3 = (2.86+3.21)/tr_SE
t_t2_t3

pvalue_t2_t3 = pt(q = t_t2_t3, df = tr_df, lower.tail = FALSE)*2
pvalue_t2_t3

pvalues = data.frame(p_values = c(pvalue_t1_t2, pvalue_t1_t3, pvalue_t2_t3))

# Groups 1 and 3 are statistically different.
