logistic\_regression
================
Robert Tumasian
11/22/2019

``` r
library(tidyverse)

#Data (2014-2018; too many NAs in prior years)
ecig_data_for_regression = 
  read.csv("./ecig_dat.csv") %>%
  mutate(age = as.numeric(age),
         year = as.numeric(year),
         weight = as.numeric(weight),
         sex = factor(sex),
         race = factor(race),
         cig_days = factor(cig_days),
         try_ecigs = factor(try_ecigs),
         quit_cig = factor(quit_cig),
         harm_ecigs = factor(harm_ecigs),
         ads_ecigs = factor(ads_ecigs),
         current_ecigs = factor(current_ecigs)) %>%
  filter(!age %in% c("9", "10", "19"),
         year >= 2014,
         is.na(current_ecigs) != TRUE) %>%
  mutate(id = row_number())
```

``` r
library(arm)

#Logistic regression to predict ecig use (2014-2018)
full_log_model = glm(current_ecigs ~ age + year + sex + 
                       relevel(race, ref = "white") +
                       cig_days + 
                       relevel(harm_ecigs, ref = "never heard of e-cigs") + 
                       relevel(ads_ecigs, ref = "never"),
                     family = binomial,
                     data = ecig_data_for_regression)
```

``` r
#Table of beta estimates, SE, ORs, CIs, and p-values
log_model_summary = 
  tibble(
  "Variable" = c("intercept", "age", "year", "sex (male)", "race (Indian/Native)", "race (Asian)", "race (Black)", 
               "race (Hispanic)", "race (Haiwaiian/Pacific)", "cig_days (1-2)", "cig_days (10-19)",
               "cig_days (20-29)", "cig_days (3-5)", "cig_days (30)", "cig_days (6-9)", 
               "harm_ecigs (equally harmful)", "harm_ecigs (less harmful)", "harm_ecigs (more harmful)",
               "harm_ecigs (unknown)", "ads_ecigs (always)", "ads_ecigs (do not use Internet)",
               "ads_ecigs (most of the time)", "ads_ecigs (rarely)", "ads_ecigs (sometimes)"),
  "Estimate" = round(summary(full_log_model)$coefficients[, 1], digits = 2),
  "SE" = round(summary(full_log_model)$coefficients[, 2], digits = 2),
  "OR" = round(exp(coef(full_log_model)), digits = 2),
  "OR CI (2.5%)" = round(exp(confint(full_log_model))[, 1], digits = 2),
  "OR CI (97.5%)" = round(exp(confint(full_log_model))[, 2], digits = 2),
  "P-value" = round(summary(full_log_model)$coefficients[, 4], digits = 2))
  
log_model_summary %>% knitr::kable()
```

| Variable                         | Estimate |    SE |    OR | OR CI (2.5%) | OR CI (97.5%) | P-value |
| :------------------------------- | -------: | ----: | ----: | -----------: | ------------: | ------: |
| intercept                        | \-402.83 | 19.83 |  0.00 |         0.00 |          0.00 |    0.00 |
| age                              |     0.22 |  0.01 |  1.24 |         1.23 |          1.26 |    0.00 |
| year                             |     0.20 |  0.01 |  1.22 |         1.19 |          1.24 |    0.00 |
| sex (male)                       |     0.21 |  0.03 |  1.23 |         1.16 |          1.30 |    0.00 |
| race (Indian/Native)             |     0.15 |  0.11 |  1.16 |         0.93 |          1.43 |    0.17 |
| race (Asian)                     |   \-0.78 |  0.09 |  0.46 |         0.38 |          0.55 |    0.00 |
| race (Black)                     |   \-0.78 |  0.05 |  0.46 |         0.41 |          0.51 |    0.00 |
| race (Hispanic)                  |   \-0.06 |  0.03 |  0.94 |         0.88 |          1.00 |    0.05 |
| race (Haiwaiian/Pacific)         |   \-0.11 |  0.19 |  0.90 |         0.61 |          1.28 |    0.57 |
| cig\_days (1-2)                  |     2.11 |  0.06 |  8.22 |         7.37 |          9.16 |    0.00 |
| cig\_days (10-19)                |     2.58 |  0.11 | 13.24 |        10.71 |         16.44 |    0.00 |
| cig\_days (20-29)                |     2.35 |  0.12 | 10.47 |         8.22 |         13.38 |    0.00 |
| cig\_days (3-5)                  |     2.50 |  0.09 | 12.21 |        10.17 |         14.68 |    0.00 |
| cig\_days (30)                   |     2.35 |  0.09 | 10.47 |         8.77 |         12.51 |    0.00 |
| cig\_days (6-9)                  |     2.46 |  0.12 | 11.70 |         9.32 |         14.73 |    0.00 |
| harm\_ecigs (equally harmful)    |     0.44 |  0.14 |  1.56 |         1.20 |          2.06 |    0.00 |
| harm\_ecigs (less harmful)       |     1.62 |  0.14 |  5.06 |         3.90 |          6.67 |    0.00 |
| harm\_ecigs (more harmful)       |     0.87 |  0.14 |  2.40 |         1.82 |          3.20 |    0.00 |
| harm\_ecigs (unknown)            |   \-0.76 |  0.15 |  0.47 |         0.35 |          0.63 |    0.00 |
| ads\_ecigs (always)              |     1.22 |  0.07 |  3.39 |         2.96 |          3.89 |    0.00 |
| ads\_ecigs (do not use Internet) |     0.69 |  0.08 |  1.99 |         1.71 |          2.32 |    0.00 |
| ads\_ecigs (most of the time)    |     1.09 |  0.06 |  2.98 |         2.66 |          3.33 |    0.00 |
| ads\_ecigs (rarely)              |     0.35 |  0.04 |  1.41 |         1.30 |          1.54 |    0.00 |
| ads\_ecigs (sometimes)           |     0.59 |  0.04 |  1.80 |         1.65 |          1.96 |    0.00 |

``` r
##OR plot
ggplot(log_model_summary, aes(x = Variable, y = OR)) +
  geom_pointrange(ymin = log_model_summary$`OR CI (2.5%)`, ymax = log_model_summary$`OR CI (97.5%)`) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  ylim(c(0, 17)) +
  coord_flip() +
  labs(title = "95% OR CIs")
```

![](logistic_regression_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
