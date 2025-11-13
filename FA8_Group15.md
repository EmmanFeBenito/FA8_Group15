FA8_Group15
================
Emmanuel Fe Benito
2025-11-13

# Introduction

This report tests whether using an invisibility cloak affects the amount
of mischief committed.  
We conduct an **independent-samples *t*-test** comparing mean Mischief
scores for two groups: 1. Without cloak (`Cloak = 0`) 2. With cloak
(`Cloak = 1`)

------------------------------------------------------------------------

# 1. Setup

``` r
library(tidyverse)
library(car)
```

``` r
cloak <- read.csv("Invisibility Cloak.csv")

head(cloak)
```

    ##   Participant Cloak Mischief
    ## 1           1     0        3
    ## 2           2     0        1
    ## 3           3     0        5
    ## 4           4     0        4
    ## 5           5     0        6
    ## 6           6     0        4

``` r
summary(cloak)
```

    ##   Participant        Cloak        Mischief    
    ##  Min.   : 1.00   Min.   :0.0   Min.   :0.000  
    ##  1st Qu.: 6.75   1st Qu.:0.0   1st Qu.:3.750  
    ##  Median :12.50   Median :0.5   Median :5.000  
    ##  Mean   :12.50   Mean   :0.5   Mean   :4.375  
    ##  3rd Qu.:18.25   3rd Qu.:1.0   3rd Qu.:5.250  
    ##  Max.   :24.00   Max.   :1.0   Max.   :8.000

------------------------------------------------------------------------

# 2. Assumption Checks

## Assumption 1 — Dependent variable is continuous

`Mischief` is a numeric, continuous variable.

``` r
cloak %>%
  group_by(Cloak) %>%
  summarise(
    n = n(),
    mean = mean(Mischief, na.rm = TRUE),
    sd = sd(Mischief, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   Cloak     n  mean    sd
    ##   <int> <int> <dbl> <dbl>
    ## 1     0    12  3.75  1.91
    ## 2     1    12  5     1.65

------------------------------------------------------------------------

## Assumption 2 — Independent variable has two groups

`Cloak` is categorical with two independent levels.

``` r
unique(cloak$Cloak)
```

    ## [1] 0 1

``` r
table(cloak$Cloak)
```

    ## 
    ##  0  1 
    ## 12 12

------------------------------------------------------------------------

## Assumption 3 — Independence of observations

Each participant should belong to only one group.  
→ This is satisfied by design (not statistically testable from the
data).

------------------------------------------------------------------------

## Assumption 4 — Outliers

We identify outliers using the 1.5×IQR rule.

``` r
find_outliers <- function(x){
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5*iqr
  upper <- q3 + 1.5*iqr
  x[x < lower | x > upper]
}

cloak %>%
  group_by(Cloak) %>%
  summarise(outliers = list(find_outliers(Mischief)))
```

    ## # A tibble: 2 × 2
    ##   Cloak outliers 
    ##   <int> <list>   
    ## 1     0 <int [0]>
    ## 2     1 <int [0]>

Visualize:

``` r
boxplot(Mischief ~ Cloak, data=cloak,
        names=c("Without cloak (0)", "With cloak (1)"),
        main="Boxplot of Mischief by Cloak group",
        ylab="Mischief count", col=c("#b2df8a", "#a6cee3"))
```

![](Invisibility_Cloak_TTest_Report_files/figure-gfm/boxplot-1.png)<!-- -->

------------------------------------------------------------------------

## Assumption 5 — Normality

We test each group’s distribution with the Shapiro–Wilk test and plot
histograms.

``` r
by(cloak$Mischief, cloak$Cloak, shapiro.test)
```

    ## cloak$Cloak: 0
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.91276, p-value = 0.2314
    ## 
    ## ------------------------------------------------------------ 
    ## cloak$Cloak: 1
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.97262, p-value = 0.9362

``` r
cloak %>%
  ggplot(aes(x=Mischief, fill=factor(Cloak))) +
  geom_histogram(alpha=0.6, position="identity", bins=10) +
  facet_wrap(~Cloak) +
  theme_minimal() +
  labs(fill="Cloak", title="Histogram of Mischief by Cloak group")
```

![](Invisibility_Cloak_TTest_Report_files/figure-gfm/histograms-1.png)<!-- -->

If both *p* values \> 0.05, normality is not violated.

------------------------------------------------------------------------

## Assumption 6 — Homogeneity of variances

``` r
cloak_dev <- cloak %>%
  group_by(Cloak) %>%
  mutate(dev = abs(Mischief - mean(Mischief))) %>%
  ungroup()

levene <- aov(dev ~ factor(Cloak), data = cloak_dev)
summary(levene)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## factor(Cloak)  1  0.667  0.6667   0.545  0.468
    ## Residuals     22 26.917  1.2235

If *p* \> 0.05, we assume equal variances.

------------------------------------------------------------------------

# 3. Independent Samples *t*-Test

``` r
t_test <- t.test(Mischief ~ Cloak, data=cloak, var.equal=TRUE)
t_test
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  Mischief by Cloak
    ## t = -1.7135, df = 22, p-value = 0.1007
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.7629284  0.2629284
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##            3.75            5.00

``` r
mean_diff <- diff(t_test$estimate)
se_diff <- t_test$stderr

cat("\nMean Difference:", round(mean_diff, 4))
```

    ## 
    ## Mean Difference: 1.25

``` r
cat("\nStandard Error Difference:", round(se_diff, 4))
```

    ## 
    ## Standard Error Difference: 0.7295

------------------------------------------------------------------------

# 4. Interpretation

Levene’s test shows p \> 0.05, so equal variances are assumed.

The independent samples t-test shows p \> 0.05, so we fail to reject the
null hypothesis.

There is no significant difference in the mean Mischief between those
with and without the cloak.

Both groups appear approximately normal, and no extreme outliers were
found.

------------------------------------------------------------------------

# 6. Conclusion

The invisibility cloak did **not** significantly change the average
amount of mischief committed under these data (α = 0.05).  
The assumptions of the independent-samples t-test were reasonably met,
and the data support the conclusion that both groups have similar mean
Mischief scores.

------------------------------------------------------------------------
