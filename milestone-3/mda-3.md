Mini Data-Analysis Deliverable 3
================

# Welcome to your last milestone in your mini data analysis project\!

In Milestone 1, you explored your data and came up with research
questions. In Milestone 2, you obtained some results by making summary
tables and graphs.

In this (3rd) milestone, you’ll be sharpening some of the results you
obtained from your previous milestone by:

  - Manipulating special data types in R: factors and/or dates and
    times.
  - Fitting a model object to your data, and extract a result.
  - Reading and writing data as separate files.

**NOTE**: The main purpose of the mini data analysis is to integrate
what you learn in class in an analysis. Although each milestone provides
a framework for you to conduct your analysis, it’s possible that you
might find the instructions too rigid for your data set. If this is the
case, you may deviate from the instructions – just make sure you’re
demonstrating a wide range of tools and techniques taught in this class.

## Instructions

**To complete this milestone**, edit [this very `.Rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-3.Rmd)
directly. Fill in the sections that are tagged with `<!--- start your
work here--->`.

**To submit this milestone**, make sure to knit this `.Rmd` file to an
`.md` file by changing the YAML output settings from `output:
html_document` to `output: github_document`. Commit and push all of your
work to your mini-analysis GitHub repository, and tag a release on
GitHub. Then, submit a link to your tagged release on canvas.

**Points**: This milestone is worth 40 points (compared to the usual 30
points): 30 for your analysis, and 10 for your entire mini-analysis
GitHub repository. Details follow.

**Research Questions**: In Milestone 2, you chose two research questions
to focus on. Wherever realistic, your work in this milestone should
relate to these research questions whenever we ask for justification
behind your work. In the case that some tasks in this milestone don’t
align well with one of your research questions, feel free to discuss
your results in the context of a different research question.

# Setup

Begin by loading your data and the tidyverse package below:

``` r
library(datateachr) # <- might contain the data you picked!
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'purrr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'stringr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.0.5

``` r
flow_sample <- flow_sample %>% 
  mutate(season = case_when(
    month > 5 & month < 9 ~ "Summer",
    month > 2 & month < 6 ~ "Spring",
    (month > 0 & month < 3) | month == 12  ~ "Winter",
    month > 8 & month < 12 ~ "Autumn",
    TRUE ~ as.character(month)
  ) %>% as.factor()
  )
```

``` r
flow_sampleTR <-flow_sample[-c(110,111),] %>%
  filter(extreme_type == "maximum") %>%
select(-c(station_id, sym)) %>%
  select(year, season, flow, month, day, extreme_type)


arrange_all(flow_sampleTR)
```

    ## # A tibble: 109 x 6
    ##     year season  flow month   day extreme_type
    ##    <dbl> <fct>  <dbl> <dbl> <dbl> <chr>       
    ##  1  1909 Summer   314     7     7 maximum     
    ##  2  1910 Summer   230     6    12 maximum     
    ##  3  1911 Summer   264     6    14 maximum     
    ##  4  1912 Summer   174     8    25 maximum     
    ##  5  1913 Summer   232     6    11 maximum     
    ##  6  1914 Summer   214     6    18 maximum     
    ##  7  1915 Summer   236     6    27 maximum     
    ##  8  1916 Summer   309     6    20 maximum     
    ##  9  1917 Summer   174     6    17 maximum     
    ## 10  1918 Summer   345     6    15 maximum     
    ## # ... with 99 more rows

From Milestone 2, you chose two research questions. What were they? Put
them here.

<!-------------------------- Start your work below ---------------------------->

1.  There is obviously strong relationships between time of year
    (`month` and `season`), `extreme_type` and `flow`, but is one
    significantly better for predicting flow?

2.  Is the variation in flow year to year associated with the month in
    which flow was collected?
    <!----------------------------------------------------------------------------->

# Exercise 1: Special Data Types (10)

For this exercise, you’ll be choosing two of the three tasks below –
both tasks that you choose are worth 5 points each.

But first, tasks 1 and 2 below ask you to modify a plot you made in a
previous milestone. The plot you choose should involve plotting across
at least three groups (whether by facetting, or using an aesthetic like
colour). Place this plot below (you’re allowed to modify the plot if
you’d like). If you don’t have such a plot, you’ll need to make one.
Place the code for your plot below.

<!-------------------------- Start your work below ---------------------------->

``` r
flow_sampleTR %>% 
  filter(!is.na(season)) %>% 
  group_by(year, season) %>% 
  summarise(cumflow = sum(flow, na.rm = T)) %>% 
  ggplot() +
  aes(x=year, y=cumflow) +
  facet_wrap(vars(season), scales = "free") +
  geom_line() +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Flow across the years",
    subtitle = "by season",
    x = "Year",
    y = "Cumulative Flow"
  )  
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](mda-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

<!----------------------------------------------------------------------------->

Now, choose two of the following tasks.

1.  Produce a new plot that reorders a factor in your original plot,
    using the `forcats` package (3 points). Then, in a sentence or two,
    briefly explain why you chose this ordering (1 point here for
    demonstrating understanding of the reordering, and 1 point for
    demonstrating some justification for the reordering, which could be
    subtle or speculative.)

2.  Produce a new plot that groups some factor levels together into an
    “other” category (or something similar), using the `forcats`
    package (3 points). Then, in a sentence or two, briefly explain why
    you chose this grouping (1 point here for demonstrating
    understanding of the grouping, and 1 point for demonstrating some
    justification for the grouping, which could be subtle or
    speculative.)

3.  If your data has some sort of time-based column like a date (but
    something more granular than just a year):
    
    1.  Make a new column that uses a function from the `lubridate` or
        `tsibble` package to modify your original time-based column. (3
        points)
          - Note that you might first have to *make* a time-based column
            using a function like `ymd()`, but this doesn’t count.
          - Examples of something you might do here: extract the day of
            the year from a date, or extract the weekday, or let 24
            hours elapse on your dates.
    2.  Then, in a sentence or two, explain how your new column might be
        useful in exploring a research question. (1 point for
        demonstrating understanding of the function you used, and 1
        point for your justification, which could be subtle or
        speculative).
          - For example, you could say something like “Investigating the
            day of the week might be insightful because penguins don’t
            work on weekends, and so may respond differently”.

<!-------------------------- Start your work below ---------------------------->

**Task Number**: 1

``` r
flow_sampleTR %>% 
  mutate(season = fct_relevel(season, 
                              c('Autumn', 'Winter', 'Spring', 'Summer'))) %>%
  filter(!is.na(season)) %>% 
  group_by(year, season) %>% 
  summarise(cumflow = sum(flow, na.rm = T)) %>% 
  ggplot() +
  aes(x=year, y=cumflow) +
  facet_wrap(vars(season), scales = "free") +
  geom_line() +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Flow across the years",
    subtitle = "by season",
    x = "Year",
    y = "Cumulative Flow"
  )  
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](mda-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

<!----------------------------------------------------------------------------->

<!-------------------------- Start your work below ---------------------------->

**Task Number**: 3

``` r
flow_sample1 = flow_sampleTR %>% 
  unite(col = "date", c(year, month, day), sep = "-") %>% 
  mutate(date = ymd(date))
```

I chose to investigate because.

<!----------------------------------------------------------------------------->

# Exercise 2: Modelling

## 2.0 (no points)

Pick a research question, and pick a variable of interest (we’ll call it
“Y”) that’s relevant to the research question. Indicate these.

<!-------------------------- Start your work below ---------------------------->

**Research Question**: Is there a significant difference of flow average
among the seasons?

**Variable of interest**: `Flow`

<!----------------------------------------------------------------------------->

## 2.1 (5 points)

Fit a model or run a hypothesis test that provides insight on this
variable with respect to the research question. Store the model object
as a variable, and print its output to screen. We’ll omit having to
justify your choice, because we don’t expect you to know about model
specifics in STAT 545.

  - **Note**: It’s OK if you don’t know how these models/tests work.
    Here are some examples of things you can do here, but the sky’s the
    limit.
      - You could fit a model that makes predictions on Y using another
        variable, by using the `lm()` function.
      - You could test whether the mean of Y equals 0 using `t.test()`,
        or maybe the mean across two groups are different using
        `t.test()`, or maybe the mean across multiple groups are
        different using `anova()` (you may have to pivot your data for
        the latter two).
      - You could use `lm()` to test for significance of regression.

<!-------------------------- Start your work below ---------------------------->

## Analysis of variance (ANOVA TEST)

``` r
anova_model <- aov(flow ~ season, data=flow_sampleTR)
summary(anova_model)
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## season        1   6469    6469   1.711  0.194
    ## Residuals   107 404456    3780

``` r
ggplot(data=flow_sampleTR, aes(x=season, y=flow, fill=season)) +
  geom_bar(stat="summary", fun.y="mean") +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=.4) 
```

    ## Warning: Ignoring unknown parameters: fun.y

    ## No summary function supplied, defaulting to `mean_se()`

![](mda-3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

<!----------------------------------------------------------------------------->

## 2.2 (5 points)

Produce something relevant from your fitted model: either predictions on
Y, or a single value like a regression coefficient or a p-value.

  - Be sure to indicate in writing what you chose to produce.
  - Your code should either output a tibble (in which case you should
    indicate the column that contains the thing you’re looking for), or
    the thing you’re looking for itself.
  - Obtain your results using the `broom` package if possible. If your
    model is not compatible with the broom function you’re needing, then
    you can obtain your results by some other means, but first indicate
    which broom function is not compatible.

<!-------------------------- Start your work below ---------------------------->

``` r
linear_model <- lm(flow ~ season, data=flow_sampleTR)
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = flow ~ season, data = flow_sampleTR)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.269  -44.269   -6.269   30.731  250.731 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    193.50      15.37  12.589   <2e-16 ***
    ## seasonSummer    21.77      16.64   1.308    0.194    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 61.48 on 107 degrees of freedom
    ## Multiple R-squared:  0.01574,    Adjusted R-squared:  0.006544 
    ## F-statistic: 1.711 on 1 and 107 DF,  p-value: 0.1936

### Using `broom` package

``` r
library(broom)
```

    ## Warning: package 'broom' was built under R version 4.0.5

``` r
tidy(linear_model)
```

    ## # A tibble: 2 x 5
    ##   term         estimate std.error statistic  p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)     193.       15.4     12.6  7.61e-23
    ## 2 seasonSummer     21.8      16.6      1.31 1.94e- 1

``` r
glance(linear_model)
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0157       0.00654  61.5      1.71   0.194     1  -603. 1211. 1219.
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

### Checking Residuals

``` r
resid_data.frame = augment(linear_model)
ggplot(resid_data.frame) +
  aes(x=season, y=.resid) +
  facet_wrap(vars(season), scales = "free") +
  geom_boxplot()
```

![](mda-3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

<!----------------------------------------------------------------------------->

# Exercise 3: Reading and writing data

Get set up for this exercise by making a folder called `output` in the
top level of your project folder / repository. You’ll be saving things
there.

## 3.1 (5 points)

Take a summary table that you made from Milestone 2 (Exercise 1.2), and
write it as a csv file in your `output` folder. Use the `here::here()`
function.

  - **Robustness criteria**: You should be able to move your Mini
    Project repository / project folder to some other location on your
    computer, or move this very Rmd file to another location within your
    project repository / folder, and your code should still work.
  - **Reproducibility criteria**: You should be able to delete the csv
    file, and remake it simply by knitting this Rmd file.

<!-------------------------- Start your work below ---------------------------->

``` r
flow_month_stats <- flow_sample %>%
  group_by(month) %>%
  summarise(mean = mean(flow), 
            r =range(flow), 
            sd = sd(flow), 
            var = var(flow))
```

    ## `summarise()` has grouped output by 'month'. You can override using the `.groups` argument.

I am storing this summary table from milestone 2.

``` r
flow_month_stats
```

    ## # A tibble: 22 x 5
    ## # Groups:   month [11]
    ##    month   mean      r     sd      var
    ##    <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
    ##  1     1   6.50   3.62  1.22     1.48 
    ##  2     1   6.50   8.41  1.22     1.48 
    ##  3     2   6.07   4.08  0.956    0.914
    ##  4     2   6.07   7.98  0.956    0.914
    ##  5     3   6.42   4.14  1.03     1.06 
    ##  6     3   6.42   8.44  1.03     1.06 
    ##  7     4   6.17   4.9   0.833    0.694
    ##  8     4   6.17   7.53  0.833    0.694
    ##  9     5 194.   133    41.8   1750.   
    ## 10     5 194.   289    41.8   1750.   
    ## # ... with 12 more rows

``` r
write.csv(flow_month_stats,here::here("Output/table.csv"))
```

<!----------------------------------------------------------------------------->

## 3.2 (5 points)

Write your model object from Exercise 2 to an R binary file (an RDS),
and load it again. Be sure to save the binary file in your `output`
folder. Use the functions `saveRDS()` and `readRDS()`.

  - The same robustness and reproducibility criteria as in 3.1 apply
    here.

<!-------------------------- Start your work below ---------------------------->

``` r
saveRDS(linear_model, here::here("linear_model.RDS"))
```

``` r
my_model_rds <- readRDS(here::here("linear_model.RDS"))
my_model_rds
```

    ## 
    ## Call:
    ## lm(formula = flow ~ season, data = flow_sampleTR)
    ## 
    ## Coefficients:
    ##  (Intercept)  seasonSummer  
    ##       193.50         21.77

<!----------------------------------------------------------------------------->

# Tidy Repository

Now that this is your last milestone, your entire project repository
should be organized. Here are the criteria we’re looking for.

## Main README (3 points)

There should be a file named `README.md` at the top level of your
repository. Its contents should automatically appear when you visit the
repository on GitHub.

Minimum contents of the README file:

  - In a sentence or two, explains what this repository is, so that
    future-you or someone else stumbling on your repository can be
    oriented to the repository.
  - In a sentence or two (or more??), briefly explains how to engage
    with the repository. You can assume the person reading knows the
    material from STAT 545A. Basically, if a visitor to your repository
    wants to explore your project, what should they know?

Once you get in the habit of making README files, and seeing more README
files in other projects, you’ll wonder how you ever got by without
them\! They are tremendously helpful.

## File and Folder structure (3 points)

You should have at least four folders in the top level of your
repository: one for each milestone, and one output folder. If there are
any other folders, these are explained in the main README.

Each milestone document is contained in its respective folder, and
nowhere else.

Every level-1 folder (that is, the ones stored in the top level, like
“Milestone1” and “output”) has a `README` file, explaining in a
sentence or two what is in the folder, in plain language (it’s enough to
say something like “This folder contains the source for Milestone 1”).

## Output (2 points)

All output is recent and relevant:

  - All Rmd files have been `knit`ted to their output, and all data
    files saved from Exercise 3 above appear in the `output` folder.
  - All of these output files are up-to-date – that is, they haven’t
    fallen behind after the source (Rmd) files have been updated.
  - There should be no relic output files. For example, if you were
    knitting an Rmd to html, but then changed the output to be only a
    markdown file, then the html file is a relic and should be deleted.

Our recommendation: delete all output files, and re-knit each
milestone’s Rmd file, so that everything is up to date and relevant.

PS: there’s a way where you can run all project code using a single
command, instead of clicking “knit” three times. More on this in STAT
545B\!

## Error-free code (1 point)

This Milestone 3 document knits error-free. (We’ve already graded this
aspect for Milestone 1 and 2)

## Tagged release (1 point)

You’ve tagged a release for Milestone 3. (We’ve already graded this
aspect for Milestone 1 and 2)
