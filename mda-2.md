Data-Analysis-Milestone 2
================

``` r
library(datateachr)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'purrr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'stringr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

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

# Task 1

## Research Questions:

Here are the research questions I identified in milestone 1. I have
reworked them slightly to better fit the tasks of this milestone:

1)  What is the relationship between flow and season.

2)  Can we accurately predict flow from month alone?

3)  What effect does month have on extreme\_type?

4)  What is the relationship between year and flow?

## Task 1.2

### Exploring question 1

First I will compute the range, mean, sd and variance for flow
(numerical) across season (catagorical)

``` r
group_by(flow_sample, season) %>%
  summarize(m = mean(flow), r =range(flow), sd = sd(flow), var = var(flow))
```

    ## `summarise()` has grouped output by 'season'. You can override using the `.groups` argument.

    ## # A tibble: 10 x 5
    ## # Groups:   season [5]
    ##    season      m      r     sd      var
    ##    <fct>   <dbl>  <dbl>  <dbl>    <dbl>
    ##  1 Autumn   6.11   5.21  0.707    0.500
    ##  2 Autumn   6.11   7.16  0.707    0.500
    ##  3 Spring  55.5    4.14 85.6   7327.   
    ##  4 Spring  55.5  289    85.6   7327.   
    ##  5 Summer 215.   107    64.1   4111.   
    ##  6 Summer 215.   466    64.1   4111.   
    ##  7 Winter   6.21   3.62  0.966    0.934
    ##  8 Winter   6.21   8.41  0.966    0.934
    ##  9 <NA>    NA     NA    NA       NA    
    ## 10 <NA>    NA     NA    NA       NA

Now I create a graph .

### Exploring question 2

I compute the range, mean, median, and sd across month

``` r
group_by(flow_sample, month) %>%
  summarize(m = mean(flow), r =range(flow), sd = sd(flow), var = var(flow))
```

    ## `summarise()` has grouped output by 'month'. You can override using the `.groups` argument.

    ## # A tibble: 22 x 5
    ## # Groups:   month [11]
    ##    month      m      r     sd      var
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

### Exploring question 3

I compute the number of observations for `extreme_type` (catagorical)

``` r
flow_sample %>%
  count(extreme_type)
```

    ## # A tibble: 2 x 2
    ##   extreme_type     n
    ##   <chr>        <int>
    ## 1 maximum        109
    ## 2 minimum        109

Now I graph extreme and month

### question 4

I will create a new (ordinal) categorical variable called decades.

``` r
flow_sample <- flow_sample %>% 
  mutate(decades = case_when(
    year > 1900 & year < 1930 ~ "1900-1929",
    year > 1929 & year < 1960 ~ "1930-1959",
    year > 1959 & year < 1990  ~ "1960-1989",
    year > 1989 & year < 2020 ~ "1990-2019",
    TRUE ~ as.character(year)
  ) %>% as.factor()
  )
```

Now I will create a graph of flow and decades

``` r
flow_sample %>% 
  group_by(decades) %>% 
   
  ggplot() +
  aes(x=decades, y=flow) +
  geom_line() +
  geom_point() +
  labs(
    title = "Flow within decades",
    x = "Decades",
    y = "Flow"
  )  
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](mda-2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Task 1.3

### Revised questions

Is the downward trend across `decades` statistically significant?

# Task 2

Here I will tidy the data. Lets take a quick look at `flow_sample`. This
is the before

``` r
head(flow_sample)
```

    ## # A tibble: 6 x 9
    ##   station_id  year extreme_type month   day  flow sym   season decades  
    ##   <chr>      <dbl> <chr>        <dbl> <dbl> <dbl> <chr> <fct>  <fct>    
    ## 1 05BB001     1909 maximum          7     7   314 <NA>  Summer 1900-1929
    ## 2 05BB001     1910 maximum          6    12   230 <NA>  Summer 1900-1929
    ## 3 05BB001     1911 maximum          6    14   264 <NA>  Summer 1900-1929
    ## 4 05BB001     1912 maximum          8    25   174 <NA>  Summer 1900-1929
    ## 5 05BB001     1913 maximum          6    11   232 <NA>  Summer 1900-1929
    ## 6 05BB001     1914 maximum          6    18   214 <NA>  Summer 1900-1929

## Task 2.1

### Cloumn 1 `Year`

I will unite the year, month, and day to make date.

``` r
(flow_sample1 <- flow_sample %>%
    unite(col = "date", c(year, month, day), sep = "-"))
```

    ## # A tibble: 218 x 7
    ##    station_id date      extreme_type  flow sym   season decades  
    ##    <chr>      <chr>     <chr>        <dbl> <chr> <fct>  <fct>    
    ##  1 05BB001    1909-7-7  maximum        314 <NA>  Summer 1900-1929
    ##  2 05BB001    1910-6-12 maximum        230 <NA>  Summer 1900-1929
    ##  3 05BB001    1911-6-14 maximum        264 <NA>  Summer 1900-1929
    ##  4 05BB001    1912-8-25 maximum        174 <NA>  Summer 1900-1929
    ##  5 05BB001    1913-6-11 maximum        232 <NA>  Summer 1900-1929
    ##  6 05BB001    1914-6-18 maximum        214 <NA>  Summer 1900-1929
    ##  7 05BB001    1915-6-27 maximum        236 <NA>  Summer 1900-1929
    ##  8 05BB001    1916-6-20 maximum        309 <NA>  Summer 1900-1929
    ##  9 05BB001    1917-6-17 maximum        174 <NA>  Summer 1900-1929
    ## 10 05BB001    1918-6-15 maximum        345 <NA>  Summer 1900-1929
    ## # ... with 208 more rows

``` r
print(flow_sample1)
```

    ## # A tibble: 218 x 7
    ##    station_id date      extreme_type  flow sym   season decades  
    ##    <chr>      <chr>     <chr>        <dbl> <chr> <fct>  <fct>    
    ##  1 05BB001    1909-7-7  maximum        314 <NA>  Summer 1900-1929
    ##  2 05BB001    1910-6-12 maximum        230 <NA>  Summer 1900-1929
    ##  3 05BB001    1911-6-14 maximum        264 <NA>  Summer 1900-1929
    ##  4 05BB001    1912-8-25 maximum        174 <NA>  Summer 1900-1929
    ##  5 05BB001    1913-6-11 maximum        232 <NA>  Summer 1900-1929
    ##  6 05BB001    1914-6-18 maximum        214 <NA>  Summer 1900-1929
    ##  7 05BB001    1915-6-27 maximum        236 <NA>  Summer 1900-1929
    ##  8 05BB001    1916-6-20 maximum        309 <NA>  Summer 1900-1929
    ##  9 05BB001    1917-6-17 maximum        174 <NA>  Summer 1900-1929
    ## 10 05BB001    1918-6-15 maximum        345 <NA>  Summer 1900-1929
    ## # ... with 208 more rows

Now I separate them again.

``` r
(flow_sample1.1 <- flow_sample1 %>%
    separate(date, into = c("year", "month", "day"), sep = ""))
```

    ## Warning: Expected 3 pieces. Additional pieces discarded in 218 rows [1, 2, 3, 4,
    ## 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

    ## # A tibble: 218 x 9
    ##    station_id year  month day   extreme_type  flow sym   season decades  
    ##    <chr>      <chr> <chr> <chr> <chr>        <dbl> <chr> <fct>  <fct>    
    ##  1 05BB001    ""    1     9     maximum        314 <NA>  Summer 1900-1929
    ##  2 05BB001    ""    1     9     maximum        230 <NA>  Summer 1900-1929
    ##  3 05BB001    ""    1     9     maximum        264 <NA>  Summer 1900-1929
    ##  4 05BB001    ""    1     9     maximum        174 <NA>  Summer 1900-1929
    ##  5 05BB001    ""    1     9     maximum        232 <NA>  Summer 1900-1929
    ##  6 05BB001    ""    1     9     maximum        214 <NA>  Summer 1900-1929
    ##  7 05BB001    ""    1     9     maximum        236 <NA>  Summer 1900-1929
    ##  8 05BB001    ""    1     9     maximum        309 <NA>  Summer 1900-1929
    ##  9 05BB001    ""    1     9     maximum        174 <NA>  Summer 1900-1929
    ## 10 05BB001    ""    1     9     maximum        345 <NA>  Summer 1900-1929
    ## # ... with 208 more rows

``` r
print(flow_sample1.1)
```

    ## # A tibble: 218 x 9
    ##    station_id year  month day   extreme_type  flow sym   season decades  
    ##    <chr>      <chr> <chr> <chr> <chr>        <dbl> <chr> <fct>  <fct>    
    ##  1 05BB001    ""    1     9     maximum        314 <NA>  Summer 1900-1929
    ##  2 05BB001    ""    1     9     maximum        230 <NA>  Summer 1900-1929
    ##  3 05BB001    ""    1     9     maximum        264 <NA>  Summer 1900-1929
    ##  4 05BB001    ""    1     9     maximum        174 <NA>  Summer 1900-1929
    ##  5 05BB001    ""    1     9     maximum        232 <NA>  Summer 1900-1929
    ##  6 05BB001    ""    1     9     maximum        214 <NA>  Summer 1900-1929
    ##  7 05BB001    ""    1     9     maximum        236 <NA>  Summer 1900-1929
    ##  8 05BB001    ""    1     9     maximum        309 <NA>  Summer 1900-1929
    ##  9 05BB001    ""    1     9     maximum        174 <NA>  Summer 1900-1929
    ## 10 05BB001    ""    1     9     maximum        345 <NA>  Summer 1900-1929
    ## # ... with 208 more rows

### Column 2 `extreme_type`

I am going to widen the data so we see the flow of each `extreme_type`
by making `extreme_type` it’s own column.

``` r
(flow_sample2 <- flow_sample %>%
  pivot_wider(id_cols = c(-extreme_type, -flow),
              names_from = extreme_type,
              values_from = flow))
```

    ## # A tibble: 218 x 9
    ##    station_id  year month   day sym   season decades   maximum minimum
    ##    <chr>      <dbl> <dbl> <dbl> <chr> <fct>  <fct>       <dbl>   <dbl>
    ##  1 05BB001     1909     7     7 <NA>  Summer 1900-1929     314      NA
    ##  2 05BB001     1910     6    12 <NA>  Summer 1900-1929     230      NA
    ##  3 05BB001     1911     6    14 <NA>  Summer 1900-1929     264      NA
    ##  4 05BB001     1912     8    25 <NA>  Summer 1900-1929     174      NA
    ##  5 05BB001     1913     6    11 <NA>  Summer 1900-1929     232      NA
    ##  6 05BB001     1914     6    18 <NA>  Summer 1900-1929     214      NA
    ##  7 05BB001     1915     6    27 <NA>  Summer 1900-1929     236      NA
    ##  8 05BB001     1916     6    20 <NA>  Summer 1900-1929     309      NA
    ##  9 05BB001     1917     6    17 <NA>  Summer 1900-1929     174      NA
    ## 10 05BB001     1918     6    15 <NA>  Summer 1900-1929     345      NA
    ## # ... with 208 more rows

``` r
arrange_all(flow_sample2)
```

    ## # A tibble: 218 x 9
    ##    station_id  year month   day sym   season decades   maximum minimum
    ##    <chr>      <dbl> <dbl> <dbl> <chr> <fct>  <fct>       <dbl>   <dbl>
    ##  1 05BB001     1909     7     7 <NA>  Summer 1900-1929     314   NA   
    ##  2 05BB001     1909    NA    NA <NA>  <NA>   1900-1929      NA   NA   
    ##  3 05BB001     1910     6    12 <NA>  Summer 1900-1929     230   NA   
    ##  4 05BB001     1910    NA    NA <NA>  <NA>   1900-1929      NA   NA   
    ##  5 05BB001     1911     2    27 <NA>  Winter 1900-1929      NA    5.75
    ##  6 05BB001     1911     6    14 <NA>  Summer 1900-1929     264   NA   
    ##  7 05BB001     1912     3    14 <NA>  Spring 1900-1929      NA    5.8 
    ##  8 05BB001     1912     8    25 <NA>  Summer 1900-1929     174   NA   
    ##  9 05BB001     1913     3    18 B     Spring 1900-1929      NA    6.12
    ## 10 05BB001     1913     6    11 <NA>  Summer 1900-1929     232   NA   
    ## # ... with 208 more rows

It is clear that there are many missing values. First I am going to
check how many for maximum and how many for minimum.

``` r
sum(is.na(flow_sample2$maximum))
```

    ## [1] 109

``` r
sum(is.na(flow_sample2$minimum))
```

    ## [1] 111

Most of these missing values are not an issue as only data on one
extreme type was collected per year. I want to see the rows where both
are NA.

``` r
sum(is.na(flow_sample2$maximum) & is.na(flow_sample2$minimum) )
```

    ## [1] 2

There are only 2 rows that have missing values for both extreme types.
Since these rows have no flow information I am going to remove them.

``` r
which(is.na(flow_sample2$maximum) & is.na(flow_sample2$minimum) )
```

    ## [1] 110 111

These are the two rows I will remove.

``` r
flow_sample2.1 <- flow_sample2[-c(110,111),]
arrange_all(flow_sample2.1)
```

    ## # A tibble: 216 x 9
    ##    station_id  year month   day sym   season decades   maximum minimum
    ##    <chr>      <dbl> <dbl> <dbl> <chr> <fct>  <fct>       <dbl>   <dbl>
    ##  1 05BB001     1909     7     7 <NA>  Summer 1900-1929     314   NA   
    ##  2 05BB001     1910     6    12 <NA>  Summer 1900-1929     230   NA   
    ##  3 05BB001     1911     2    27 <NA>  Winter 1900-1929      NA    5.75
    ##  4 05BB001     1911     6    14 <NA>  Summer 1900-1929     264   NA   
    ##  5 05BB001     1912     3    14 <NA>  Spring 1900-1929      NA    5.8 
    ##  6 05BB001     1912     8    25 <NA>  Summer 1900-1929     174   NA   
    ##  7 05BB001     1913     3    18 B     Spring 1900-1929      NA    6.12
    ##  8 05BB001     1913     6    11 <NA>  Summer 1900-1929     232   NA   
    ##  9 05BB001     1914     6    18 <NA>  Summer 1900-1929     214   NA   
    ## 10 05BB001     1914    11    17 <NA>  Autumn 1900-1929      NA    7.16
    ## # ... with 206 more rows

Now I will put it back.

``` r
(flow_sample2.2 <- flow_sample2.1 %>% 
  pivot_longer(cols = c(-station_id, -year, -month, -day, -sym, -season, -decades ), 
               names_to  = "extreme_type", 
               values_to = "flow"))
```

    ## # A tibble: 432 x 9
    ##    station_id  year month   day sym   season decades   extreme_type  flow
    ##    <chr>      <dbl> <dbl> <dbl> <chr> <fct>  <fct>     <chr>        <dbl>
    ##  1 05BB001     1909     7     7 <NA>  Summer 1900-1929 maximum        314
    ##  2 05BB001     1909     7     7 <NA>  Summer 1900-1929 minimum         NA
    ##  3 05BB001     1910     6    12 <NA>  Summer 1900-1929 maximum        230
    ##  4 05BB001     1910     6    12 <NA>  Summer 1900-1929 minimum         NA
    ##  5 05BB001     1911     6    14 <NA>  Summer 1900-1929 maximum        264
    ##  6 05BB001     1911     6    14 <NA>  Summer 1900-1929 minimum         NA
    ##  7 05BB001     1912     8    25 <NA>  Summer 1900-1929 maximum        174
    ##  8 05BB001     1912     8    25 <NA>  Summer 1900-1929 minimum         NA
    ##  9 05BB001     1913     6    11 <NA>  Summer 1900-1929 maximum        232
    ## 10 05BB001     1913     6    11 <NA>  Summer 1900-1929 minimum         NA
    ## # ... with 422 more rows

``` r
arrange_all(flow_sample2.2)
```

    ## # A tibble: 432 x 9
    ##    station_id  year month   day sym   season decades   extreme_type   flow
    ##    <chr>      <dbl> <dbl> <dbl> <chr> <fct>  <fct>     <chr>         <dbl>
    ##  1 05BB001     1909     7     7 <NA>  Summer 1900-1929 maximum      314   
    ##  2 05BB001     1909     7     7 <NA>  Summer 1900-1929 minimum       NA   
    ##  3 05BB001     1910     6    12 <NA>  Summer 1900-1929 maximum      230   
    ##  4 05BB001     1910     6    12 <NA>  Summer 1900-1929 minimum       NA   
    ##  5 05BB001     1911     2    27 <NA>  Winter 1900-1929 maximum       NA   
    ##  6 05BB001     1911     2    27 <NA>  Winter 1900-1929 minimum        5.75
    ##  7 05BB001     1911     6    14 <NA>  Summer 1900-1929 maximum      264   
    ##  8 05BB001     1911     6    14 <NA>  Summer 1900-1929 minimum       NA   
    ##  9 05BB001     1912     3    14 <NA>  Spring 1900-1929 maximum       NA   
    ## 10 05BB001     1912     3    14 <NA>  Spring 1900-1929 minimum        5.8 
    ## # ... with 422 more rows

You can see that we once again have variable `extreme_type` with values
minimum and maximum.

### Column 3 `month`

### Column 4 `day`

### Column 5 `flow`

### Column 6 `sym`

First lets look and see how many missing values there are.

``` r
sum(is.na(flow_sample$sym))
```

    ## [1] 119

That is a lot of missing data. Once again I am going to remove rows 110
and 111.

``` r
flow_sample6 <- flow_sample[-c(110,111),]
arrange_all(flow_sample2.1)
```

    ## # A tibble: 216 x 9
    ##    station_id  year month   day sym   season decades   maximum minimum
    ##    <chr>      <dbl> <dbl> <dbl> <chr> <fct>  <fct>       <dbl>   <dbl>
    ##  1 05BB001     1909     7     7 <NA>  Summer 1900-1929     314   NA   
    ##  2 05BB001     1910     6    12 <NA>  Summer 1900-1929     230   NA   
    ##  3 05BB001     1911     2    27 <NA>  Winter 1900-1929      NA    5.75
    ##  4 05BB001     1911     6    14 <NA>  Summer 1900-1929     264   NA   
    ##  5 05BB001     1912     3    14 <NA>  Spring 1900-1929      NA    5.8 
    ##  6 05BB001     1912     8    25 <NA>  Summer 1900-1929     174   NA   
    ##  7 05BB001     1913     3    18 B     Spring 1900-1929      NA    6.12
    ##  8 05BB001     1913     6    11 <NA>  Summer 1900-1929     232   NA   
    ##  9 05BB001     1914     6    18 <NA>  Summer 1900-1929     214   NA   
    ## 10 05BB001     1914    11    17 <NA>  Autumn 1900-1929      NA    7.16
    ## # ... with 206 more rows

Lets look at the breakdown of the values of this variable.

``` r
table(flow_sample6$sym)
```

    ## 
    ##  A  B  E 
    ##  2 95  2

It looks like most of the values are B with only 4 being divided between
A and E. I am going to change this variable to have values B or other.

``` r
flow_sample6.1 <- flow_sample6 %>% 
  
  mutate(sym = ifelse(is.na(sym), "Other",
                      ifelse(sym == "B", "B", "Other")))

table(flow_sample$sym)
```

    ## 
    ##  A  B  E 
    ##  2 95  2

``` r
head(flow_sample)
```

    ## # A tibble: 6 x 9
    ##   station_id  year extreme_type month   day  flow sym   season decades  
    ##   <chr>      <dbl> <chr>        <dbl> <dbl> <dbl> <chr> <fct>  <fct>    
    ## 1 05BB001     1909 maximum          7     7   314 <NA>  Summer 1900-1929
    ## 2 05BB001     1910 maximum          6    12   230 <NA>  Summer 1900-1929
    ## 3 05BB001     1911 maximum          6    14   264 <NA>  Summer 1900-1929
    ## 4 05BB001     1912 maximum          8    25   174 <NA>  Summer 1900-1929
    ## 5 05BB001     1913 maximum          6    11   232 <NA>  Summer 1900-1929
    ## 6 05BB001     1914 maximum          6    18   214 <NA>  Summer 1900-1929

This should have removed missing values from this variable. Let’s check.

``` r
sum(is.na(flow_sample6.1$sym))
```

    ## [1] 0

Great\! \#\#\# Column 7 `season` First I will create a subset of
`flow_sample` containing only the season `Winter`.

``` r
(flow_sample7 <- flow_sample %>%
   filter(season == "Winter"))
```

    ## # A tibble: 57 x 9
    ##    station_id  year extreme_type month   day  flow sym   season decades  
    ##    <chr>      <dbl> <chr>        <dbl> <dbl> <dbl> <chr> <fct>  <fct>    
    ##  1 05BB001     1911 minimum          2    27  5.75 <NA>  Winter 1900-1929
    ##  2 05BB001     1915 minimum          1    27  6.94 <NA>  Winter 1900-1929
    ##  3 05BB001     1917 minimum          2    23  6.06 B     Winter 1900-1929
    ##  4 05BB001     1918 minimum          2    20  6.03 B     Winter 1900-1929
    ##  5 05BB001     1919 minimum          2    28  4.56 B     Winter 1900-1929
    ##  6 05BB001     1923 minimum         12    20  6.43 E     Winter 1900-1929
    ##  7 05BB001     1925 minimum         12    31  5.44 B     Winter 1900-1929
    ##  8 05BB001     1927 minimum          2    17  6.48 B     Winter 1900-1929
    ##  9 05BB001     1928 minimum         12    29  6.09 B     Winter 1900-1929
    ## 10 05BB001     1930 minimum         12    29  6    B     Winter 1930-1959
    ## # ... with 47 more rows

``` r
print(flow_sample7)
```

    ## # A tibble: 57 x 9
    ##    station_id  year extreme_type month   day  flow sym   season decades  
    ##    <chr>      <dbl> <chr>        <dbl> <dbl> <dbl> <chr> <fct>  <fct>    
    ##  1 05BB001     1911 minimum          2    27  5.75 <NA>  Winter 1900-1929
    ##  2 05BB001     1915 minimum          1    27  6.94 <NA>  Winter 1900-1929
    ##  3 05BB001     1917 minimum          2    23  6.06 B     Winter 1900-1929
    ##  4 05BB001     1918 minimum          2    20  6.03 B     Winter 1900-1929
    ##  5 05BB001     1919 minimum          2    28  4.56 B     Winter 1900-1929
    ##  6 05BB001     1923 minimum         12    20  6.43 E     Winter 1900-1929
    ##  7 05BB001     1925 minimum         12    31  5.44 B     Winter 1900-1929
    ##  8 05BB001     1927 minimum          2    17  6.48 B     Winter 1900-1929
    ##  9 05BB001     1928 minimum         12    29  6.09 B     Winter 1900-1929
    ## 10 05BB001     1930 minimum         12    29  6    B     Winter 1930-1959
    ## # ... with 47 more rows

### Column 8 `decades`

## Task 2.3
