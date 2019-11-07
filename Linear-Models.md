Linear Models
================
Olya Besedina

# linear models

``` r
set.seed(1)

data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    boro = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(boro != "Staten Island") %>% 
  select(price, stars, boro, neighborhood, room_type)
```

An good place to start is to consider price as an outcome that may
depend on rating and borough. We fit that initial model in the following
code.

``` r
fit = lm(price ~ stars + boro, data = nyc_airbnb)
```

I cat predictors (boro), r assumes that it is a factor abd will orginize
them is some order. If order is not specified than R wil use
alphabetical order - Bronx

``` r
# not very descriptive
fit

# better summary
summary(fit)

# extract the coef
coef(fit)
summary(fit)$coef

# broom::tidy gives you the same thing but orginized in nice tibble

# broom::glance gives you other important info like AIC and BIC
fit %>% 
  broom::glance()

fit %>% 
  broom::tidy()
```

Tidy the results

``` r
fit %>% 
  broom::tidy() %>% 
  #replave default boro with nice looking one
  mutate(
    term = str_replace(term, "boro", "Boro: ")
  ) %>%
  knitr::kable(digits = 3)
```

| term            | estimate | std.error | statistic | p.value |
| :-------------- | -------: | --------: | --------: | ------: |
| (Intercept)     | \-70.414 |    14.021 |   \-5.022 |   0.000 |
| stars           |   31.990 |     2.527 |    12.657 |   0.000 |
| Boro: Brooklyn  |   40.500 |     8.559 |     4.732 |   0.000 |
| Boro: Manhattan |   90.254 |     8.567 |    10.534 |   0.000 |
| Boro: Queens    |   13.206 |     9.065 |     1.457 |   0.145 |

\#Take a look at factors

fct\_infreq - put cat var in order of how often they occur in data set
the most freq boro is manhattan

``` r
nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(
    boro = fct_infreq(boro),
    room_type = fct_infreq(room_type)
  )
```

Refit the last model

``` r
# now ref is manhattan
fit = lm(price ~ stars + boro, data = nyc_airbnb)

fit %>% 
  broom::tidy()
```

    ## # A tibble: 5 x 5
    ##   term         estimate std.error statistic   p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)      19.8     12.2       1.63 1.04e-  1
    ## 2 stars            32.0      2.53     12.7  1.27e- 36
    ## 3 boroBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroBronx       -90.3      8.57    -10.5  6.64e- 26
