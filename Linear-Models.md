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

## diagnostics - look at the residuals

modelr is tidyverse adj cut off at 500 to exclude outliers

``` r
modelr::add_residuals(nyc_airbnb, fit) %>% 
  ggplot(aes(x = stars, y = resid))+
  geom_point() +
  ylim(-500, 500)
```

<img src="Linear-Models_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

Add fitted values, synonymous to predicted values

``` r
modelr::add_predictions(nyc_airbnb, fit)
```

    ## # A tibble: 40,492 x 6
    ##    price stars boro  neighborhood room_type        pred
    ##    <dbl> <dbl> <fct> <chr>        <fct>           <dbl>
    ##  1    99   5   Bronx City Island  Private room     89.5
    ##  2   200  NA   Bronx City Island  Private room     NA  
    ##  3   300  NA   Bronx City Island  Entire home/apt  NA  
    ##  4   125   5   Bronx City Island  Entire home/apt  89.5
    ##  5    69   5   Bronx City Island  Private room     89.5
    ##  6   125   5   Bronx City Island  Entire home/apt  89.5
    ##  7    85   5   Bronx City Island  Entire home/apt  89.5
    ##  8    39   4.5 Bronx Allerton     Private room     73.5
    ##  9    95   5   Bronx Allerton     Entire home/apt  89.5
    ## 10   125   4.5 Bronx Allerton     Entire home/apt  73.5
    ## # … with 40,482 more rows

``` r
fit_null = lm(price ~ stars + boro, data = nyc_airbnb)
fit_alt = lm(price ~ stars + boro + room_type, data = nyc_airbnb)
```

## Nesting

It is possible that going from 4 to 5 stars has different effect in Manh
and Bronx Check interactions

``` r
fit_interaction = lm(price ~ stars * boro, data = nyc_airbnb)

fit_interaction %>% 
  broom::tidy()
```

    ## # A tibble: 8 x 5
    ##   term               estimate std.error statistic  p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)           -34.3     19.8     -1.73  8.34e- 2
    ## 2 stars                  43.3      4.13    10.5   1.05e-25
    ## 3 boroBrooklyn           23.0     25.9      0.887 3.75e- 1
    ## 4 boroQueens             52.4     41.8      1.25  2.10e- 1
    ## 5 boroBronx              84.2     80.2      1.05  2.93e- 1
    ## 6 stars:boroBrooklyn    -15.3      5.46    -2.81  5.02e- 3
    ## 7 stars:boroQueens      -27.5      8.90    -3.09  2.00e- 3
    ## 8 stars:boroBronx       -38.4     17.9     -2.15  3.16e- 2

``` r
# in man is stars are increased by 1, price will go up by ave $43
# in man starts effect the price a lot, in other boros not so much
```

Check interactions between stars and boro AND room type and boro

``` r
nyc_airbnb %>% 
  lm(price ~ stars * boro + room_type * boro, data = .) %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term                                |  estimate | std.error | statistic | p.value |
| :---------------------------------- | --------: | --------: | --------: | ------: |
| (Intercept)                         |    95.694 |    19.184 |     4.988 |   0.000 |
| stars                               |    27.110 |     3.965 |     6.838 |   0.000 |
| boroBrooklyn                        |  \-26.066 |    25.080 |   \-1.039 |   0.299 |
| boroQueens                          |   \-4.118 |    40.674 |   \-0.101 |   0.919 |
| boroBronx                           |   \-5.627 |    77.808 |   \-0.072 |   0.942 |
| room\_typePrivate room              | \-124.188 |     2.996 |  \-41.457 |   0.000 |
| room\_typeShared room               | \-153.635 |     8.692 |  \-17.676 |   0.000 |
| stars:boroBrooklyn                  |   \-6.139 |     5.237 |   \-1.172 |   0.241 |
| stars:boroQueens                    |  \-17.455 |     8.539 |   \-2.044 |   0.041 |
| stars:boroBronx                     |  \-22.664 |    17.099 |   \-1.325 |   0.185 |
| boroBrooklyn:room\_typePrivate room |    31.965 |     4.328 |     7.386 |   0.000 |
| boroQueens:room\_typePrivate room   |    54.933 |     7.459 |     7.365 |   0.000 |
| boroBronx:room\_typePrivate room    |    71.273 |    18.002 |     3.959 |   0.000 |
| boroBrooklyn:room\_typeShared room  |    47.797 |    13.895 |     3.440 |   0.001 |
| boroQueens:room\_typeShared room    |    58.662 |    17.897 |     3.278 |   0.001 |
| boroBronx:room\_typeShared room     |    83.089 |    42.451 |     1.957 |   0.050 |

Different model for each boro?

``` r
nyc_airbnb %>% 
  filter(boro=="Brooklyn") %>% 
  lm(price ~ stars + room_type, data = .) %>% 
  broom::tidy()
```

    ## # A tibble: 4 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               69.6     14.0       4.96 7.27e-  7
    ## 2 stars                     21.0      2.98      7.05 1.90e- 12
    ## 3 room_typePrivate room    -92.2      2.72    -34.0  6.40e-242
    ## 4 room_typeShared room    -106.       9.43    -11.2  4.15e- 29

Try to map this insted

``` r
nyc_airbnb %>% 
  # nest everything but boro
  nest(data = -boro) %>% 
  # fit lm for each boro as a new var
  mutate(
    models = map (.x = data, ~ lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(boro, results) %>% 
  unnest()
```

    ## # A tibble: 16 x 6
    ##    boro      term                  estimate std.error statistic   p.value
    ##    <fct>     <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 Bronx     (Intercept)              90.1      15.2       5.94 5.73e-  9
    ##  2 Bronx     stars                     4.45      3.35      1.33 1.85e-  1
    ##  3 Bronx     room_typePrivate room   -52.9       3.57    -14.8  6.21e- 41
    ##  4 Bronx     room_typeShared room    -70.5       8.36     -8.44 4.16e- 16
    ##  5 Queens    (Intercept)              91.6      25.8       3.54 4.00e-  4
    ##  6 Queens    stars                     9.65      5.45      1.77 7.65e-  2
    ##  7 Queens    room_typePrivate room   -69.3       4.92    -14.1  1.48e- 43
    ##  8 Queens    room_typeShared room    -95.0      11.3      -8.43 5.52e- 17
    ##  9 Brooklyn  (Intercept)              69.6      14.0       4.96 7.27e-  7
    ## 10 Brooklyn  stars                    21.0       2.98      7.05 1.90e- 12
    ## 11 Brooklyn  room_typePrivate room   -92.2       2.72    -34.0  6.40e-242
    ## 12 Brooklyn  room_typeShared room   -106.        9.43    -11.2  4.15e- 29
    ## 13 Manhattan (Intercept)              95.7      22.2       4.31 1.62e-  5
    ## 14 Manhattan stars                    27.1       4.59      5.91 3.45e-  9
    ## 15 Manhattan room_typePrivate room  -124.        3.46    -35.8  9.40e-270
    ## 16 Manhattan room_typeShared room   -154.       10.1     -15.3  2.47e- 52

Let’s nest neighborhoods

``` r
manhattan_nest_lm_results = 
  nyc_airbnb %>% 
  filter( boro =="Manhattan") %>% 
  nest(data = -neighborhood) %>% 
  mutate(
    models = map (.x = data, ~ lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(neighborhood, results) %>% 
  unnest()
```

``` r
manhattan_nest_lm_results %>% 
  filter(str_detect(term, "room_type")) %>% 
  ggplot(aes(x = neighborhood, y = estimate)) + 
  geom_point() + 
  facet_wrap(~term) + 
  theme(axis.text.x = element_text(angle = 80, hjust = 1))
```

<img src="Linear-Models_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" />

check that one point in noho - does not really work

``` r
nyc_airbnb %>% 
  filter(neighborhood == "NoHo", room_type == "Shared room")
```

    ## # A tibble: 1 x 5
    ##   price stars boro      neighborhood room_type  
    ##   <dbl> <dbl> <fct>     <chr>        <fct>      
    ## 1   219     4 Manhattan NoHo         Shared room

Binary outcomes

``` r
baltimore_df = 
  read_csv("data/homicide-data.csv") %>% 
  filter(city == "Baltimore") %>% 
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"),
    victim_age = as.numeric(victim_age),
    victim_race = fct_relevel(victim_race, "White")) %>% 
  select(resolved, victim_age, victim_race, victim_sex)
```

Using these data, we can fit a logistic regression for the binary
“resolved” outcome and victim demographics as predictors. This uses
the glm function with the family specified to account for the
non-Gaussian outcome distribution.

``` r
fit_logistic = 
  baltimore_df %>% 
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) 
```

Many of the same tools we used to work with lm fits can be used for glm
fits. The table below summaries the coefficients from the model fit;
because logistic model estimates are log odds ratios, we include a step
to compute odds ratios as well.

``` r
fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```
