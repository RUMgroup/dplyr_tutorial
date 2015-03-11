# The `dplyr` data manipulation package


    library(dplyr)
    require(microbenchmark)

Data manipulation:

'Grammar of Data' Functional programming 'flow of data' through a system
of functions

-   Filtering rows on some criteria to give a subset
-   Adding / changing columns (possibly based on existing columns)
-   Reordering rows
-   Summarising by groups
-   Merging datasets
-   Sampling
-   binding datasets together

It allows for:

-   Greater consistency
-   Greater speed
-   More efficient Workflows

<!-- -->

    ## Flitering / subsetting data
    data(iris)
    head(iris)

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

    filter(iris, Species == "setosa")

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           5.1         3.5          1.4         0.2  setosa
    ## 2           4.9         3.0          1.4         0.2  setosa
    ## 3           4.7         3.2          1.3         0.2  setosa
    ## 4           4.6         3.1          1.5         0.2  setosa
    ## 5           5.0         3.6          1.4         0.2  setosa
    ## 6           5.4         3.9          1.7         0.4  setosa
    ## 7           4.6         3.4          1.4         0.3  setosa
    ## 8           5.0         3.4          1.5         0.2  setosa
    ## 9           4.4         2.9          1.4         0.2  setosa
    ## 10          4.9         3.1          1.5         0.1  setosa
    ## 11          5.4         3.7          1.5         0.2  setosa
    ## 12          4.8         3.4          1.6         0.2  setosa
    ## 13          4.8         3.0          1.4         0.1  setosa
    ## 14          4.3         3.0          1.1         0.1  setosa
    ## 15          5.8         4.0          1.2         0.2  setosa
    ## 16          5.7         4.4          1.5         0.4  setosa
    ## 17          5.4         3.9          1.3         0.4  setosa
    ## 18          5.1         3.5          1.4         0.3  setosa
    ## 19          5.7         3.8          1.7         0.3  setosa
    ## 20          5.1         3.8          1.5         0.3  setosa
    ## 21          5.4         3.4          1.7         0.2  setosa
    ## 22          5.1         3.7          1.5         0.4  setosa
    ## 23          4.6         3.6          1.0         0.2  setosa
    ## 24          5.1         3.3          1.7         0.5  setosa
    ## 25          4.8         3.4          1.9         0.2  setosa
    ## 26          5.0         3.0          1.6         0.2  setosa
    ## 27          5.0         3.4          1.6         0.4  setosa
    ## 28          5.2         3.5          1.5         0.2  setosa
    ## 29          5.2         3.4          1.4         0.2  setosa
    ## 30          4.7         3.2          1.6         0.2  setosa
    ## 31          4.8         3.1          1.6         0.2  setosa
    ## 32          5.4         3.4          1.5         0.4  setosa
    ## 33          5.2         4.1          1.5         0.1  setosa
    ## 34          5.5         4.2          1.4         0.2  setosa
    ## 35          4.9         3.1          1.5         0.2  setosa
    ## 36          5.0         3.2          1.2         0.2  setosa
    ## 37          5.5         3.5          1.3         0.2  setosa
    ## 38          4.9         3.6          1.4         0.1  setosa
    ## 39          4.4         3.0          1.3         0.2  setosa
    ## 40          5.1         3.4          1.5         0.2  setosa
    ## 41          5.0         3.5          1.3         0.3  setosa
    ## 42          4.5         2.3          1.3         0.3  setosa
    ## 43          4.4         3.2          1.3         0.2  setosa
    ## 44          5.0         3.5          1.6         0.6  setosa
    ## 45          5.1         3.8          1.9         0.4  setosa
    ## 46          4.8         3.0          1.4         0.3  setosa
    ## 47          5.1         3.8          1.6         0.2  setosa
    ## 48          4.6         3.2          1.4         0.2  setosa
    ## 49          5.3         3.7          1.5         0.2  setosa
    ## 50          5.0         3.3          1.4         0.2  setosa

    filter(iris, 
           Species == "setosa", 
           Sepal.Length < 5, 
           Sepal.Width > 3, Petal.Width == 0.1)

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          4.9         3.1          1.5         0.1  setosa
    ## 2          4.9         3.6          1.4         0.1  setosa

    filter(iris, Species == "setosa", 
           Sepal.Length < 5, 
           Sepal.Width > 3 | Petal.Width > 1.4)

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           4.7         3.2          1.3         0.2  setosa
    ## 2           4.6         3.1          1.5         0.2  setosa
    ## 3           4.6         3.4          1.4         0.3  setosa
    ## 4           4.9         3.1          1.5         0.1  setosa
    ## 5           4.8         3.4          1.6         0.2  setosa
    ## 6           4.6         3.6          1.0         0.2  setosa
    ## 7           4.8         3.4          1.9         0.2  setosa
    ## 8           4.7         3.2          1.6         0.2  setosa
    ## 9           4.8         3.1          1.6         0.2  setosa
    ## 10          4.9         3.1          1.5         0.2  setosa
    ## 11          4.9         3.6          1.4         0.1  setosa
    ## 12          4.4         3.2          1.3         0.2  setosa
    ## 13          4.6         3.2          1.4         0.2  setosa

-   Adding / changing columns (possibly based on existing columns)

<!-- -->

    head(mutate(iris, wide_petals = Petal.Width > mean(Petal.Width)))

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species wide_petals
    ## 1          5.1         3.5          1.4         0.2  setosa       FALSE
    ## 2          4.9         3.0          1.4         0.2  setosa       FALSE
    ## 3          4.7         3.2          1.3         0.2  setosa       FALSE
    ## 4          4.6         3.1          1.5         0.2  setosa       FALSE
    ## 5          5.0         3.6          1.4         0.2  setosa       FALSE
    ## 6          5.4         3.9          1.7         0.4  setosa       FALSE

    # can depend on columns defined earlier in the command
    head(mutate(iris, wide_petals = Petal.Width > mean(Petal.Width),
           big_petals = wide_petals & Petal.Length > mean(Petal.Length)))

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species wide_petals
    ## 1          5.1         3.5          1.4         0.2  setosa       FALSE
    ## 2          4.9         3.0          1.4         0.2  setosa       FALSE
    ## 3          4.7         3.2          1.3         0.2  setosa       FALSE
    ## 4          4.6         3.1          1.5         0.2  setosa       FALSE
    ## 5          5.0         3.6          1.4         0.2  setosa       FALSE
    ## 6          5.4         3.9          1.7         0.4  setosa       FALSE
    ##   big_petals
    ## 1      FALSE
    ## 2      FALSE
    ## 3      FALSE
    ## 4      FALSE
    ## 5      FALSE
    ## 6      FALSE

-   Reordering rows

<!-- -->

    arrange(iris, Sepal.Length)[1:10,]

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           4.3         3.0          1.1         0.1  setosa
    ## 2           4.4         2.9          1.4         0.2  setosa
    ## 3           4.4         3.0          1.3         0.2  setosa
    ## 4           4.4         3.2          1.3         0.2  setosa
    ## 5           4.5         2.3          1.3         0.3  setosa
    ## 6           4.6         3.1          1.5         0.2  setosa
    ## 7           4.6         3.4          1.4         0.3  setosa
    ## 8           4.6         3.6          1.0         0.2  setosa
    ## 9           4.6         3.2          1.4         0.2  setosa
    ## 10          4.7         3.2          1.3         0.2  setosa

    arrange(iris, Species, Sepal.Width)[1:10,]

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           4.5         2.3          1.3         0.3  setosa
    ## 2           4.4         2.9          1.4         0.2  setosa
    ## 3           4.9         3.0          1.4         0.2  setosa
    ## 4           4.8         3.0          1.4         0.1  setosa
    ## 5           4.3         3.0          1.1         0.1  setosa
    ## 6           5.0         3.0          1.6         0.2  setosa
    ## 7           4.4         3.0          1.3         0.2  setosa
    ## 8           4.8         3.0          1.4         0.3  setosa
    ## 9           4.6         3.1          1.5         0.2  setosa
    ## 10          4.9         3.1          1.5         0.1  setosa

    arrange(iris, -Sepal.Length)[1:10,]

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
    ## 1           7.9         3.8          6.4         2.0 virginica
    ## 2           7.7         3.8          6.7         2.2 virginica
    ## 3           7.7         2.6          6.9         2.3 virginica
    ## 4           7.7         2.8          6.7         2.0 virginica
    ## 5           7.7         3.0          6.1         2.3 virginica
    ## 6           7.6         3.0          6.6         2.1 virginica
    ## 7           7.4         2.8          6.1         1.9 virginica
    ## 8           7.3         2.9          6.3         1.8 virginica
    ## 9           7.2         3.6          6.1         2.5 virginica
    ## 10          7.2         3.2          6.0         1.8 virginica

-   Summarising Not too useful by itself:

<!-- -->

    summarise(iris, total = n())

    ##   total
    ## 1   150

combine with group\_by to summarise by group:

    a <- group_by(iris, Species)
    summarise(a, total = n(), 
              petal_length = sum(Petal.Length))

    ## Source: local data frame [3 x 3]
    ## 
    ##      Species total petal_length
    ## 1     setosa    50         73.1
    ## 2 versicolor    50        213.0
    ## 3  virginica    50        277.6

-   Sampling

<!-- -->

    sample_n(iris, 10)

    ##     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
    ## 85           5.4         3.0          4.5         1.5 versicolor
    ## 129          6.4         2.8          5.6         2.1  virginica
    ## 82           5.5         2.4          3.7         1.0 versicolor
    ## 19           5.7         3.8          1.7         0.3     setosa
    ## 65           5.6         2.9          3.6         1.3 versicolor
    ## 78           6.7         3.0          5.0         1.7 versicolor
    ## 68           5.8         2.7          4.1         1.0 versicolor
    ## 102          5.8         2.7          5.1         1.9  virginica
    ## 125          6.7         3.3          5.7         2.1  virginica
    ## 126          7.2         3.2          6.0         1.8  virginica

    sample_n(iris, nrow(iris), replace = TRUE)[1:10,] # bootstrap sample

    ##       Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
    ## 45             5.1         3.8          1.9         0.4     setosa
    ## 145            6.7         3.3          5.7         2.5  virginica
    ## 144            6.8         3.2          5.9         2.3  virginica
    ## 145.1          6.7         3.3          5.7         2.5  virginica
    ## 136            7.7         3.0          6.1         2.3  virginica
    ## 23             4.6         3.6          1.0         0.2     setosa
    ## 135            6.1         2.6          5.6         1.4  virginica
    ## 137            6.3         3.4          5.6         2.4  virginica
    ## 24             5.1         3.3          1.7         0.5     setosa
    ## 84             6.0         2.7          5.1         1.6 versicolor

-   binding

<!-- -->

    bind_rows(iris, iris)

    ## Source: local data frame [300 x 5]
    ## 
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           5.1         3.5          1.4         0.2  setosa
    ## 2           4.9         3.0          1.4         0.2  setosa
    ## 3           4.7         3.2          1.3         0.2  setosa
    ## 4           4.6         3.1          1.5         0.2  setosa
    ## 5           5.0         3.6          1.4         0.2  setosa
    ## 6           5.4         3.9          1.7         0.4  setosa
    ## 7           4.6         3.4          1.4         0.3  setosa
    ## 8           5.0         3.4          1.5         0.2  setosa
    ## 9           4.4         2.9          1.4         0.2  setosa
    ## 10          4.9         3.1          1.5         0.1  setosa
    ## ..          ...         ...          ...         ...     ...

    bind_rows(lapply(1:10, function(x) iris)) # do.call(rbind, ...)

    ## Source: local data frame [1,500 x 5]
    ## 
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           5.1         3.5          1.4         0.2  setosa
    ## 2           4.9         3.0          1.4         0.2  setosa
    ## 3           4.7         3.2          1.3         0.2  setosa
    ## 4           4.6         3.1          1.5         0.2  setosa
    ## 5           5.0         3.6          1.4         0.2  setosa
    ## 6           5.4         3.9          1.7         0.4  setosa
    ## 7           4.6         3.4          1.4         0.3  setosa
    ## 8           5.0         3.4          1.5         0.2  setosa
    ## 9           4.4         2.9          1.4         0.2  setosa
    ## 10          4.9         3.1          1.5         0.1  setosa
    ## ..          ...         ...          ...         ...     ...

    bind_cols(lapply(1:2, function(x) iris))[1:10,]

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal.Length
    ## 1           5.1         3.5          1.4         0.2  setosa          5.1
    ## 2           4.9         3.0          1.4         0.2  setosa          4.9
    ## 3           4.7         3.2          1.3         0.2  setosa          4.7
    ## 4           4.6         3.1          1.5         0.2  setosa          4.6
    ## 5           5.0         3.6          1.4         0.2  setosa          5.0
    ## 6           5.4         3.9          1.7         0.4  setosa          5.4
    ## 7           4.6         3.4          1.4         0.3  setosa          4.6
    ## 8           5.0         3.4          1.5         0.2  setosa          5.0
    ## 9           4.4         2.9          1.4         0.2  setosa          4.4
    ## 10          4.9         3.1          1.5         0.1  setosa          4.9
    ##    Sepal.Width Petal.Length Petal.Width Species
    ## 1          3.5          1.4         0.2  setosa
    ## 2          3.0          1.4         0.2  setosa
    ## 3          3.2          1.3         0.2  setosa
    ## 4          3.1          1.5         0.2  setosa
    ## 5          3.6          1.4         0.2  setosa
    ## 6          3.9          1.7         0.4  setosa
    ## 7          3.4          1.4         0.3  setosa
    ## 8          3.4          1.5         0.2  setosa
    ## 9          2.9          1.4         0.2  setosa
    ## 10         3.1          1.5         0.1  setosa

All these functions - Have the dataset as the first argument - Do not
need to mention the dataset again - return a dataframe Workflows:

Can be ugly: either step by step:

    dat1 <- select(iris, -Sepal.Length, -Sepal.Width)   
    dat2 <- mutate(dat1, Petal.Width = Petal.Width * 10,
                   Petal.Length = Petal.Length * 10)
    dat3 <- group_by(dat2, Species)
    dat4 <- summarise_each(dat3, funs(mean, sd))
    dat4

    ## Source: local data frame [3 x 5]
    ## 
    ##      Species Petal.Length_mean Petal.Width_mean Petal.Length_sd
    ## 1     setosa             14.62             2.46        1.736640
    ## 2 versicolor             42.60            13.26        4.699110
    ## 3  virginica             55.52            20.26        5.518947
    ## Variables not shown: Petal.Width_sd (dbl)

or nested:

    summarise_each(
      group_by(
        mutate(
          select(iris, -Sepal.Length, -Sepal.Width), 
          Petal.Width = Petal.Width * 10,
          Petal.Length = Petal.Length * 10), 
        Species), 
      funs(mean, sd))  

    ## Source: local data frame [3 x 5]
    ## 
    ##      Species Petal.Length_mean Petal.Width_mean Petal.Length_sd
    ## 1     setosa             14.62             2.46        1.736640
    ## 2 versicolor             42.60            13.26        4.699110
    ## 3  virginica             55.52            20.26        5.518947
    ## Variables not shown: Petal.Width_sd (dbl)

    ## Chaining

x %\>% f(y) turns into f(x, y)

    iris %>% 
      select(-Sepal.Length, -Sepal.Width) %>%
      mutate(Petal.Width = Petal.Width * 10,
             Petal.Length = Petal.Length * 10) %>%
      group_by(Species) %>%
      summarise_each(funs(mean, sd)) -> data1
    iris$Sepal.Length %>% mean 

    ## [1] 5.843333

You can also chain with other functions outside of dplyr:

    iris %>% head

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

    iris$Sepal.Length %>% mean

    ## [1] 5.843333

    ## An example with 'real' data:

We Want to find the prevalence of a chronic condition in two UK general
practices over a 10 year period. Prevalence is ~ the number of patients
with the condition / the total population

We have data frames for: - all cases in the practices without the
condition (denoms) - all cases with the condtion and their diagnosis
dates (incident\_cases) - the different GP practices and the times they
are collecting data from and until (practices)

    rm(list=ls())
    load("example2.rda")

    ## cleaning/merging in base R:

    t <- Sys.time()
    d1 <- denoms[denoms$sex %in% c(1,2),]  
    d1 <- merge(d1, practices) 
    d1 <- merge(d1, incident_cases, all.x = TRUE)
    d1$diagnosis_date <- as.Date(d1$diagnosis_date)
    d1$outdate <- as.Date(d1$outdate)
    d1$num <- !is.na(d1$diagnosis_date)
    d1$denom <- !d1$num
    d1$age <- d1$year - d1$birthyear
    d1 <- d1[, c("patient_id", "gp_practice", "sex", 
                 "year", "num", "denom", "age")]
    d1 <- d1[order(d1$gp_practice, d1$patient_id),]
    Sys.time() - t 

    ## Time difference of 4.463446 secs

    head(d1)

    ##   patient_id gp_practice sex year   num denom age
    ## 1       1231           1   2 2004 FALSE  TRUE  77
    ## 2       1231           1   2 2005 FALSE  TRUE  71
    ## 3       1231           1   2 2006 FALSE  TRUE  74
    ## 4       1231           1   2 2007 FALSE  TRUE  82
    ## 5       1231           1   2 2008 FALSE  TRUE  76
    ## 6       1231           1   2 2009 FALSE  TRUE  82

    ## cleaning/merging with dplyr:

    t <- Sys.time()
    denoms %>% 
      filter(sex %in% c(1,2)) %>%
      inner_join(practices) %>%
      left_join(incident_cases)  %>%
      mutate(diagnosis_date = as.Date(diagnosis_date),
             outdate = as.Date(outdate),
             num = !is.na(diagnosis_date),
             denom = !num,
             age = year - birthyear) %>%
      select(patient_id, gp_practice, sex,
             year, num, denom, age) %>% 
      arrange(gp_practice, patient_id) -> d2 

    ## Joining by: "gp_practice"
    ## Joining by: c("patient_id", "year")

    Sys.time() - t 

    ## Time difference of 1.037104 secs

    head(d2)

    ## Source: local data frame [6 x 7]
    ## 
    ##   patient_id gp_practice sex year   num denom age
    ## 1       1231           1   2 2004 FALSE  TRUE  77
    ## 2       1231           1   2 2005 FALSE  TRUE  71
    ## 3       1231           1   2 2006 FALSE  TRUE  74
    ## 4       1231           1   2 2007 FALSE  TRUE  82
    ## 5       1231           1   2 2008 FALSE  TRUE  76
    ## 6       1231           1   2 2009 FALSE  TRUE  82

    ## elements are the same:

    all(d1 == d2)

    ## [1] TRUE

    ## BUT not indentical objects

    identical(d1, d2)

    ## [1] FALSE

    ## because dplyr adds extra class metadata:

    class(d1)

    ## [1] "data.frame"

    class(d2)

    ## [1] "tbl_df"     "tbl"        "data.frame"

    microbenchmark(base = {d3 <- merge(denoms, practices)}, 
                   dplyr = {d4 <- inner_join(denoms, practices)}, 
                   times = 5)

    ## Joining by: "gp_practice"
    ## Joining by: "gp_practice"
    ## Joining by: "gp_practice"
    ## Joining by: "gp_practice"
    ## Joining by: "gp_practice"

    ## Unit: milliseconds
    ##   expr      min        lq      mean    median        uq      max neval
    ##   base 746.9898 768.24422 806.55250 797.06001 806.49201 913.9764     5
    ##  dplyr  26.5691  27.96849  69.54838  73.65334  76.84453 142.7064     5

    microbenchmark(base = {do.call(`rbind`, (lapply(1:50, function(x) incident_cases)))}, 
                   dplyr = {bind_rows(lapply(1:50, function(x) incident_cases))}, 
                   times = 5)

    ## Unit: milliseconds
    ##   expr       min        lq      mean    median        uq      max neval
    ##   base 344.45712 351.24549 381.61456 394.24385 405.21107 412.9153     5
    ##  dplyr  16.28437  16.58652  17.52009  16.59799  16.69338  21.4382     5

There seems to be a greater speedup in Linux than on windows (suprise!)

    ## Summarise numerators and denominators by year using base R

    t <- Sys.time()
    denominator <- aggregate(d1[d1$denom == TRUE,]$patient_id, 
              by = list(year = d1[d1$denom == TRUE,]$year), 
              FUN = length)
    names(denominator)[2] <- "denominator"
    numerator <- aggregate(d1[d1$num == TRUE,]$patient_id, 
                           by = list(year = d1[d1$num == TRUE,]$year), 
                           FUN = length)
    names(numerator)[2] <- "numerator"
    prevalence1 <- merge(denominator, numerator)
    prevalence1$prevalence <- with(prevalence1, 100 * (numerator / denominator))
    Sys.time() - t 

    ## Time difference of 4.0014 secs

    prevalence1

    ##    year denominator numerator prevalence
    ## 1  2004       33521       674   2.010680
    ## 2  2005       35209       763   2.167060
    ## 3  2006       36748       847   2.304887
    ## 4  2007       38181       916   2.399099
    ## 5  2008       39854       994   2.494103
    ## 6  2009       40984      1048   2.557095
    ## 7  2010       41912      1101   2.626933
    ## 8  2011       42855      1159   2.704469
    ## 9  2012       43969      1215   2.763311
    ## 10 2013       45013      1244   2.763646

    ## Summarise numerators and denominators by year with dplyr
    t <- Sys.time()
    inner_join(d2 %>% 
                 filter(denom == TRUE) %>% 
                 group_by(year) %>% 
                 summarise(denominator = n()), 
               d2 %>% 
                 filter(num == TRUE) %>% 
                 group_by(year) %>% 
                 summarise(numerator = n())) %>%
      mutate(prevalence = 100 * (numerator / denominator)) -> prevalence2

    ## Joining by: "year"

    Sys.time() - t 

    ## Time difference of 0.03600383 secs

    prevalence2

    ## Source: local data frame [10 x 4]
    ## 
    ##    year denominator numerator prevalence
    ## 1  2004       33521       674   2.010680
    ## 2  2005       35209       763   2.167060
    ## 3  2006       36748       847   2.304887
    ## 4  2007       38181       916   2.399099
    ## 5  2008       39854       994   2.494103
    ## 6  2009       40984      1048   2.557095
    ## 7  2010       41912      1101   2.626933
    ## 8  2011       42855      1159   2.704469
    ## 9  2012       43969      1215   2.763311
    ## 10 2013       45013      1244   2.763646

    all(prevalence1 == prevalence2)

    ## [1] TRUE

    ## Databases

    ## LOOK AT THE VIGNETTES!

-   <http://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html>
-   <http://cran.r-project.org/web/packages/dplyr/vignettes/data_frames.html>
-   <http://cran.r-project.org/web/packages/dplyr/vignettes/two-table.html>
-   <http://cran.r-project.org/web/packages/dplyr/vignettes/databases.html>
-   <http://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html>
-   <http://cran.r-project.org/web/packages/dplyr/vignettes/nse.html>
-   <http://cran.r-project.org/web/packages/dplyr/vignettes/hybrid-evaluation.html>
    Or look at the code:

-   <https://github.com/hadley/dplyr>
