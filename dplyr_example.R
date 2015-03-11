# This is a short introduction to the `dplyr` data manipulation package


library(dplyr)
require(microbenchmark)

#' Data manipulation:
#' 
#' 'Grammar of Data'
#' Functional programming
#' 'flow of data' through a system of functions
#' 
#'  - Filtering rows on some criteria to give a subset
#'  - Adding / changing columns (possibly based on existing columns)
#'  - Reordering rows
#'  - Summarising by groups
#'  - Merging datasets
#'  - Sampling
#'  - binding datasets together
#' 
#' It allows for: 
#' 
#' -  Greater consistency
#' -  Greater speed
#' -  More efficient Workflows 











## Flitering / subsetting data
data(iris)
head(iris)
filter(iris, Species == "setosa")
filter(iris, 
       Species == "setosa", 
       Sepal.Length < 5, 
       Sepal.Width > 3, Petal.Width == 0.1)
filter(iris, Species == "setosa", 
       Sepal.Length < 5, 
       Sepal.Width > 3 | Petal.Width > 1.4)

#'  - Adding / changing columns (possibly based on existing columns)
head(mutate(iris, wide_petals = Petal.Width > mean(Petal.Width)))
# can depend on columns defined earlier in the command
head(mutate(iris, wide_petals = Petal.Width > mean(Petal.Width),
       big_petals = wide_petals & Petal.Length > mean(Petal.Length)))

#'  - Reordering rows

arrange(iris, Sepal.Length)[1:10,]
arrange(iris, Species, Sepal.Width)[1:10,]
arrange(iris, -Sepal.Length)[1:10,]

#' Summarising

#' Is not too useful by itself:
summarise(iris, total = n())

#' But you can combine with group_by to summarise by group:
a <- group_by(iris, Species)
summarise(a, total = n(), 
          petal_length = sum(Petal.Length))


#'  - Sampling

sample_n(iris, 10)
sample_n(iris, nrow(iris), replace = TRUE)[1:10,] # bootstrap sample


#' - binding

bind_rows(iris, iris)
bind_rows(lapply(1:10, function(x) iris)) # do.call(rbind, ...)
bind_cols(lapply(1:2, function(x) iris))[1:10,]






## All these functions:

#'   - Have the dataset as the first argument
#'   - Do not need to mention the dataset again
#'   - return a dataframe

#' Workflows:
#'
#' Programming with these functions can be ugly:
#' either you need to go step by step:
 


dat1 <- select(iris, -Sepal.Length, -Sepal.Width)   
dat2 <- mutate(dat1, Petal.Width = Petal.Width * 10,
               Petal.Length = Petal.Length * 10)
dat3 <- group_by(dat2, Species)
dat4 <- summarise_each(dat3, funs(mean, sd))
dat4

#' or nested:
#' 
summarise_each(
  group_by(
    mutate(
      select(iris, -Sepal.Length, -Sepal.Width), 
      Petal.Width = Petal.Width * 10,
      Petal.Length = Petal.Length * 10), 
    Species), 
  funs(mean, sd))  

## Chaining
#' x %>% f(y) turns into f(x, y)
#' 
#' 


iris %>% 
  select(-Sepal.Length, -Sepal.Width) %>%
  mutate(Petal.Width = Petal.Width * 10,
         Petal.Length = Petal.Length * 10) %>%
  group_by(Species) %>%
  summarise_each(funs(mean, sd)) -> data1
iris$Sepal.Length %>% mean 

#' You can also chain with other functions outside of dplyr:


iris %>% head
iris$Sepal.Length %>% mean



## An example with 'real' data:

#' We Want to find the prevalence of a chronic condition in two UK general practices over a 10 year period.
#' Prevalence is ~ the number of patients with the condition / the total population 
#' 
#' We have data frames for: 
#'
#'   - all cases in the practices without the condition (denoms)
#'   - all cases with the condtion and their diagnosis dates (incident_cases)
#'   - the different GP practices and the times they are collecting data from and until (practices)

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

head(d1)






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
Sys.time() - t 

head(d2)

## elements are the same:

all(d1 == d2)


## BUT not indentical objects

identical(d1, d2)

## because dplyr adds extra class metadata:

class(d1)
class(d2)


microbenchmark(base = {d3 <- merge(denoms, practices)}, 
               dplyr = {d4 <- inner_join(denoms, practices)}, 
               times = 5)

microbenchmark(base = {do.call(`rbind`, (lapply(1:50, function(x) incident_cases)))}, 
               dplyr = {bind_rows(lapply(1:50, function(x) incident_cases))}, 
               times = 5)

#' There seems to be a greater speedup in Linux than on windows (suprise!)


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
prevalence1






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
Sys.time() - t 
prevalence2

all(prevalence1 == prevalence2)


## Databases

## LOOK AT THE VIGNETTES!
#' 
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/introduction.html
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/data_frames.html
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/two-table.html
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/databases.html
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
#' - http://cran.r-project.org/web/packages/dplyr/vignettes/hybrid-evaluation.html

#' Or look at the code:
#' 
#' - https://github.com/hadley/dplyr

