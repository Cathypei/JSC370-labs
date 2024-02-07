Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")

met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
# merge the data using left join
data <- left_join(met, stations, by = c("USAFID" = "USAF"))
# remove duplicate column for lat and lon
data <- subset(data, select = -c(LAT, LON))
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
# find median value for each station
median_by_station <- data %>%
  group_by(USAFID) %>%
  summarise(
    temp = median(temp, na.rm = TRUE),
    wind.sp = median(wind.sp,na.rm = TRUE),
    atm.press = median(atm.press,na.rm = TRUE)
  )
median_by_station
```

    ## # A tibble: 1,852 × 4
    ##    USAFID  temp wind.sp atm.press
    ##     <int> <dbl>   <dbl>     <dbl>
    ##  1 690150  26.7     4.1     1009.
    ##  2 720110  28       3.1       NA 
    ##  3 720113  20       3.1       NA 
    ##  4 720120  24       3.6       NA 
    ##  5 720137  21.1     2.6       NA 
    ##  6 720151  28.0     3.6       NA 
    ##  7 720169  23       3.6       NA 
    ##  8 720170  23.5     2.6       NA 
    ##  9 720172  23.4     2.1       NA 
    ## 10 720175  25       2.6     1011.
    ## # … with 1,842 more rows

``` r
# identify stations with median values using quantile()
median_temp <- median(median_by_station$temp, na.rm = TRUE)

stations_with_median_temp <- median_by_station %>%
  group_by(USAFID) %>%
  filter(quantile(temp, 0.5, na.rm = TRUE) == median_temp) %>%
  distinct(USAFID)

median_wind_sp <- median(median_by_station$wind.sp, na.rm = TRUE)

stations_with_median_wind_sp <- median_by_station %>%
  group_by(USAFID) %>%
  filter(quantile(wind.sp, 0.5, na.rm = TRUE) == median_wind_sp) %>%
  distinct(USAFID)

median_atm_press <- median(median_by_station$atm.press, na.rm = TRUE)

stations_with_median_atm_press <- median_by_station %>%
  group_by(USAFID) %>%
  filter(quantile(atm.press, 0.5, na.rm = TRUE) == median_atm_press) %>%
  distinct(USAFID)
```

Next identify the stations have these median values.

``` r
# print the station IDs
print(stations_with_median_temp)
```

    ## # A tibble: 51 × 1
    ## # Groups:   USAFID [51]
    ##    USAFID
    ##     <int>
    ##  1 720137
    ##  2 720285
    ##  3 720413
    ##  4 720674
    ##  5 722172
    ##  6 722185
    ##  7 723084
    ##  8 723117
    ##  9 723118
    ## 10 723120
    ## # … with 41 more rows

``` r
print(stations_with_median_wind_sp)
```

    ## # A tibble: 577 × 1
    ## # Groups:   USAFID [577]
    ##    USAFID
    ##     <int>
    ##  1 720110
    ##  2 720113
    ##  3 720258
    ##  4 720261
    ##  5 720266
    ##  6 720267
    ##  7 720268
    ##  8 720272
    ##  9 720283
    ## 10 720284
    ## # … with 567 more rows

``` r
print(stations_with_median_atm_press)
```

    ## # A tibble: 41 × 1
    ## # Groups:   USAFID [41]
    ##    USAFID
    ##     <int>
    ##  1 722069
    ##  2 722070
    ##  3 722185
    ##  4 722197
    ##  5 722223
    ##  6 722225
    ##  7 722230
    ##  8 722280
    ##  9 722310
    ## 10 722499
    ## # … with 31 more rows

``` r
# find coincide station
coincide_stations <- Reduce(intersect, list(stations_with_median_temp$USAFID, stations_with_median_wind_sp$USAFID, stations_with_median_atm_press$USAFID))

# print the coincide station IDs
print(coincide_stations)
```

    ## [1] 722185

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
median_by_station_with_state <- left_join(median_by_station, stations, by = c("USAFID" = "USAF"))

# calculate Euclidean distance
median_by_station_with_state$distance <- sqrt((median_by_station_with_state$temp - median_temp)^2 + 
                              (median_by_station_with_state$wind.sp - median_wind_sp)^2)

# find the station with the smallest distance for each state
closest_stations <- median_by_station_with_state %>%
  group_by(STATE) %>%
  slice(which.min(distance)) %>%
  select(STATE, USAFID, temp, wind.sp, distance)

# print the result
print(closest_stations)
```

    ## # A tibble: 48 × 5
    ## # Groups:   STATE [48]
    ##    STATE USAFID  temp wind.sp distance
    ##    <chr>  <int> <dbl>   <dbl>    <dbl>
    ##  1 AL    720413  21.1     2.6    0.5  
    ##  2 AR    723445  22.2     2.6    1.21 
    ##  3 AZ    723723  20.6     3.6    0.707
    ##  4 CA    724800  20.6     3.1    0.5  
    ##  5 CO    720531  20.8     3.1    0.300
    ##  6 CT    725045  20.6     3.1    0.5  
    ##  7 DE    724093  21.1     3.6    0.5  
    ##  8 FL    722067  23       3.1    1.90 
    ##  9 GA    722185  21.1     3.1    0    
    ## 10 IA    725465  21.1     3.1    0    
    ## # … with 38 more rows

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
\~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
