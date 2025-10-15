
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(readr)

deaths <- av |>
  pivot_longer(
    cols = starts_with("Death"),
    names_to = "Time_Death",
    values_to = "Death"
  ) |>
  mutate(
    Time_Death = parse_number(Time_Death),
    Death = factor(Death, levels = c("YES", "NO", ""))
  )

returns <- av |>
  pivot_longer(
    cols = starts_with("Return"),
    names_to = "Time_Return",
    values_to = "Return"
  ) |>
  mutate(
    Time_Return = parse_number(Time_Return),
    Return = factor(Return, levels = c("YES", "NO", ""))
  )
```

``` r
deaths  <- deaths  %>% mutate(Death  = toupper(trimws(as.character(Death))))
returns <- returns %>% mutate(Return = toupper(trimws(as.character(Return))))

avg_deaths_tbl <- deaths %>%
  mutate(is_death = Death == "YES") %>%
  group_by(Name.Alias) %>%
  summarize(death_count = sum(is_death), .groups = "drop")

mean_deaths   <- mean(avg_deaths_tbl$death_count)
median_deaths <- median(avg_deaths_tbl$death_count)
max_deaths    <- max(avg_deaths_tbl$death_count)

cat("Average deaths per Avenger:", round(mean_deaths, 3), "\n")
```

    ## Average deaths per Avenger: 0.546

``` r
cat("Median deaths:", median_deaths, "\n")
```

    ## Median deaths: 0

``` r
cat("Max deaths for any single Avenger:", max_deaths, "\n")
```

    ## Max deaths for any single Avenger: 7

As a team, we found the average deaths per Avenger to be 0.546, the
median to be 0, and the max was 7.

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

Nayan Menezes Statement: Out of 173 listed Avengers, my analysis found
that 69 had died at least one time after they joined the team.

Phillip Giametta Statement: I counted 89 total deaths — some unlucky
Avengers are basically Meat Loaf with an E-ZPass — and on 57 occasions
the individual made a comeback.

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

Nayan’s Code:

``` r
num_died <- deaths %>%
  filter(Death == "YES") %>%
  distinct(Name.Alias) %>%
  nrow()

cat("Number of Avengers who died at least once:", num_died, "\n")
```

    ## Number of Avengers who died at least once: 64

``` r
total_avengers <- deaths %>%
  distinct(Name.Alias) %>%
  nrow()

cat("Total number of Avengers:", total_avengers, "\n")
```

    ## Total number of Avengers: 163

``` r
percent_died <- num_died / total_avengers * 100
cat("Percentage of Avengers who died at least once:", round(percent_died, 1), "%\n")
```

    ## Percentage of Avengers who died at least once: 39.3 %

Phillip’s Code:

``` r
total_deaths <- deaths %>% filter(Death == "YES") %>% nrow()
total_returns <- returns %>% filter(Return == "YES") %>% nrow()

cat("Total recorded deaths:", total_deaths, "\n")
```

    ## Total recorded deaths: 89

``` r
cat("Total recorded returns:", total_returns, "\n")
```

    ## Total recorded returns: 57

### Include your answer

Nayan’s Fact Check: It does seem that the number of avengers whom died
at least once hovers right around 40%, with an actual percentage of
39.3%. However, the number of distinct avengers that was found based on
finding distinct Name.Alias was actually 163, so it’s possible there was
some duplicate rows.

Phillip’s Fact Check: Overall my data supports the article. Looking at
my results, there was 89 total deaths and 57 returns, so nearly 64.0% of
deaths lead to a comeback. Paired with 64 of 163 or 39.3% Avengers dying
at least once and an average of 0.546 deaths per Avenger (1.39 among
only those who die). In the end we can see, this support’s the articles
point that Avengers die fairly often but as he said, they don’t normally
stay dead.

Include at least one sentence discussing the result of your
fact-checking endeavor.

Upload your changes to the repository. Discuss and refine answers as a
team.
