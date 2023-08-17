BSA
================
Tom Saunders

``` r
library(readxl)
library(lubridate)
library(readr)
library(dplyr)
library(stringr)
```

Read in two datasets: the first is for complaints made under the
previous BSA Codebook (1 April 2016 to 30 June 2022); the second is for
complaints made under the current Codebook (1 July 2022 to present).

``` r
bsa_prev <- read_excel("data/20230810 AllClosedFormalComplaints.xlsx")
bsa_curr <- read_excel("data/20230807 AllClosedFormalComplaints.xlsx")
```

Convert `Broadcast Date/Time` to date in each dataset. I can’t find a
way to preserve times.

``` r
bsa_prev$`Broadcast Date/Time` <- as.numeric(bsa_prev$`Broadcast Date/Time`)
```

    Warning: NAs introduced by coercion

``` r
bsa_prev$`Broadcast Date/Time` <- as.Date(bsa_prev$`Broadcast Date/Time`, origin = "1899-12-30")

bsa_curr$`Broadcast Date/Time` <- as.Date(bsa_curr$`Broadcast Date/Time`, origin = "1899-12-30")
```

Join the two datasets.

``` r
bsa_full <- full_join(bsa_prev, bsa_curr)
```

Rename columns.

``` r
colnames(bsa_full) <- c("broadcaster", "complaint", "programme", "broadcast_date", "genre",
                        "code", "determination", "decision_date", "majority", "split", 
                        "elect_standards", "elect_notupheld", "elect_upheld", 
                        "fta_standards", "fta_notupheld", "fta_upheld", "paytv_standards",
                        "paytv_notupheld", "paytv_upheld", "radio_standards", "radio_notupheld",
                        "radio_upheld", "standards", "not_upheld", "upheld", "tv_radio")
```

Consolidate individual standards based on previous codebook into
‘standards’ column introduced for new codebook. Repeat for ‘upheld’ and
‘not upheld’ standards. Drop these columns once they are consolidated.

``` r
bsa_full$standards <- coalesce(bsa_full$elect_standards, bsa_full$fta_standards, bsa_full$paytv_standards, bsa_full$radio_standards, bsa_full$standards)
  
bsa_full$not_upheld <- coalesce(bsa_full$elect_notupheld, bsa_full$fta_notupheld, bsa_full$paytv_notupheld, bsa_full$radio_notupheld, bsa_full$not_upheld)

bsa_full$upheld <- coalesce(bsa_full$elect_upheld, bsa_full$fta_upheld, bsa_full$paytv_upheld, bsa_full$radio_upheld, bsa_full$upheld)

bsa_full <- select(bsa_full, -c(11:22))
```

Fix factors.WIP.

``` r
# Recode 'broadcaster' "Newstalk ZB" as the actual Broadcaster
bsa_full$broadcaster <- recode(bsa_full$broadcaster, "Newstalk ZB" = "NZME Radio Ltd")

# Recode 'complaint' numbers as what they have been renumbered to
bsa_full$complaint <- recode(bsa_full$complaint, 
                             "2016-053 (renumbered 048B)" = "2016-048B",
                             "2017-027 (renumbered 018B)" = "2017-018B",
                             "2017-028 (renumbered 018C)" = "2017-018C",
                             "2017-029 (renumbered 018D)" = "2017-018D")

# programme: Need to figure out how to use regex

# Recode 'code' as 'either 'Television' or 'Radio', move to 'tv_radio' column to align with new codebook, then delete 'code' column.

bsa_full$code <- recode(bsa_full$code, 
                             "Free to Air TV" = "Television",
                             "Pay TV" = "Television",
                             "Election Program" = "Television")

bsa_full$tv_radio <- coalesce(bsa_full$code, bsa_full$tv_radio)

bsa_full <- select(bsa_full, -code)

# Remove asterisk and whitespace from columns containing them.
bsa_full <- bsa_full |> 
  mutate(
    standards = gsub("\\*", "", standards),
    not_upheld = gsub("\\*", "", not_upheld),
    upheld = gsub("\\*", "", upheld),
  )

bsa_full$standards <- str_trim(bsa_full$standards, "left")
bsa_full$upheld <- str_trim(bsa_full$upheld, "left")
bsa_full$not_upheld <- str_trim(bsa_full$not_upheld, "left")

# Fix missing values in standards/upheld/not_upheld columns

## Find rows where standards are missing from both 'upheld' and 'not_upheld" columns.

missing <- bsa_full |> 
  filter(is.na(upheld) & is.na(not_upheld))

## Add missing values to standards column based on manual BSA decision database lookup

missing[1,10] <- "Discrimination and Denigration"
missing[4, 10] <- "Accuracy" # Complaint 2018-020 
missing[5,10] <- "Balance,Accuracy" # Complaint 2019-006
missing[5,4] <- as.Date("2018-12-17")

## Use case_when to fill in missing values in not_upheld and upheld columns.

missing <- missing |>
  mutate(
    not_upheld = case_when(determination %in% c("Not Upheld", "Declined to Determine 11a", "Declined to Determine 11b") ~ standards,
                           complaint == "2017-101C" ~ "Children's Interests",
                           complaint == "2017-101D" ~ "Children's Interests",
                           complaint == "2019-019B" ~ "Privacy"),
    upheld = case_when(complaint == "2019-019B" ~ "Fairness",
                       complaint == "2017-101C" ~ "Good Taste and Decency",
                       complaint == "2017-101D" ~ "Good Taste and Decency")
  )
  
## Merge missing dataframe back into bsa_full

bsa_full <- bsa_full |>
  filter(!is.na(upheld) | !is.na(not_upheld)) |> 
  bind_rows(missing)
```