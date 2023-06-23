# Purpose

Proposed layout: EDA (univariate, bivariate analysis), model (compare
hard to interpret results against bivariate)

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.2     ✔ purrr   1.0.1
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.2
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.4     ✔ forcats 1.0.0

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'forcats' was built under R version 4.2.3

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ranger)
```

    ## Warning: package 'ranger' was built under R version 4.2.3

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.3

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(tidyr)
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.2.3

    ## corrplot 0.92 loaded

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 4.2.3

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(broom)
```

    ## Warning: package 'broom' was built under R version 4.2.3

``` r
library(huxtable)
```

    ## 
    ## Attaching package: 'huxtable'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     add_rownames
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     theme_grey

``` r
source("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\code\\column_mutations.R")
source("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\code\\data_loading.R")
source("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\code\\ggplots.R")
source("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\code\\title_finders.R")

# loading and merging data

purchases <- read_csv("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\data\\Full_Rec_Mooc_Purchases.csv") %>% select(mooc_id, buyer_id)
```

    ## New names:
    ## • `` -> `...1`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 224632 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): purchase_id, mooc_id, buyer_id, buyer_geography, goods, price, sla...
    ## dbl  (3): ...1, price_rijx, price_stuv
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
auctions <- read_csv("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\data\\Full_Rec_Mooc.csv") %>% 
    select(mooc_id, date, auction_tot) %>% 
    mutate(auction_tot= as.numeric(auction_tot)) 
```

    ## New names:
    ## Rows: 1962 Columns: 15
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (12): mooc_id, deceased_id, deceased_name, date, date_str, page, officia... dbl
    ## (3): ...1, action_rijx, auction_stuiv
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `auction_tot = as.numeric(auction_tot)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
final_df <- data_loader("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\data")
merged_df <- merger(final_df, auctions) %>% 
    filter(unit_price>0)
```

    ## Warning in left_join(data1, data2, by = c("mooc_id", "date")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 14 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# summing the number of items within a good type per auction 
goods <- c("books", "art_products", "balance", "beds", "cattle", "chairs", "china", "clocks", 
           "coatrack", "cups", "guns", "hammers", "horse_cart", "horses", "irons", "jewlery", 
           "lamps", "oven", "plates", "pot", "plough", "sheep", "slaves", "tables", 
                                                     "utensils", "wagons")
goods_id_str <- paste0("total_", goods)

total_number_good_auction <-  good_counter(goods = c("books", "art_products", "balance", 
                                                     "beds", "cattle", "chairs", "china", 
                                                     "clocks", "coatrack", "cups", "guns", 
                                                     "hammers", "horse_cart", "horses", "irons", 
                                                     "jewlery", "lamps", "oven", "plates", 
                                                     "pot", "plough", "sheep", "slaves", "tables", 
                                                     "utensils", "wagons")) %>% 
    distinct(mooc_id, .keep_all=TRUE) %>% 
    ungroup() %>% 
    dplyr::select(mooc_id, total_goods, goods_id) %>% 
    arrange(mooc_id) %>% 
    pivot_wider(names_from = goods_id, values_from = total_goods) %>% 
    as.data.frame() %>% 
    mutate(across(all_of(goods_id_str), ~ ifelse(is.na(.), 0, .)))

household_items <- c("beds", "chairs", "china", "clocks", "coatrack", "cups", "irons", "lamps", "tables", "balance")
kitchen_items <- c("oven", "plates", "pot", "utensils")
farming_items <- c("cattle", "sheep", "plough", "wagons", "horses", "horse_cart", "guns", "hammers")
select_items <- c("books", "jewelry", "slaves", "art_products")



# determining auction size

auction_size <- merged_df %>% 
        group_by(mooc_id) %>% 
    mutate(auction_size=sum(count)) %>% 
    distinct(mooc_id, .keep_all=TRUE) %>% 
    dplyr::select(mooc_id, date, auction_tot, auction_size) 

pen_ultimate_df <- left_join(total_number_good_auction, auction_size, "mooc_id")


# counting attendance by titled men and women per auction
women <- titledwomen_locator(titled_women, purchases$buyer_id) %>% 
    as.numeric()
men <- titledmen_locator(titled_men, purchases$buyer_id) %>% 
    as.numeric()
purchases_df <- tibble(purchases, women, men)
purchases_df <- title_counter(purchases_df) 
   

# merging titles and auctions
model_df <- inner_join(pen_ultimate_df,purchases_df, by="mooc_id")
model_df <- model_df %>% 
    mutate(year = substr(date, 1, 4)) %>% 
    arrange(year) %>% 
    group_by(year) %>% 
      mutate(date=n())

# assigning value from 1 to N for each year
model_df$date <- as.numeric(match(model_df$year, unique(model_df$year)))

# filtering table based on recording errors in year transcription
model_df <- model_df %>% 
    filter(date<103) %>% 
    filter(date!=1)  %>% 
    filter(auction_tot>0) %>% 
    filter(auction_tot!=-Inf)

# table for machine learning models
ml_df <- model_df %>% 
     ungroup() %>% 
    dplyr::select(-year, -mooc_id) %>% 
    na.omit() %>% 
  mutate(across(everything(), ~ifelse(. != 0, log(.), .)))
```

# example

``` r
# auction example table
auction_example <- read_csv("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\data\\Full_Rec_Mooc_Purchases.csv") %>% arrange(mooc_id) %>% 
    select(mooc_id, purchase_id, price, buyer_id, goods) %>% 
    filter(mooc_id=="MOOC10/10.1")
```

    ## New names:
    ## • `` -> `...1`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 224632 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): purchase_id, mooc_id, buyer_id, buyer_geography, goods, price, sla...
    ## dbl  (3): ...1, price_rijx, price_stuv
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
auction_example_goods <- data_auction("C:\\Users\\tessa\\OneDrive\\Desktop\\Masters 2023\\Data science\\Project\\data") %>% select(-bundle, -unit_price, -count) %>% mutate(year = substr(date, 1, 4))
auction_table <- full_join(auction_example, auction_example_goods, by = c("mooc_id", "purchase_id")) %>% 
    select(-date, -year, -buyer_id) %>% 
    arrange(purchase_id) %>% 
    filter(purchase_id< "MOOC10/10.1/20")


hux_auction_table <- huxtable(auction_table)
latex_auction_table <- to_latex(hux_auction_table)
```

# descriptives

``` r
Mean_auction_total <- mean(ml_df$auction_tot)
sd_auction_total <- sd(ml_df$auction_tot)

summary(model_df)
```

    ##    mooc_id            total_beds      total_books        total_chairs   
    ##  Length:1483        Min.   : 0.000   Min.   :   0.000   Min.   :  0.00  
    ##  Class :character   1st Qu.: 0.000   1st Qu.:   0.000   1st Qu.:  0.00  
    ##  Mode  :character   Median : 1.000   Median :   0.000   Median :  3.00  
    ##                     Mean   : 2.355   Mean   :   6.549   Mean   : 10.11  
    ##                     3rd Qu.: 3.000   3rd Qu.:   2.000   3rd Qu.: 13.00  
    ##                     Max.   :48.000   Max.   :1144.000   Max.   :154.00  
    ##   total_china       total_irons      total_jewlery     total_utensils   
    ##  Min.   :   0.00   Min.   : 0.0000   Min.   : 0.0000   Min.   :   0.00  
    ##  1st Qu.:   0.00   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.:   0.00  
    ##  Median :   0.00   Median : 0.0000   Median : 0.0000   Median :   4.00  
    ##  Mean   :  41.79   Mean   : 0.9737   Mean   : 0.3958   Mean   :  29.07  
    ##  3rd Qu.:  24.00   3rd Qu.: 1.0000   3rd Qu.: 0.0000   3rd Qu.:  30.00  
    ##  Max.   :8041.00   Max.   :37.0000   Max.   :60.0000   Max.   :3208.00  
    ##  total_art_products total_balance      total_clocks     total_coatrack   
    ##  Min.   :  0.000    Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000  
    ##  1st Qu.:  0.000    1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000  
    ##  Median :  0.000    Median : 0.0000   Median : 0.0000   Median : 0.0000  
    ##  Mean   :  4.591    Mean   : 0.7546   Mean   : 0.2583   Mean   : 0.6433  
    ##  3rd Qu.:  4.000    3rd Qu.: 1.0000   3rd Qu.: 0.0000   3rd Qu.: 1.0000  
    ##  Max.   :130.000    Max.   :21.0000   Max.   :10.0000   Max.   :32.0000  
    ##    total_guns      total_lamps        total_oven      total_plates   
    ##  Min.   : 0.000   Min.   : 0.0000   Min.   : 0.000   Min.   :  0.00  
    ##  1st Qu.: 0.000   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.:  0.00  
    ##  Median : 0.000   Median : 0.0000   Median : 0.000   Median :  4.00  
    ##  Mean   : 1.287   Mean   : 0.8422   Mean   : 1.122   Mean   : 25.54  
    ##  3rd Qu.: 2.000   3rd Qu.: 1.0000   3rd Qu.: 1.000   3rd Qu.: 30.00  
    ##  Max.   :48.000   Max.   :32.0000   Max.   :26.000   Max.   :914.00  
    ##    total_pot       total_tables     total_slaves     total_cattle    
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :   0.00  
    ##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:   0.00  
    ##  Median : 2.000   Median : 1.000   Median : 1.000   Median :   0.00  
    ##  Mean   : 4.983   Mean   : 2.695   Mean   : 3.419   Mean   :  28.59  
    ##  3rd Qu.: 7.000   3rd Qu.: 4.000   3rd Qu.: 4.000   3rd Qu.:  20.00  
    ##  Max.   :76.000   Max.   :36.000   Max.   :72.000   Max.   :1327.00  
    ##  total_hammers     total_horse_cart   total_horses      total_plough    
    ##  Min.   : 0.0000   Min.   :  0.000   Min.   :  0.000   Min.   : 0.0000  
    ##  1st Qu.: 0.0000   1st Qu.:  0.000   1st Qu.:  0.000   1st Qu.: 0.0000  
    ##  Median : 0.0000   Median :  0.000   Median :  0.000   Median : 0.0000  
    ##  Mean   : 0.5604   Mean   :  1.557   Mean   :  3.647   Mean   : 0.5253  
    ##  3rd Qu.: 0.0000   3rd Qu.:  1.000   3rd Qu.:  3.000   3rd Qu.: 0.0000  
    ##  Max.   :31.0000   Max.   :189.000   Max.   :155.000   Max.   :18.0000  
    ##   total_sheep      total_wagons       total_cups            date       
    ##  Min.   :   0.0   Min.   : 0.0000   Min.   :   0.000   Min.   :  2.00  
    ##  1st Qu.:   0.0   1st Qu.: 0.0000   1st Qu.:   0.000   1st Qu.: 50.00  
    ##  Median :   0.0   Median : 0.0000   Median :   0.000   Median : 70.00  
    ##  Mean   : 189.5   Mean   : 0.8699   Mean   :   8.155   Mean   : 66.76  
    ##  3rd Qu.:  80.0   3rd Qu.: 1.0000   3rd Qu.:   0.000   3rd Qu.: 91.00  
    ##  Max.   :5744.0   Max.   :26.0000   Max.   :3944.000   Max.   :102.00  
    ##   auction_tot       auction_size       women_tot     men_total     
    ##  Min.   :    0.2   Min.   :    1.0   Min.   :  0   Min.   :  0.00  
    ##  1st Qu.:  158.3   1st Qu.:   11.0   1st Qu.:  0   1st Qu.:  1.00  
    ##  Median :  699.5   Median :  107.0   Median :  1   Median :  8.00  
    ##  Mean   : 2347.2   Mean   :  370.8   Mean   : 10   Mean   : 26.47  
    ##  3rd Qu.: 2572.9   3rd Qu.:  389.5   3rd Qu.:  8   3rd Qu.: 28.00  
    ##  Max.   :58825.2   Max.   :14171.0   Max.   :354   Max.   :565.00  
    ##      year          
    ##  Length:1483       
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
# trying out plots 
trial_plots <- model_df %>% 
  filter(year >= 1720) %>% 
  mutate(time_period = ifelse(year > 1720 & year < 1730, "1720-1730", 
                              ifelse(year >= 1730 & year < 1740, "1730-1740", 
                                     ifelse(year >= 1740 & year < 1750, "1740-1750",
                                            ifelse(year >= 1750 & year < 1760, "1750-1760", 
                                                   ifelse(year >= 1760 & year < 1770, "1760-1770",
                                                          ifelse(year >= 1770 & year < 1780, "1770-1780",
                                                                 ifelse(year >= 1780 & year < 1790, "1780-1790",
                                                                        ifelse(year >= 1790 & year < 1800, "1790-1800", 
                                                                               ifelse(year >= 1800 & year < 1810, "1800-1810", 
                                                                                      ifelse(year >= 1810 & year < 1820, "1810-1820", NA))))))))))) %>% 
    filter(time_period!="NA")


# total auction value graph 
custom_breaks <- c(1720, 1740, 1760, 1780, 1800, 1820)
custom_labels <- c("1720", "1740", "1760", "1780", "1800", "1820")
trial_plots$year <- as.numeric(trial_plots$year)
```

    ## Warning: NAs introduced by coercion

``` r
total_auction <- ggplot(trial_plots, aes(x = year, y = auction_tot)) +
  geom_point() +
  labs(title = "Total auction value over time",
       x = "Year", y = "Auction total (Rix-dollars)") +
  scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.margin = margin(15, 10, 15, 10))

# plotting all goods
household_items <- c("beds", "chairs", "china", "clocks", "coatrack", "cups", "irons", "lamps", "tables", "balance")
household_items <- paste0("total_", household_items)
kitchen_items <- c("oven", "plates", "pot", "utensils")
kitchen_items <- paste0("total_", kitchen_items)
farming_items <- c("cattle", "sheep", "plough", "wagons", "horses", "horse_cart", "guns", "hammers")
farming_items <- paste0("total_", farming_items)
select_items <- c("books", "slaves", "art_products")
select_items <- paste0("total_", select_items)

# household goods

all_goods_household <- trial_plots %>% 
    select(mooc_id, year, time_period, household_items) %>% 
    group_by(time_period) %>% 
    summarise(total_beds=mean(total_beds), 
              total_chairs=mean(total_chairs), 
              total_china=mean(total_china), 
              total_clocks=mean(total_clocks), 
              total_coatrack=mean(total_coatrack), 
              total_cups=mean(total_cups), 
              total_irons=mean(total_irons), 
              total_lamps=mean(total_lamps), 
              total_tables=mean(total_tables), 
              total_balance=mean(total_balance)) %>% 
        gather(household_goods, value, household_items) %>% 
    ungroup()
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(household_items)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(household_items))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
household_goods <- ggplot(all_goods_household, aes(x=time_period, y=value, fill= household_goods)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values = c("#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF", "#009944", "#8C4799", "#FF6600", "#A6BDDB", "#00B33C"))+
     labs(title = "Number of houshold goods per auction over time",
       x = "Year", y = "Total number of household goods", 
       fill = "Household goods") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# kitchen items 

all_goods_kitchen <- trial_plots %>% 
    select(mooc_id, year, time_period, kitchen_items) %>% 
    group_by(time_period) %>% 
    summarise(total_oven=mean(total_oven), 
              total_plates=mean(total_plates), 
              total_pot=mean(total_pot), 
              total_utensils=mean(total_utensils)) %>% 
        gather(kitchen_goods, value, kitchen_items) %>% 
    ungroup()
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(kitchen_items)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(kitchen_items))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
kitchen_goods <- ggplot(all_goods_kitchen, aes(x=time_period, y=value, fill= kitchen_goods)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values = c("#FFB400", "#FFC740", "#C20008", "#FF020D"))+
     labs(title = "Number of kitchen goods per auction over time",
       x = "Year", y = "Total number of kitchen goods", 
       fill = "Kitchen goods") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.margin = margin(15, 10, 15, 10))

# farming items

all_goods_farm <- trial_plots %>% 
    select(mooc_id, year, time_period, farming_items) %>% 
    group_by(time_period) %>% 
    summarise(total_cattle=mean(total_cattle), 
              total_sheep=mean(total_sheep), 
              total_plough=mean(total_plough), 
              total_wagons=mean(total_wagons), 
              total_horses=mean(total_horses), 
              total_horse_cart=mean(total_horse_cart), 
              total_guns=mean(total_guns), 
              total_hammers=mean(total_hammers)) %>% 
        gather(farming_goods, value, farming_items) %>% 
    ungroup()
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(farming_items)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(farming_items))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
farming_goods <- ggplot(all_goods_farm, aes(x=time_period, y=value, fill= farming_goods)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values = c("#FFB400", "#FFC740","#8C4799" , "#FF020D", "#13AFEF", "#009944","#C20008" , "#FF6600"))+ 
     labs(title = "Number of farming goods per auction over time",
       x = "Year", y = "Total number of farming goods", 
       fill = "Farming goods")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.margin = margin(15, 10, 15, 10))

# select items

all_goods_select <- trial_plots %>% 
    select(mooc_id, year, time_period, select_items) %>% 
    group_by(time_period) %>% 
    summarise(total_books=mean(total_books),
              total_slaves=mean(total_slaves), 
              total_art_products=mean(total_art_products)) %>% 
        gather(select_goods, value, select_items) %>% 
    ungroup()
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(select_items)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(select_items))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
select_goods <- ggplot(all_goods_select, aes(x=time_period, y=value, fill= select_goods)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values = c("#FFB400", "#C20008", "#FF020D"))+
     labs(title = "Number of select goods per auction over time",
       x = "Year", y = "Total number of select goods", 
       fill = "Select goods") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.margin = margin(15, 10, 15, 10))

 
# focus on 5 most common goods and how bundle changes over time 

big_five_df <- model_df %>% 
  filter(year >= 1720) %>% 
  mutate(time_period = ifelse(year > 1720 & year < 1755, "1720-1755", 
                              ifelse(year >= 1755 & year < 1790, "1755-1790", 
                                     ifelse(year >= 1790 & year < 1820, "1790-1820", "NA"))))%>% filter(time_period!="NA") %>% 
    dplyr::select(time_period, total_cattle, total_sheep, total_horses, total_slaves, total_utensils) %>% 
    gather(good, value, total_cattle, total_sheep, total_horses, total_slaves, total_utensils) %>% 
    filter(value<200)
```

    ## Adding missing grouping variables: `year`

``` r
big_five_boxplots <- ggplot(big_five_df, aes(x = time_period, y = value, fill = good)) +
  geom_boxplot() +
  facet_wrap(~time_period, scales = "free_x") +
  labs(title = "Boxplots of the most frequently purchased goods by time period",
       x = "Time Period",
       y = "Number of purchases",
       fill = "Good type") +
  scale_fill_manual(values = c("#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_blank(),
        plot.margin = margin(15, 10, 15, 10))


# attendance

library("remotes")
```

    ## Warning: package 'remotes' was built under R version 4.2.3

``` r
library("ggstream")
```

    ## Warning: package 'ggstream' was built under R version 4.2.3

``` r
library(ggplot2)

attendance_df <-  model_df %>% 
  filter(year >= 1720) %>% 
  mutate(time_period = ifelse(year > 1720 & year < 1730, "1720-1730", 
                              ifelse(year >= 1730 & year < 1740, "1730-1740", 
                                     ifelse(year >= 1740 & year < 1750, "1740-1750",
                                            ifelse(year >= 1750 & year < 1760, "1750-1760", 
                                                   ifelse(year >= 1760 & year < 1770, "1760-1770",
                                                          ifelse(year >= 1770 & year < 1780, "1770-1780",
                                                                 ifelse(year >= 1780 & year < 1790, "1780-1790",
                                                                        ifelse(year >= 1790 & year < 1800, "1790-1800", 
                                                                               ifelse(year >= 1800 & year < 1810, "1800-1810", 
                                                                                      ifelse(year >= 1810 & year < 1820, "1810-1820", NA))))))))))) %>% 
    filter(time_period!="NA") %>% 
    group_by(time_period) %>% 
    summarise(women=mean(women_tot), 
              men=mean(men_total)) %>% 
    gather(attendance, number, women, men)
attendance_df$number <- as.numeric(attendance_df$number)


attendance_plot <- ggplot(attendance_df, aes(x = time_period, y = number, fill = attendance)) +
  geom_bar(stat = "identity", position = "stack")+
      labs(title = "Average attendance by titled men and women over time",
       x = "Time Period",
       y = "Average attendance",
       fill = "Titled") +
  scale_fill_manual(values = c("#FFC740", "#C20008")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.margin = margin(15, 10, 15, 10))
    
# bivariate analysis 

auction_size <- scatter_plotter(trial_plots$auction_size, "Auction values vs auction\n size","Auction size")
cattle <- scatter_plotter(trial_plots$total_cattle, "Auction values vs cattle\n purchases","Total number of cattle\n purhased per auction")
slaves <- scatter_plotter(trial_plots$total_slaves, "Auction values vs slave\n purchases","Total number of slaves\n purhased per auction")
sheep <- scatter_plotter(trial_plots$total_sheep, "Auction values vs sheep\n purchases","Total number of sheep\n purhased per auction")
horses <- scatter_plotter(trial_plots$total_horses, "Auction values vs horse\n purchases","Total number of horses\n purhased per auction")
men <- scatter_plotter(trial_plots$men_total, "Auction values vs titled\n men attendance","Total number of titled\n men per auction")

bivariat_plot <- grid.arrange(auction_size, cattle, slaves, sheep, horses, men, ncol = 3, nrow = 2)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

feature and target transformations

``` r
auction_unlogged <- unlogged_distribution(model_df$auction_tot, "Distribution of total auction values", "Total auction value")
auction_logged <- model_df %>% mutate(auction_tot=log(auction_tot)) %>% 
     ggplot(aes(x=auction_tot))+
    geom_density()+
    labs(title = "Distribution of logged\n total auction values",
         x = "Logged total auction value", y = "Density") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"))
cattle_unlogged <- unlogged_distribution(model_df$total_cattle, "Distribution of number\n of cattle purchases", "Total cattle purchases per auction")
cattle_logged <- model_df %>% mutate(cattle=log(total_cattle)) %>% 
     ggplot(aes(x=cattle))+
    geom_density()+
    labs(title = "Distribution of logged\n cattle purchases",
         x = "Logged cattle purchases per auction", y = "Density") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"))
men_attend_unlogged <- unlogged_distribution(model_df$men_total, "Distribution of titled\n men auction attendance", "Titled men attendance\n per auction")
men_attend_logged <- model_df %>% mutate(men_total=log(men_total)) %>% 
     ggplot(aes(x=men_total))+
    geom_density()+
    labs(title = "Distribution of logged\n titled men attendance",
         x = "Logged titled men\n attendance per auction", y = "Density") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"))

distribution_plots <- grid.arrange(auction_unlogged, auction_logged, cattle_unlogged, cattle_logged, men_attend_unlogged, men_attend_logged, ncol = 2, nrow = 3)
```

    ## Warning: Removed 946 rows containing non-finite values (`stat_density()`).

    ## Warning: Removed 273 rows containing non-finite values (`stat_density()`).

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

# regression

``` r
reg <- lm(auction_tot ~ ., data = ml_df)
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = auction_tot ~ ., data = ml_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5948 -0.5293  0.0539  0.6001  4.1455 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -0.313766   0.258401  -1.214 0.224846    
    ## total_beds          0.030920   0.051578   0.599 0.548949    
    ## total_books         0.015259   0.031399   0.486 0.627072    
    ## total_chairs        0.261487   0.050099   5.219 2.05e-07 ***
    ## total_china        -0.141661   0.030891  -4.586 4.91e-06 ***
    ## total_irons        -0.038633   0.080620  -0.479 0.631865    
    ## total_jewlery      -0.103175   0.084421  -1.222 0.221852    
    ## total_utensils      0.047291   0.029330   1.612 0.107102    
    ## total_art_products -0.033569   0.039911  -0.841 0.400440    
    ## total_balance      -0.117334   0.081773  -1.435 0.151540    
    ## total_clocks        0.035921   0.161785   0.222 0.824322    
    ## total_coatrack     -0.051524   0.084728  -0.608 0.543211    
    ## total_guns          0.131654   0.057684   2.282 0.022615 *  
    ## total_lamps        -0.102747   0.088199  -1.165 0.244232    
    ## total_oven          0.118811   0.066142   1.796 0.072653 .  
    ## total_plates        0.010087   0.040727   0.248 0.804424    
    ## total_pot          -0.143148   0.052739  -2.714 0.006721 ** 
    ## total_tables        0.027901   0.071554   0.390 0.696644    
    ## total_slaves        0.841320   0.046059  18.266  < 2e-16 ***
    ## total_cattle        0.118041   0.031233   3.779 0.000164 ***
    ## total_hammers       0.002032   0.070919   0.029 0.977149    
    ## total_horse_cart    0.061872   0.054133   1.143 0.253238    
    ## total_horses        0.026942   0.046512   0.579 0.562515    
    ## total_plough       -0.723920   0.107709  -6.721 2.58e-11 ***
    ## total_sheep         0.014712   0.024532   0.600 0.548800    
    ## total_wagons        0.108481   0.101793   1.066 0.286739    
    ## total_cups         -0.052726   0.035224  -1.497 0.134640    
    ## date                1.056096   0.057965  18.219  < 2e-16 ***
    ## auction_size        0.249047   0.033901   7.346 3.39e-13 ***
    ## women_tot           0.007093   0.033654   0.211 0.833102    
    ## men_total           0.205969   0.033692   6.113 1.25e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.138 on 1452 degrees of freedom
    ## Multiple R-squared:  0.705,  Adjusted R-squared:  0.6989 
    ## F-statistic: 115.7 on 30 and 1452 DF,  p-value: < 2.2e-16

``` r
Title <- "Auction total value regression"
Label <- "Auctiont total"
reg_tidy <- tidy(reg)

output_reg <- huxreg(reg_tidy, note = "%stars%.") %>%
  set_caption(Title)
```

    ## Warning in FUN(X[[i]], ...): Error calling `glance` on model 1, of class
    ## `tbl_df`:

    ## Warning in FUN(X[[i]], ...): Error : There is no glance method for tibbles. Did you mean `tibble::glimpse()`?

    ## Warning in nobs.default(m, use.fallback = TRUE): no 'nobs' method is available

    ## Warning in huxreg(reg_tidy, note = "%stars%."): Unrecognized statistics: r.squared, logLik, AIC
    ## Try setting `statistics` explicitly in the call to `huxreg()`

``` r
output_reg <- output_reg %>% slice(-1, -(nrow(output_reg) - 1))

latex_reg <- to_latex(output_reg)

# comparison
residuals <- reg$residuals
rmse <- sqrt(mean(residuals^2)) # 1.13
r_squared <- summary(reg)$r.squared # 0.71
```

# splitting into testing and training

``` r
# testing and training 
set.seed(123)
indices <- sample(nrow(ml_df))
train_size <- floor(0.6 * nrow(ml_df))
training_data <- ml_df[indices[1:train_size], ]
testing_data <- ml_df[indices[(train_size + 1):nrow(ml_df)], ]
```

# knn method

``` r
sampling_strat <- caret::trainControl(
    method="repeatedcv", 
    number=10, 
    repeats=5
)

hyper_grid <- expand.grid(k = seq(1, 25, by = 1))

knn_fit <- train(
  auction_tot ~ ., 
  data = training_data, 
  method = "knn", 
  trControl = sampling_strat, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)


knn_plot <- ggplot(knn_fit)+
    labs(title= "Optimal k-value", 
         x="Neighbours")+
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# k = 5 has the lowest RMSE
#  k=5 RMSE = 1.345573  Rsquared = 0.5987131 MAE = 0.9381762  

library(caret)

# Train the KNN model with the chosen k value
k <- 5
knn_model <- train(
  auction_tot ~ .,
  data = training_data,
  method = "knn",
  trControl = sampling_strat,
  tuneGrid = data.frame(k = k),
  metric = "RMSE"
)
knn_model
```

    ## k-Nearest Neighbors 
    ## 
    ## 889 samples
    ##  30 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 800, 800, 798, 801, 801, 800, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE      
    ##   1.348051  0.5968507  0.9350051
    ## 
    ## Tuning parameter 'k' was held constant at a value of 5

``` r
# Make predictions on the testing data
predictions_knn <- predict(knn_model, newdata = testing_data)
RMSE(predictions_knn, testing_data$auction_tot) # 1.17
```

    ## [1] 1.172112

``` r
MAE(predictions_knn, testing_data$auction_tot) # 0.83
```

    ## [1] 0.836182

``` r
R2(predictions_knn, testing_data$auction_tot) # r-squared: 0.66
```

    ## [1] 0.6603818

``` r
# plot
prediction_knn_df <- data.frame(
  Actual = testing_data$auction_tot,
  Predicted = predictions_knn
) 
prediction_knn_df <- prediction_knn_df %>% 
    mutate(accuracy= ifelse(abs(Predicted-Actual)<0.1, "Within 0.1 of predicted value", 
                            ifelse(abs(Predicted-Actual)<0.2, "Within 0.2 of predicted value", 
                                   ifelse(abs(Predicted-Actual)<0.3, "Within 0.3 of predicted value", 
                                          "More than 0.3 from predicted value"))))

knn_predictions_plot <- ggplot(prediction_knn_df, aes(x = Actual, y = Predicted, colour=accuracy)) +
  geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "knn model: predictions vs actual values", x = "Actual", y = "Predicted")+
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
```

# random forest

``` r
dim(ml_df)
```

    ## [1] 1483   31

``` r
n_features <- length(setdiff(names(training_data), "auction_tot"))


# train a default random forest model
training_rf <- ranger(
  auction_tot ~ ., 
  data = training_data,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  importance= "permutation",
  seed = 123
)
training_rf
```

    ## Ranger result
    ## 
    ## Call:
    ##  ranger(auction_tot ~ ., data = training_data, mtry = floor(n_features/3),      respect.unordered.factors = "order", importance = "permutation",      seed = 123) 
    ## 
    ## Type:                             Regression 
    ## Number of trees:                  500 
    ## Sample size:                      889 
    ## Number of independent variables:  30 
    ## Mtry:                             10 
    ## Target node size:                 5 
    ## Variable importance mode:         permutation 
    ## Splitrule:                        variance 
    ## OOB prediction error (MSE):       1.201175 
    ## R squared (OOB):                  0.7317454

``` r
default_rmse <- sqrt(training_rf$prediction.error) # RMSE: 1.1
rf_importance <- ranger::importance(training_rf)
sort(rf_importance, decreasing = TRUE)
```

    ##       total_slaves       auction_size               date          men_total 
    ##       2.0709986939       1.1675256521       0.8769795417       0.5941417931 
    ##       total_chairs       total_plates     total_utensils          total_pot 
    ##       0.5820742967       0.4190024827       0.2884131821       0.2372129686 
    ##        total_china      total_balance         total_beds       total_cattle 
    ##       0.1551568313       0.1440438703       0.1317438275       0.1278095933 
    ##          women_tot        total_sheep         total_guns       total_tables 
    ##       0.1099365076       0.1087080797       0.0815461205       0.0800542503 
    ##       total_horses       total_plough   total_horse_cart        total_lamps 
    ##       0.0729479193       0.0717635415       0.0594090271       0.0499657630 
    ##     total_coatrack        total_books       total_wagons total_art_products 
    ##       0.0409177238       0.0399103094       0.0361094745       0.0193542969 
    ##         total_cups         total_oven        total_irons      total_jewlery 
    ##       0.0151104075       0.0140819272       0.0070677599       0.0046132689 
    ##       total_clocks      total_hammers 
    ##       0.0002587413       0.0001944009

``` r
# parameter grid
#parameter_grid <- expand.grid(
#  mtry = c(floor(n_features / 3)),
#  num.trees = c(100, 300, 500),
#  min.node.size = c(1, 5, 10)
#)

#results <- lapply(1:nrow(parameter_grid), function(i) {
#  model <- ranger(
#    auction_tot ~ .,
#    data = training_data,
#    num.trees = parameter_grid$num.trees[i],
#    mtry = parameter_grid$mtry[i],
#    min.node.size = parameter_grid$min.node.size[i],
#   respect.unordered.factors = "order",
#   importance = "permutation",
#    seed = 123
#  )
#  predictions <- predict(model, data = testing_data)$predictions
#  rmse <- sqrt(mean((predictions - testing_data$auction_tot)^2))
#  list(model = model, rmse = rmse)
#})

#best_model_index <- which.min(sapply(results, function(x) x$rmse))
#best_model <- results[[best_model_index]]$model

#train_predictions <- predict(best_model, data = training_data)$predictions
#rmse_train <- sqrt(mean((train_predictions - training_data$auction_tot)^2)) # RMSE: 0.68

# baseline model is the best
default_predictions <- predict(training_rf, data = testing_data)$predictions
rmse_default_test <- sqrt(mean((default_predictions - training_data$auction_tot)^2)) # 2.68
```

    ## Warning in default_predictions - training_data$auction_tot: longer object
    ## length is not a multiple of shorter object length

``` r
# plot
rf_predictions <- data.frame(
  Actual = testing_data$auction_tot,
  Predictions = default_predictions
) %>% 
    mutate(Accuracy= ifelse(abs(Predictions-Actual)<0.1, "Within 0.1 of predicted value", 
                            ifelse(abs(Predictions-Actual)<0.2, "Within 0.2 of predicted value", 
                                   ifelse(abs(Predictions-Actual)<0.3, "Within 0.3 of predicted value", 
                                          "More than 0.3 from predicted value"))))

rf_plot <- ggplot(rf_predictions, aes(x = Actual, y = Predictions, color = Accuracy)) +
  geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Random forest: predictions vs actual values", x = "Actual", y = "Predicted")+
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
```

# importance plot

``` r
rf_importance_df <- data.frame(
    Importance = training_rf[["variable.importance"]]
) %>% rownames_to_column() %>% arrange(desc(Importance))
colnames(rf_importance_df) <- c("Feature", "Importance")


importance_plot <- ggplot(rf_importance_df, aes(x= reorder(Feature, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#C20008" ) +
  labs(title = "Feature Importance", x = "Feature", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=7))+
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"))
```
