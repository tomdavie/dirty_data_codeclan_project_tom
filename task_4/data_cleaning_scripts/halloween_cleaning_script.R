library(tidyverse)
library(janitor)
library(readxl)
library(here)

boing_boing_candy_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx"))
boing_boing_candy_2016 <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx"))
boing_boing_candy_2017 <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx"))

# selecting useful columns
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  select(2:96, 115)

boing_boing_candy_2016 <- boing_boing_candy_2016 %>% 
  select(2:106)

boing_boing_candy_2017 <- boing_boing_candy_2017 %>% 
  select(2:109)

# Go back and look at 'other joy / despair' text box and how could incorporate

# renaming columns 
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  rename(
    age = "How old are you?",
    going_trick_or_treating = "Are you going actually going trick or treating yourself?") 

boing_boing_candy_2016 <- boing_boing_candy_2016 %>% 
  rename(
    going_trick_or_treating = "Are you going actually going trick or treating yourself?",
    gender = "Your gender:",
    age = "How old are you?",
    country = "Which country do you live in?",
    state = "Which state, province, county do you live in?"
  )

boing_boing_candy_2017 <- boing_boing_candy_2017 %>% 
  rename(
    going_trick_or_treating = "Q1: GOING OUT?",
    gender = "Q2: GENDER",
    age = "Q3: AGE",
    country = "Q4: COUNTRY",
    state = "Q5: STATE, PROVINCE, COUNTY, ETC"
  )

# Adding missing gender, country and state columns and filling with NAs
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  add_column(gender = NA,
             country = NA,
             state = NA)

# I considered giving each person an 'id' in order to count people. However, I 
# realised this wasn't necessary for our analysis. The below code was the 
# beginning of this. 

#boing_boing_candy_2015 <- tibble::rowid_to_column(boing_boing_candy_2015, "id")
#boing_boing_candy_2016 <- tibble::rowid_to_column(boing_boing_candy_2016, "id")
#boing_boing_candy_2017 <- tibble::rowid_to_column(boing_boing_candy_2017, "id")

# pivoting 2015 table
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  pivot_longer(4:96, names_to = "candy_name", values_to = "review")

# ordering columns to match other dfs before join
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  select(#"id", 
    "going_trick_or_treating", "gender", "age", "country", "state", "candy_name", "review")

# pivoting 2016 table
boing_boing_candy_2016 <- boing_boing_candy_2016 %>% 
  pivot_longer(6:105, names_to = "candy_name", values_to = "review")

#pivoting 2017 table
boing_boing_candy_2017 <- boing_boing_candy_2017 %>% 
  pivot_longer(6:108, names_to = "candy_name", values_to = "review")

# Adding year columns to answer question 7 
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  add_column(year = 2015, .before = 1)

boing_boing_candy_2016 <- boing_boing_candy_2016 %>% 
  add_column(year = 2016, .before = 1)

boing_boing_candy_2017 <- boing_boing_candy_2017 %>% 
  add_column(year = 2017, .before = 1)

# Removing square brackets from 'candy name' column in 2015 and 2016
boing_boing_candy_2015 <- boing_boing_candy_2015 %>% 
  mutate(candy_name = str_sub(candy_name, 2, -2))

boing_boing_candy_2016 <- boing_boing_candy_2016 %>% 
  mutate(candy_name = str_sub(candy_name, 2, -2))

boing_boing_candy_2017 <- boing_boing_candy_2017 %>% 
  mutate(candy_name = str_sub(candy_name, start = 6,))

# Combining the three dataframes into one 
boing_boing_candy_2015_to_2017 <- rbind(boing_boing_candy_2017, boing_boing_candy_2016, boing_boing_candy_2015)

# Converting 'age' to an integer column
boing_boing_candy_2015_to_2017 <- boing_boing_candy_2015_to_2017 %>% 
  mutate(age = as.integer(age))

# Pulled a distinct list of countries inputs and used regexr.com to figure out appropriate regex to clean up

# cleaning the 'country' column 
boing_boing_candy_2015_to_2017_clean <- boing_boing_candy_2015_to_2017 %>%
  mutate(
    country = case_when(
      str_detect(country, "North Carolina") ~ "United States of America",
      str_detect(country, "California") ~ "United States of America",
      str_detect(country, "Pittsburgh") ~ "United States of America",
      str_detect(country, "New York") ~ "United States of America",
      str_detect(country, "New Jersey") ~ "United States of America",
      str_detect(country, "\\s+[S|s]t*[a|e|s]") ~ "United States of America",
      str_detect(country, "[U|u][. ]*[S|s] *[A|a]*") ~ "United States of America",
      str_detect(country, "The Yoo Ess of Aaayyyyyy") ~ "United States of America",
      str_detect(country, "[Cc]ascadia") ~ "United States of America",
      str_detect(country, "[Mm][u|e]r+i[c|k]a") ~ "United States of America",
      str_detect(country, "Trumpistan") ~ "United States of America",
      str_detect(country, "soviet canuckistan") ~ "Canada",
      str_detect(country, "[U|u]\\.?[K|k]") ~ "United Kingdom",
      str_detect(country, "[UnitedKindom]{6}") ~ "United Kingdom",
      str_detect(country, "[g|t|d]land") ~ "United Kingdom",
      TRUE ~ NA_character_
    )
  ) 

# extra lines I coded but realised they were unnecessary towards the three 
# countries we were analysing

#              str_detect(country, "Brasil") ~ "Brazil",
#              str_detect(country, "[Nn]etherlands") ~ "The Netherlands",
#              str_detect(country, "EUA|UAE") ~ "United Arab Emirates",
# str_detect(country, "\\w+\\s\\w+\\s\\w") ~ "NA",
#              str_detect(country, "this one") ~ "NA",
#              str_detect(country, "god's country") ~ "NA",
#              str_detect(country, "Somewhere") ~ "NA",
#              str_detect(country, "españa") ~ "NA",
#              str_detect(country, "Neverland") ~ "NA",
#              str_detect(country, "See above") ~ "NA", #could look for this
#              str_detect(country, "Denial") ~ "NA",
#              str_detect(country, "Europe") ~ "NA",
#              str_detect(country, "Earth") ~ "NA",
#              str_detect(country, "insanity lately") ~ "NA",
#              str_detect(country, "A") ~ "NA",
#              str_detect(country, "Can") ~ "NA",
#              str_detect(country, "UD") ~ "NA",
#              str_detect(country, "Narnia") ~ "NA",
#              str_detect(country, "Not the USA or Canada") ~ "NA", 
#              str_detect(country, "\\d") ~ "NA",
#              str_detect(country, "N. America") ~ "NA",
#              str_detect(country, " *Ame") ~ "NA",


# Taking out non-candies 
boing_boing_candy_2015_to_2017_clean <- 
  boing_boing_candy_2015_to_2017_clean %>%
  mutate(
    candy_name = case_when(
      str_detect(candy_name, "M&M") ~ "M&M's",
      str_detect(candy_name, "Anon") ~ "Mary Janes",
      str_detect(candy_name, "[Gg]ummy Bears") ~ "Gummy Bears",
      str_detect(candy_name, "Jolly Rancher") ~ "Jolly Rancher",
      str_detect(candy_name, "[Ll]icorice") ~ "Licorice",
      str_detect(candy_name, "baseball") ~ "Gum",
      str_detect(candy_name, "Tolberone") ~ "Tolberone",
      str_detect(candy_name, "Chick-o-Sticks") ~ "Chick-o-Sticks",
      str_detect(candy_name, "[Cc]ircus") ~ "Circus Peanuts",
      str_detect(candy_name, "[Pp]eanut [Bb]utter [B|J]") ~ "Peanut Butter Bars",
      str_detect(candy_name, "Sourpatch") ~ "Sourpatch Kids",
      str_detect(candy_name, "Sweetums") ~ "Sweetums",
      str_detect(candy_name, "Dark Chocolate") ~ "Hershey's Dark Chocolate",
      str_detect(candy_name, "vein") ~ "NA",
      str_detect(candy_name, "restaurant") ~ "NA",
      str_detect(candy_name, "vein") ~ "NA",
      str_detect(candy_name, "[Cc]ash") ~ "NA",
      str_detect(candy_name, "vein") ~ "NA",
      str_detect(candy_name, "[Dd]ental") ~ "NA",
      str_detect(candy_name, "Acetam") ~ "NA",
      str_detect(candy_name, "[Gg]low *stick") ~ "NA",
      str_detect(candy_name, "[Gg]low *stick") ~ "NA",
      str_detect(candy_name, "[Cc]omics") ~ "NA",
      str_detect(candy_name, "[Ff]ruit") ~ "NA",
      str_detect(candy_name, "[Hh]ugs") ~ "NA",
      str_detect(candy_name, "[Kk]ale") ~ "NA",
      str_detect(candy_name, "[Ll]apel|[Pp]in") ~ "NA",
      str_detect(candy_name, "[Ll]apel|[Pp]in") ~ "NA",
      str_detect(candy_name, "[Ll]apel|[Pp]in") ~ "NA",
      str_detect(candy_name, "[Cc]hips") ~ "NA",
      str_detect(candy_name, "[Ll]apel|[Pp]in") ~ "NA",
      str_detect(candy_name, "Iodine") ~ "NA",
      str_detect(candy_name, "[Ss]potted [Dd]ick") ~ "NA",
      str_detect(candy_name, "[Tt]rail Mix") ~ "NA",
      str_detect(candy_name, "[Vv]icodin") ~ "NA",
      str_detect(candy_name, "[Vv]icodin") ~ "NA",
      str_detect(candy_name, "[Bb]read") ~ "NA",
      str_detect(candy_name, "[Ww]heat") ~ "NA",
      str_detect(candy_name, "[Bb]read") ~ "NA",
      str_detect(candy_name, "game") ~ "NA",
      str_detect(candy_name, "[Bb]onkers") ~ "Bonkers",
      str_detect(candy_name, "[Rr]aisins") ~ "Raisins",
      str_detect(candy_name, "[Cc]hardonnay") ~ "NA",
      str_detect(candy_name, "[Ss]eason") ~ "NA",
      str_detect(candy_name, "Whatchamacallit Bars") ~ "NA",
      str_detect(candy_name, "[Ss]andwich") ~ "NA",
      TRUE ~ candy_name
    )
  ) %>% mutate(candy_name = na_if(candy_name, "NA")) 

write_csv(boing_boing_candy_2015_to_2017_clean, "clean_data/halloween_clean_data.csv")
