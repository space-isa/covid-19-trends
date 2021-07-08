library(readr)
library(tibble)
library(tidyverse)
library(dplyr)

#  Load csv file
covid_df <- read_csv('./input/covid19.csv')

#  Determine dimensions
dim(covid_df)

#  Determine column names
vector_cols <- colnames(covid_df)

#  Explore dataset
head(covid_df)
glimpse(covid_df)

#  Filter by "All States" and remove the Province_State column
covid_df_all_states_us <- covid_df %>% 
                              filter(Province_State == "All States") %>% 
                              select(-Province_State) 
print(covid_df_all_states_us)

#  Goal 1: determine top ten cases by country
cols_to_keep <- c("Date", 
                  "Country_Region", 
                  "active", 
                  "hospitalizedCurr", 
                  "daily_tested", 
                  "daily_positive")

covid_df_all_states_dialy <- covid_df_all_states_us %>% 
                                select(Date, cols_to_keep)

print(covid_df_all_states_dialy)

covid_df_all_states_dialy_sum <- covid_df_all_states_dialy %>% 
                                    group_by(Country_Region) %>% 
                                    summarize(tested=sum(daily_tested),
                                              positive=sum(daily_positive),
                                              active=sum(active),
                                              hospitalized=sum(hospitalizedCurr)) %>%
                                    arrange(-tested)
print(covid_df_all_states_dialy_sum)

covid_top_10 <- head(covid_df_all_states_dialy_sum, 10)
print(covid_top_10)

#  Goal 2: Which countries have had the highest number of positive cases vs 
#  the number of tests?

#  Create vectors from columns
countries <-  covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

#  Name tested_cases through hospitalized_cases using the countries vector
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

#  Calculate ratio of positive to tested cases 
ratio_positive_tested_cases <-  positive_cases / tested_cases

#  Create a new column with calculated ratios
covid_top_10$ratio <- ratio_positive_tested_cases

#  Determine top 3 (descending order)
sorted_ratio_desc <- covid_top_10 %>% arrange(desc(ratio))
top_3_countries <- head(sorted_ratio_desc, n=3)
top_3_countries

#  Displaying final answers
question1 <- "Which 10 countries had the most cases between 1/20/20 and 6/1/20?"
answer1 <- c("Top 10" = covid_top_10["Country_Region"])

question2 <- "Which countries have had the highest number of positive cases vs 
the number of tests between 1/20/20 and 6/1/20?"
answer2 <- c("Positive tested cases" = top_3_countries["ratio"])

question1
answer1

question2
answer2
