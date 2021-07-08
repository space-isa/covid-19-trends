library(readr)
library(tibble)
library(tidyverse)
library(dplyr)
library(ggplot2)

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

covid_top_10
par(mfrow=c(1,2))

#  Horizontal bar plot: Ratios for the top 10 countries
round(covid_top_10$ratio, digits = 3) <- covid_top_10
plot <- ggplot(data=covid_top_10, aes(x=ratio, y=reorder(Country_Region, -ratio), fill=ratio)) +
        geom_bar(stat="identity") + guides(fill=F) +
        #geom_text(aes(label=round(ratio, digits = 3)), vjust=1.6, color="white", size=3.5) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) + 
        ggtitle("Ratio of positive cases vs. number of tests") + xlab(" ") + ylab(" ")
plot

# Bar plot: Cumulative cases in the US
filtered_us <- filter(covid_df, Country_Region == "United States")

plot2 <- ggplot(data=filtered_us, aes(x=Date, y=daily_positive)) +
  geom_bar(stat="identity", fill="#53b0e2") +
  #geom_text(aes(label=max(daily_positive)), vjust=1.6, color="white", size=3.5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  ggtitle("Cumulative cases in the United States") + xlab(" ") + ylab("Number of Cases")
plot2

# Bar plot: Cumulative cases in the UK
filtered_uk <- filter(covid_df, Country_Region == "United Kingdom")

plot3 <- ggplot(data=filtered_uk, aes(x=Date, y=daily_positive)) +
  geom_bar(stat="identity", fill="#56B4E9") +
  #geom_text(aes(label=max(daily_positive)), vjust=1.6, color="white", size=3.5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ggtitle("Cumulative cases in the United Kingdom") + xlab(" ") + ylab(" ")
plot3

require(gridExtra)
grid.arrange(plot, 
             plot2, plot3, 
             nrow=2, ncol=2,
             layout_matrix = rbind(c(1,1), c(2,3)))

#  Number of countries included in the data
length(unique(covid_df$Country_Region))
