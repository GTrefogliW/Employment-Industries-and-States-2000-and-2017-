
# Name of project: Employment, Industries, and States (2000 and 2017)

library(tidyverse)

#hw1_indus <- "C:\\Users\\guill\\OneDrive\\Documents\\Data and Programming II\\SAEMP25N by industry.csv"
#hw1_total <- "C:\\Users\\guill\\OneDrive\\Documents\\Data and Programming II\\SAEMP25N total.csv"

# dropping useless rows 
df_indus <- read_csv(hw1_indus, skip = 4)
n <- dim(df_indus)[1]
df_indus <- df_indus[1:(n-5),]

df_total <- read_csv(hw1_total, skip = 4)
n <- dim(df_total)[1]
df_total <- df_total[1:(n-3),]

# dropping useless columns
df_indus_fltr <- df_indus[-c(1,3)]
df_total_fltr <- df_total[-1]

# renaming columns
df_total_renam <- rename(df_total_fltr, "state" = "GeoName")

df_indus_renam <- rename(df_indus_fltr, "state" = "GeoName",
                         "industry" = "Description")

# making the two dataset longer to create "year" column
df_indus_year <- df_indus_renam %>% 
  pivot_longer(cols = "2000":"2017",
               names_to = "year",
               values_to = "employment")

df_total_year <- df_total_renam %>% 
  pivot_longer(cols = "2000":"2017",
               names_to = "year",
               values_to = "employment_state")

# dropping missing values from the "By Industry" dataset. 

#Note: I will report the shares of labor per industry subject 
# to the valid information in the dataset.

df_indus_na <- filter(df_indus_year, 
                      industry != "By industry",
                      employment != "(D)",
                      employment != "(T)")
# merging datasets
df_merged <- df_indus_na %>% left_join(df_total_year, 
                                       by = c("state" = "state", "year" = "year"))

# calculating shares
df_merged$employment <- as.numeric(df_merged$employment)
df_merged$employment_state <- as.numeric(df_merged$employment_state)

df_shares <- mutate(df_merged, share = employment / employment_state)
df_shares$share <- round(df_shares$share, 3)
  
# making the dataset wider to create columns per industry
df_shares <- select(df_shares, -c("employment", "employment_state"))

df_12 <- df_shares %>% pivot_wider(names_from = "industry",
                                   values_from = "share")

#write.csv(df_12,"C:/Users/guill/OneDrive/Documents/Data and Programming II/data.csv", 
#          row.names = FALSE)


# states with the top five share of manufacturing employment in the year 2000
top_manuf_2000 <- df_12 %>% 
  filter(year == 2000) %>% 
  select(state, Manufacturing) %>% 
  arrange(desc(Manufacturing)) %>% 
  head(5)

# changes in share of employment in manufacturing between 2000 and 2017
df_list <- top_manuf_2000[[1]]

df_mf <- df_12 %>% 
  filter(state %in% df_list) %>%
  select(state, year, Manufacturing) %>% 
  pivot_wider(names_from = year,
              values_from =  Manufacturing)

df_mf_change <- mutate(df_mf, mf_change = df_mf[3] - df_mf[2])

df_plot <- df_mf %>%
  pivot_longer(cols = "2000":"2017",
               names_to = "year",
               values_to = "share") 
df_plot %>% 
  ggplot(aes(year, share, group = state, color = state)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year",
       y = "Share",
       title = "Share of Labor Market in Manufacturing (1981-2010)",
       subtitle = "Top 5 States")

# five states with the highest concentration of employment in a any single industry
# in each of 2000 and 2017
list_2000 <- df_shares %>% 
  filter(year == "2000") %>% 
  arrange(desc(share)) %>% 
  head(5)

list_2017 <- df_shares %>% 
  filter(year == "2017") %>% 
  arrange(desc(share)) %>% 
  head(5)

print(list_2000)
print(list_2017)

# end.

