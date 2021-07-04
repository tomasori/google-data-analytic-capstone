install.packages("tidyverse")
install.packages("ggplot2")
library("tidyverse")
library("ggplot2")

# libraries to adjust color of ggplots
install.packages("viridis")
library("viridis")




## read in CSV file created from BigQuery
## Assign data types since year and number were being read in as a double
## without assigning it upfront

tomas_csv <- read_csv(file = '~/Capstone Case Study/3 - R Studio/versions_of_tomas_usa_names_1920_to_2020.csv',
                      col_types = cols(
                        year = col_integer(),
                        state = col_character(),
                        number = col_integer(),
                        gender = col_character(),
                        name = col_character() ))

## quick check of data ( a few options to see a summary of the data)
colnames(tomas_csv)
glimpse(tomas_csv)
head(tomas_csv)
str(tomas_csv)




###########################################################################
## show data for total number of names per year

tomas_yr_total_ds <- tomas_csv %>%
  group_by(year,name) %>% 
  summarize(yr_total_all_names = sum(number))

#test dataset
colnames(tomas_yr_total_ds)
tibble(tomas_yr_total_ds)

## plot and area bar chart of total of all names by year

tomas_yr_total_ds %>% 
  group_by(year) %>% 
  ggplot() +
    geom_area(mapping = aes(x = year, y = yr_total_all_names, fill=name), 
            alpha=0.5) +
    scale_fill_viridis(discrete = TRUE, direction = 1, option = "turbo") +
    scale_x_continuous(breaks = seq(1910, 2020, by = 10)) +
    scale_y_continuous(breaks = seq(0, 1000000 , by = 10000)) +
      theme(axis.text.x = element_text(angle = 90)) +
    labs(title="Name Occurrence over the Years", 
         subtitle = "Excludes Thomas",
         caption="Source: SSA.gov,USA Names 1910 to Sept 2020", 
         x="Year",
         y="Occurrence")




###########################################################################
### Create charts of each name (except Thomas) so data can be seen better.
### Also break down by M/F.  May need addition charts for female only


## show data for total number of names per year
tomas_yr_total_ds2 <- tomas_csv %>%
  group_by(year,name,gender) %>% 
  summarize(yr_total_all_names = sum(number))
#test dataset
colnames(tomas_yr_total_ds2)
tibble(tomas_yr_total_ds2)



## plot and area bar chart of total of all names by year - one plot per name
## but excludes Thomas so a better y-scale is used to see the data.
## viridis -  I like turbo, viridis and cividis thems for 2 items.


tomas_yr_total_ds2 %>% 
  filter(name != "Thomas") %>% 
  group_by(year) %>% 
  ggplot() +
    geom_area(mapping = aes(x = year, y = yr_total_all_names, fill=gender), 
        alpha=0.5 , size=.65, color="white") +
    scale_fill_viridis(discrete = TRUE, direction = -1, option = "turbo") +
    facet_wrap(~name) +
    scale_x_continuous(breaks = seq(1910, 2020, by = 10)) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title="Name Occurrence over the Years", 
         subtitle = "Excludes Thomas",
         caption="Source: SSA.gov,USA Names 1910 to Sept 2020", 
         x="Year",
         y="Occurrence")


## plot and bar chart of total of all names by year - one plot per name
## but excludes Thomas so a better y-scale is used to see the data.
## viridis -  I like turbo, viridis and cividis thems for 2 items.


tomas_yr_total_ds2 %>% 
  filter(name == "Thom") %>% 
  group_by(year) %>% 
  ggplot( aes(x = year, y = yr_total_all_names)) +
    geom_col(alpha=0.5, fill = "#30123B", width = 1) +
    geom_text(aes(label = yr_total_all_names), vjust = -0.5, size = 3.5) +
    ##  scale_x_discrete(name, breaks, labels, limits) +
    scale_x_discrete(limits=c(1942:1962)) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title="Occurrences of Thom over the years", 
         subtitle = "Typo?",
         caption="Source: SSA.gov,USA Names 1910 to Sept 2020", 
         x="Year",
         y="Occurrence")




###########################################################################
### Create charts for female only 
### exclude  Tomas and Thom since no females had that name
## show data for total number of names per year

female_yr_total_ds <- tomas_csv %>%
    filter(gender == "F" ) %>% 
    group_by(year,name) %>% 
    summarize(yr_total_female_names = sum(number))

#test dataset

colnames(female_yr_total_ds)
tibble(female_yr_total_ds)



## plot and area bar chart of total of all names by year - one plot per name
## but excludes Thomas so a better y-scale is used to see the data.
## viridis -  I like turbo, viridis and cividis thems for 2 items.
## viridis function not working so used fill color from last plot at alpha
## set to 1.0.  Then the alpha on the geom_area() line will make it look 
## the same.


female_yr_total_ds %>% 
  group_by(year) %>% 
  ggplot() +
  geom_area(mapping = aes(x = year, y = yr_total_female_names), 
            alpha=0.5, fill = "#7A0403") +
  ##scale_fill_viridis(discrete = TRUE, direction = 1, option = "turbo") +
  facet_wrap(~name) +
  scale_x_continuous(breaks = seq(1910, 2020, by = 10)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Female Name Occurrence over the Years", 
       subtitle = "Excludes Thomas",
       caption="Source: SSA.gov,USA Names 1910 to Sept 2020", 
       x="Year",
       y="Occurrence")




###########################################################################
### Create bar chart for Tomas only for Louisiana only to provide I'm
### not included in dataset.


tomas_filtered_by_name_ds <-  tomas_csv %>%
    filter(name=="Tomas" & state=="LA") %>% 
    group_by(year) %>% 
    summarize(tomas_yr_total = sum(number))

#test dataset
colnames(tomas_filtered_by_name_ds )
tibble(tomas_filtered_by_name_ds )


## plot bar chart 
ggplot(tomas_filtered_by_name_ds, aes(x = year, y = tomas_yr_total)) +
    geom_col(alpha=0.5, fill = "#30123B", width = 1) +
  geom_text(aes(label = tomas_yr_total), vjust = -0.5, size = 3.5) +
##  scale_x_discrete(name, breaks, labels, limits) +
  scale_x_discrete(limits=c(1999:2017)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Occurrences of Tomas in LA", 
       subtitle = "What about me?",
       caption="Source: SSA.gov,USA Names 1910 to Sept 2020", 
       x="Year",
       y="Occurrence")


