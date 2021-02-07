###### Load packages #######
lapply(c("tidyverse", "ISLR", "sjPlot", "ggcorrplot", "corrplot", "glmnet", "caret", "ROCR"), require, character.only = TRUE)
install.packages("psych") # for describe function
install.packages("lubridate") # for date function
install.packages("ggrepel") # for graphic visualization
install.packages("reshape2") # for melt function

library("psych")
library("lubridate")
library("ggrepel")
library("reshape2")

##### Data import ######
user_table_data = read_csv("user_table.csv")
home_page_table_data = read_csv("home_page_table.csv")
search_page_table_data = read_csv("search_page_table.csv")
payment_page_table_data = read_csv("payment_page_table.csv")
payment_confirmation_table_data = read_csv("payment_confirmation_table.csv")

##### Check for duplicate records #######
user_table_data[duplicated(user_table_data$user_id),] # no duplicate record
describe(user_table_data) #More detailed summary statistics from the psych package
length(unique(user_table_data$user_id)) #90400 unique users

describe(home_page_table_data$user_id) # 90400 rows of record
length(unique(home_page_table_data$user_id)) # 90400 unique users

# This dataset only contains one-time users. It seems that the e-commerce site is not very effective for retaining users

# The total number number of Desktop vs. Mobile users
user_table_data %>%
  select(device) %>%
  table()

# Since they are only one-time users with unique record, they are either desktop users or mobile users.
prop.table(table(user_table_data$device)) #imbalanced dataset with desktop vs Mobile = 0.67:0.33

# The total number number of Female vs. male users
user_table_data %>%
  select(sex) %>%
  table()

# Proportion of Female and male users
prop.table(table(user_table_data$sex))

# Create a user journey record
funnel <- user_table_data %>%
  left_join(home_page_table_data, by = "user_id") %>%
  left_join(search_page_table_data, by = "user_id") %>%
  left_join(payment_page_table_data, by = "user_id") %>%
  left_join(payment_confirmation_table_data, by = "user_id") 

# Retrieve the data period
range(funnel$date)  # "2015-01-01"  to "2015-04-30"

#rename the funnel table's column
page_name <-c("home_page", "search_page", "payment_page", "payment_confirmation_page")
colnames(funnel)[5:8] <- page_name

##### Funnel analysis by Device ######
# mutate() adds new variables and preserves existing ones;
melt_funnel_table <- funnel %>%
  mutate(home_page = ifelse(!is.na(home_page), TRUE, FALSE),
         search_page = ifelse(!is.na(search_page), TRUE, FALSE),
         payment_page = ifelse(!is.na(payment_page), TRUE, FALSE),
         payment_confirmation_page = ifelse(!is.na(payment_confirmation_page), TRUE, FALSE),
         year = year(date),
         month = month(date),
         day = day(date)
         ) %>%
  gather(page, visited, page_name) %>%  # page & visited are the key while page_name is value
  mutate(page = factor(page, rev(page_name))) # factor() command is used to create and modify factors in R


#### Alternatives with melt function #####
melt_funnel_table <- funnel %>%
  mutate(home_page = ifelse(!is.na(home_page), TRUE, FALSE),
         search_page = ifelse(!is.na(search_page), TRUE, FALSE),
         payment_page = ifelse(!is.na(payment_page), TRUE, FALSE),
         payment_confirmation_page = ifelse(!is.na(payment_confirmation_page), TRUE, FALSE),
         year = year(date),
         month = month(date),
         day = day(date)
  ) %>%
  melt(measure.vars = c("home_page", "search_page", "payment_page", "payment_confirmation_page"),
               variable.name = "page", value.name = "visited")


##### Show what factor() is doing ######
factor_test <-factor(c("home_page", "search_page", "payment_page", "payment_confirmation_page"), rev(page_name))
levels(factor_test)


ggplot(funnel, aes(x = , y =)) + geom_col(aes(fill = supp), width = 0.7)



#### Total visit count per month per device
page_visted_count <- melt_funnel_table %>%
  filter(visited == TRUE & page == "home_page") %>%
  group_by(device, month) %>%
  summarise(page_visit_total_count = n()) # summarise() creates a new data frame
  
### Create a stage table with monthly visit and page visit total count by month ######
page_visted_stage_table <- melt_funnel_table %>%
  filter(visited == TRUE) %>%
  group_by(device, year, month, page) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count, by = c("device", "month"))


##### Create a plot for funnel performance over time per device ######
melt_funnel_table %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, device) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count, by = c("device", "month")) %>%
  mutate(time = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/page_visit_total_count * 100, 1)) %>%
  ggplot(aes(x = time, y = monthly_visits, fill = page)) +
  geom_bar(stat = "identity") +
  geom_label_repel(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5), size = 4, 
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
  facet_wrap(~device) +
  labs(title = "Funnel performance over time by Device")

##### Alternative: Fixed the x-axis label problem #####
melt_funnel_table %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, device) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count, by = c("device", "month")) %>%
  mutate(time = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/page_visit_total_count * 100, 1)) %>%
  ggplot(aes(x = month, y = monthly_visits, fill = page)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(labels = c("Jan","Feb","Mar","Apr"), breaks = 1:4) +
  geom_label(aes(label = paste0(percentage, "%")), size = 3, vjust = -0.5, position = position_dodge(0.9)) +
  facet_wrap(~device) +
  #coord_flip() +
  labs(title = "Funnel performance over time by Device")


### Desktop only plot #####
melt_funnel_table %>%
  filter(visited == TRUE & device == "Desktop") %>%
  group_by(device, year, month, page) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count, by = c("device", "month")) %>%
  mutate(time = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/page_visit_total_count * 100, 1)) %>%
  ggplot(aes(x = month, y = monthly_visits, fill = page)) + # forcats::fct_rev()
  geom_col(position = "dodge") +
  scale_x_continuous(labels = c("Jan","Feb","Mar","Apr"), breaks = 1:4) + # modify the date format on x-Axis 
  geom_text(aes(label = paste0(percentage, "%")), size = 3, vjust = 1.5, position = position_dodge(0.9)) +
  #coord_flip() +
  labs(title = "Funnel performance over time on Desktop")  # vjust = -0.25, hjust = 1.2

# 0 means left-justified
# 1 means right-justified
# Vertical adjustment for geoms that have a position (like points or lines), 
# not a dimension (like bars or areas). Set to 0 to align with the bottom, 
# 0.5 for the middle, and 1 (the default) for the top.

### Mobile only plot #####
melt_funnel_table %>%
  filter(visited == TRUE & device == "Mobile") %>%
  group_by(device, year, month, page) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count, by = c("device", "month")) %>%
  mutate(time = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/page_visit_total_count * 100, 1)) %>%
  ggplot(aes(x = month, y = monthly_visits, fill = page)) + # forcats::fct_rev()
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(percentage, "%")), size = 3, vjust = 1.5, position = position_dodge(0.9))
  #coord_flip() +
  labs(title = "Funnel performance over time on Mobile")
# Insight 1
# Overall, the mobile version of the e-commerce site had better conversion rates than the Desktop version 
# As for mobile version, the Home-to-Search-to-Payment-to-Confirmation page remains the same from Jan to Feb, but the conversion from Home-to-Search page drops dramatically starting from March which in turn lowered the conversion from Search-to-Payment-to-Confirmation page.
# As for Desktop version, the Home-to-Search page remains the same over time, but the search-to-payment conversion dropped significantly. why? Am I attracting the right type of user to my site to begin with?
# The conversion for payment-to-Payment-Confirmation-page dropped significantly. Is there a bug or UI issue in the checkout process that's preventing users from purchasing? Credit card or shipping info issue? Hotjar to know why?




#### Funnel analysis by Gender ######


#### Total visit count per month per device
page_visted_count_group_by_gender <- melt_funnel_table %>%
  filter(visited == TRUE & page == "home_page") %>%
  group_by(sex, month) %>%
  summarise(page_visit_total_count = n()) # summarise() creates a new data frame

### Create a stage table with monthly visit and page visit total count by month ######
page_visted_stage_table_group_by_gender <- melt_funnel_table %>%
  filter(visited == TRUE) %>%
  group_by(sex, year, month, page) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count_group_by_gender, by = c("sex", "month"))



##### Create a plot for funnel performance over time per device ######
melt_funnel_table %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, sex) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visted_count_group_by_gender, by = c("sex", "month")) %>%
  mutate(time = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/page_visit_total_count * 100, 1)) %>%
  ggplot(aes(x = time, y = monthly_visits, fill = page)) +
  geom_bar(stat = "identity") +
  geom_label_repel(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5), size = 4, 
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + 
  facet_wrap(~sex) +
  labs(title = "Funnel performance over time by Gender")


# Insight 2
# There is no significant difference between Female and Male customers regarding the user journey. 
# So, we can rule out the gender related problem







