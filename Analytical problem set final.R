##Loading relevant packages:
library(tidyverse)
library(readxl)
install.packages("writexl")
library(writexl)
library(moments)
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)

##Importing excel files in R:
customers <- read_excel("~/Desktop/Foundation to BA/customers.xlsx")
sales <- read_excel("~/Desktop/Foundation to BA/sales.xlsx", sheet = "sales-data")
stores <- read_csv("~/Desktop/Foundation to BA/stores.csv")


#2 INSPECTION AND CLEANING:

customers$customer.id<- as.factor(customers$customer.id)

#------------

table(customers$customer.state)

customers <- customers %>%
  mutate(customer.state = ifelse(customer.state %in% c("Conn.", "Connecticut"), "CT",
                                 ifelse(customer.state %in% c("Mass", "Mass.", "Massachusets", "Massachusetts"), "MA", 
                                        customer.state))) %>% 
  mutate(customer.state = as.factor(customer.state))

table(customers$customer.state)

#------------

table(customers$birthday.month)

customers <- customers %>% 
  mutate(birthday.month = ifelse(birthday.month %in% c("Apr.", "April"), "4",
                                 ifelse(birthday.month %in% c("Feb.", "February"), "2",
                                        ifelse(birthday.month %in% c("Oct", "October"), "10",
                                               ifelse(birthday.month %in% c("Mar", "March"), "3",
                                                      ifelse(birthday.month == "Nov.", "11",
                                                             ifelse(birthday.month == "July", "7", birthday.month))))))) %>% 
  filter(birthday.month != "0" ) %>% 
  mutate(birthday.month = as.factor(birthday.month))

table(customers$birthday.month)

#------------

table(customers$selection)
table(customers$in.store.exp)
table(customers$years.as.member)


table(customers$age)
boxplot(customers$age)
##there are some values above the upper bound which will be considered outliers:
summary(customers$age)

# Define IQR (Interquartile Range) method 
Q1_age <- quantile(customers$age, 0.25)  # 1st Quartile
Q3_age <- quantile(customers$age, 0.75)  # 3rd Quartile

# Interquartile Range
IQR <- Q3_age - Q1_age 

# Outlier thresholds
lower_bound <- Q1_age - 1.5 * IQR
upper_bound <- Q3_age + 1.5 * IQR

# Identify outliers
outliers <- customers$age[customers$age < lower_bound | customers$age > upper_bound]
print(outliers)

print(customers$age)

colSums(is.na(customers))
customers <- customers[customers$age >= lower_bound & customers$age <= upper_bound, ]
print(customers$age)

#3 DATA MANIPULATION AND WRANGLING:
names(sales)

summary(sales$customer.id)
colSums(is.na(sales))

sales_table <- sales %>% 
  group_by(customer.id) %>% 
  summarize(total.items = sum(qty),
            avg.price = mean(sale.amount/qty))

customers_purchases <- merge(customers, sales_table, by = "customer.id", all.x = TRUE)

write_xlsx(customers, path = "~/Desktop/Foundation to BA/customers_purchases.xlsx")

#4 SUMMARY STATISTICS AND VISUALIZATION:

install.packages("moments")
library(moments)
summary(sales$sale.amount)
#median = 56.20
#mean = 60.60
sd(sales$sale.amount)
#36.262
skewness(sales$sale.amount)
#1.005876

##Distribution of Sale Amount for All Sales Records
install.packages("ggplot2")
library(ggplot2)
ggplot(sales, aes(y = sale.amount)) +
  geom_boxplot(fill = "#fce1e4", color = "#ffafcc", outlier.color ="#a2d2ff", outlier.shape = 4 ) +
  labs(title = "Distribution of Sale Amount for All Sales Records", y = "Sale Amount") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +   coord_flip()

##Distribution of Sale Amount for Each Category
ggplot(sales, aes(category,  sale.amount)) +
  geom_boxplot(fill = "#fce1e4", color = "#ffafcc", outlier.color = "#a2d2ff", outlier.shape = 4 ) +
  labs(title = "Distribution of Sale Amount for Each Category", y = "Sale Amount") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

##Blended Gross Margin for each Category:
blendedgross <- sales %>%
  group_by(category) %>%
  summarize(
    total.sales = sum(sale.amount, na.rm = TRUE),
    total.cost = sum(ext.cost, na.rm = TRUE),
    blended.gross = ifelse(
      total.sales != 0, 
      (total.sales - total.cost) / total.sales, 
      NA
    ),
    .groups = "drop" # Avoid unnecessary grouping after summarization
  )

##Detecting Outliers using Boxplot Method and Handling them:

outliers_salesamount <- boxplot(sales$sale.amount)$out; outliers_salesamount

##There are only 21 outlier values in the data meaning their frequency is really low,
##The reason there are outliers might be because wrong data entries or their might be some sales which are high value due to high volume purchases or expensive products.
##I would recommend replacing outlier values with NA values and dropping them.


#5 HYPOTHESIS TESTING:

sales<- sales %>% 
  mutate(store = as.factor(store),
         category = as.factor(category))

AvgGMper <- mean(100*((sum(sales$sale.amount) - sum(sales$ext.cost))/sum(sales$sale.amount)))
AvgGMdol <- mean(sum(sales$sale.amount) - sum(sales$ext.cost))

GM_Stores<- sales %>% 
  group_by(store) %>% 
  summarise(GMdollars = (sum(sale.amount) - sum(ext.cost)),
            AvgGMdol = AvgGMdol,
            GMpercentage = (GMdollars / sum(sale.amount)) * 100,
            AvgGMper = AvgGMper)

GM_Category<- sales %>% 
  group_by(category) %>% 
  summarise(GMdollars = (sum(sale.amount) - sum(ext.cost)),
            AvgGMdol = AvgGMdol,
            GMpercentage = (GMdollars / sum(sale.amount)) * 100,
            AvgGMper = AvgGMper)

sales <- sales %>%
  mutate(month = format(sale.date, "%m"))

gifts_ls <- sales %>%
  filter(category == "Gifts & Lifestyle") %>% 
  mutate(holiday_season = ifelse(month %in% c("11", "12"), "Holiday", "Non-Holiday"))

holidaygm <- gifts_ls %>%
  filter(holiday_season == "Holiday")%>%
  select(gross.margin)

non_holidaygm <- gifts_ls %>%
  filter(holiday_season == "Non-Holiday")%>%
  select(gross.margin)

t_test_result <- t.test(holidaygm, non_holidaygm, alternative = "less", var.equal = TRUE)
print(t_test_result)

p_value <- t_test_result$p.value

alpha <- 0.05  # Significance level
if (p_value < alpha) {
  print("Reject the null hypothesis: Gross margin during the holiday season is significantly less than during the non-holiday season.")
} else {
  print("Fail to reject the null hypothesis: No significant difference in gross margin between the holiday and non-holiday seasons.")
}


#6. REGRESSION ANALYSIS:

sales <- sales %>%
  mutate(price.category = as.factor(price.category),
         loyalty.member = as.factor(loyalty.member))


model <- lm(gross.margin ~ category + qty + I(qty^2) + 
                          sale.amount + I(sale.amount^2) +
                          price.category + loyalty.member + store + 
                          category:price.category + store:category + 
                          store:price.category, data = sales)


summary(model)

