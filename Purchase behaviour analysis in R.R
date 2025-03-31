#%%
#Adding needed libraries
library("tidyverse")
library("ggplot2")
library("dplyr")


#Data import
#data <- supermarket_sales...Sheet1

data = read.csv("C:/Users/vertt/Desktop/supermarket_sales - Sheet1.csv")


# Checking the data
head(data)

unique(data$Branch)

mean(data$Unit.price)

summary(data)

#Fixing date to be europen order and plotting sales over time
data <- data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(Date)

total_sales_overtime = data %>% 
    group_by(Date) %>%  summarise(totalsales = sum(Total))

total_sales_overtime = total_sales_overtime %>% arrange(Date)


ggplot(total_sales_overtime, aes(x=Date, y=totalsales, group=1)) +
  geom_line()+
  geom_point()



#Seeing which branch is the most popular

branch_sales = data%>%
  group_by(Branch)%>%
  summarise(tot_sales = sum(Total))

ggplot(branch_sales, aes(x=Branch, y=tot_sales, group=1)) +
  geom_bar(stat="identity")

# plotting each productline by payment method

summary_data <- data %>%
  group_by(Payment, Product.line) %>%
  summarise(Count = n(), .groups = "drop")


ggplot(summary_data, aes(x = Product.line, y = Count, fill = Payment)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Most Bought Product Lines by Payment Method",
       x = "Product Line", 
       y = "Count",
       fill = "Payment Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(summary_data, aes(x = Payment, y = Product.line, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Product Line Popularity by Payment Method",
       x = "Payment Method", 
       y = "Product Line",
       fill = "Count")


#Solving, if there is a difference in buying behaviour by buyer gender

#Visualising gender deviation
gen_div <- data %>%
  group_by(Gender)%>%
  summarise(Count = n(), .groups ="drop")

ggplot(gen_div, aes(x="", y=Count, fill=Gender))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

#Deviation close to 50/50

gender_buys <- data %>%
  group_by(Gender,Product.line) %>%
  summarise(Count=n(), .groups ="drop")

ggplot(gender_buys, aes(x = Gender, y = Count, fill = Product.line)) +
  geom_bar(stat = "identity", ,position = "dodge") +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2")+
  labs(title = "Heatmap of Product Line Popularity by Gender",
       x = "Gender", 
       y = "Count",
       fill = "Product line")+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#It seems that females tend to buy more on fashion related product, males seem to spend more evenly.
#However, this is not the full picture, as some categories might have larger mean buy

gender_mean <- data %>%
  group_by(Gender,Product.line) %>%
  summarise(meanbuy = mean(Total, na.rm=TRUE), .groups ="drop")

ggplot(gender_mean, aes(x = Gender, y = Product.line, fill = meanbuy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Product Line Popularity by Customer type",
       x = "Customer Type", 
       y = "Product Line",
       fill = "Mean")

# Females seem to have larger mean buy in Home & lifestyle rather than in Fashion! 
# Males seem to spend larger sums on health and beauty category, but not as not frequently.

prof <- data%>%
  group_by(Gender,Product.line) %>%
  mutate(totalbuys = mean(Total, na.rm =TRUE) * n(), .groups="drop")%>%
  select(Gender,Product.line, totalbuys)

ggplot(prof, aes(x = Gender, y = Product.line, fill = totalbuys)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Product Line Popularity by Customer type",
       x = "Customer Type", 
       y = "Product Line",
       fill = "Mean")

# Most revenue from females is generated from food and beverages. 
# While men spend more evenly, most revenue from them comes from health and beauty products.

customer_buys <- data %>%
  group_by(Customer.type,Product.line) %>%
  summarise(Count=n(), .groups ="drop")

ggplot(customer_buys, aes(x = Customer.type, y = Product.line, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Product Line Popularity by Customer type",
       x = "Customer Type", 
       y = "Product Line",
       fill = "Count")

customer_mean <- data %>%
  group_by(Customer.type,Product.line) %>%
  summarise(meanbuy = mean(Total, na.rm=TRUE), .groups ="drop")


ggplot(customer_mean, aes(x = Customer.type, y = Product.line, fill = meanbuy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of Product Line Popularity by Customer type",
       x = "Customer Type", 
       y = "Product Line",
       fill = "Mean")

memb_div <- data %>%
group_by(Customer.type, Gender) %>%
summarise(count = n(), .groups ="drop")



# %%


df <- data
# Adding Kmeans clustering to identify the customer segments

library(cluster)
library(factoextra)

print(unique(df$Product.line))
print(unique(df$City))
print(unique(df$Branch))

df <- df %>%
  mutate(Product.line_num = recode(Product.line,
                                   "Sports and travel" = 1,
                                   "Home and lifestyle"  = 2,
                                   "Electronic accessories" = 3,
                                   "Health and beauty"  = 4,
                                   "Fashion accessories" = 5,
                                   "Food and beverages" = 6))

df <- df %>%
  mutate(city_num = recode(City,
                                   "Yangon"  = 1,
                                   "Mandalay"  = 2,
                                   "Naypyitaw" = 3))

df <- df %>%
  mutate(branch_num = recode(Branch,
                                   "A"  = 1,
                                   "B"  = 2,
                                   "C" = 3))

df <- df %>%
  mutate(Mem_num = recode(Customer.type,
                                   "Member"  = 1,
                                   "Normal"  = 2))

df <- df %>%
  mutate(gen_num = recode(Gender,
                                   "Male"  = 1,
                                   "Female"  = 2))

clust_df <- subset(df, select = -c(Invoice.ID, Branch, City, Customer.type, Gender, Product.line, Payment, Date, Time, gross.margin.percentage))

set.seed(42)

clust_scaled <- scale(clust_df)

glimpse(clust_df)

k2 <- kmeans(clust_scaled, centers = 2, nstart = 50)

fviz_cluster(k2, data = clust_scaled)


fviz_nbclust(clust_scaled, kmeans, method = "wss")

fviz_nbclust(clust_scaled, kmeans, method = "silhouette")

gap_stat <- clusGap(clust_scaled, FUN = kmeans, nstart = 50,
                    K.max = 50, B = 50)

fviz_gap_stat(gap_stat)

final <- kmeans(clust_scaled, 2, nstart = 50)
print(final)

fviz_cluster(final, data = clust_scaled)

clust_df$cluster <- final$cluster


ggplot(clust_df, aes(x = Unit.price, y = Total, color = as.factor(cluster))) +
  geom_point(size = 3) +
  labs(title = "Clusters Based on Unit Price and Total",
       color = "Cluster")

ggplot(clust_df, aes(x = as.factor(Product.line_num), y = Total)) +
  geom_boxplot(aes(fill = as.factor(Product.line_num))) +
  facet_wrap(~ cluster) +
  labs(title = "Spending Amount on Each Product Line by Cluster",
       x = "Product Line",
       y = "Total Spending") +
  theme_minimal()

#Visualising gender deviation
memb_div <- clust_df %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),  # Number of observations in each cluster
    mean_Mem_num = mean(Mem_num, na.rm = TRUE),
    mean_gend = mean(gen_num,na.rm = TRUE),  # Mean of Mem_num per cluster
    .groups = "drop"
  )



# It seems the there are two customer types, high and low spending.
# Cluster 1 is smaller, containing more members and females than cluster 2
# Clsuter 2 is larger, having more "normal" customers and those customers tend to be male.
# In general, cluster 1 spends more on each prodcut line but the variation is larger than in cluster 2.
# 
