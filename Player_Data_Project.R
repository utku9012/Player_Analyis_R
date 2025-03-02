#Installing packages
install.packages("corrplot")
install.packages("dplyr")
install.packages("skimr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("treemap")
install.packages("installr")
install.packages("plotly")
install.packages("factoextra")

#Installing libraries
library(readxl)
library(corrplot)
library(dplyr)
library(skimr)
library(ggplot2)
library(plotrix)
library(treemap)
library(installr)
library(plotly)
library(factoextra)

#This data set is created by us
#Importing the data set
data <- read_excel("player_data.xlsx")

View(data)

# showing how elements are nested within each other.It also shows the names and types of the components
str(data)

#summary statistics
summary(data)

#more detailed version of summary
skim(data)

#number of columns and rows
nrow(data)
ncol(data)

#names of the columns
colnames(data)

#checking to see if there are any missing values in data 
any(is.na(data))

View(is.na(data))

#to view which indexes have NA or missing values
View(data[!complete.cases(data),])

#rounding up the Minutes_Per_Match column and making a new column
data$Minutes_Per_Match_Rounded <- round(data$Minutes_Per_Match)

#changing the NA values to 0 in Minutes_Per_Match column
data$Minutes_Per_Match_Rounded <-replace(data$Minutes_Per_Match_Rounded, is.na(data$Minutes_Per_Match_Rounded), 0)

#removing the minutes_per_Match column
data$Minutes_Per_Match <- NULL;

#summary statistics after cleaning the NA values 
summary(data)

#2024-2025 Trendyol Super Lig Average Age,Height,Weight
mean(data$Age, na.rm = TRUE)
mean(data$`Height(cm)`, na.rm = TRUE)
mean(data$`Weight(kg)`, na.rm = TRUE)


#Top 20 Valuable Players
top_players <- data %>%
  arrange(desc(Market_Value)) %>%
  head(20)

print(top_players)

ggplotly(
  ggplot(top_players, aes(x = reorder(Name, Market_Value), y = Market_Value, fill = Team)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 10 Most Valuable Players",
         x = "",
         y = "Market Value (Million Euro)")
)


#Which positions are most valuable?
position_value <- data %>%
  group_by(Position) %>%
  summarise(Total_Value = sum(Market_Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Value))

print(position_value)

#Total market value of the Turkish Super League
total_market_value <- sum(data$Market_Value, na.rm = TRUE)

cat("Total market value of the Turkish Super League :", total_market_value, "milion euros\n")

#Total market value of each team in the League
team_market_value <- data %>%
  group_by(Team) %>%              
  summarise(Total_Value = sum(Market_Value, na.rm = TRUE))

p <- ggplot(team_market_value, aes(x = reorder(Team, -Total_Value), y = Total_Value, text = paste("Value:", Total_Value))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Total Market Value of Teams",
       x = "Teams",
       y = "Market Value (Million Euro)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p, tooltip = "text")

#To count how many different nationalities there are in the League
length(unique(data$Nationality))

#Calculates how many players there are of each nationality
nationality_table <- table(data$Nationality)

nationality_table

#Converting to data frame to work in Treemap
nation_data <- data.frame(
  nation = names(nationality_table),
  count = as.numeric(nationality_table)
)

#Nation distribution treemap  
treemap(nation_data,
        index = "nation",
        vSize = "count",
        title = "Distribution of Players by Nation",
        palette = "Set2",
        border.col = "white")


#Preferred foot distribution
preferred_foot <-table(data$Preferred_Foot)

plot_ly(
  x = names(preferred_foot),
  y = as.numeric(preferred_foot),
  type = 'bar',
  text = as.numeric(preferred_foot), 
  marker = list(color = c("skyblue", "orange", "green"))
) %>%
  layout(
    title = "Preferred Foot of Players",
    xaxis = list(title = "Preferred Foot"),
    yaxis = list(title = "Count")
  )

percentages_foot <- round(100 * preferred_foot / sum(preferred_foot), 1)

labels_foot <- paste(names(percentages_foot), percentages_foot, "%", sep = " ")

pie(percentages_foot,
    main = "Distribution of Preferred Foot in Percentages",
    labels = labels_foot,
    col = rainbow(length(labels_foot)),
)

#position distribution
position_table <-table(data$Position) 

percentages_position <- round(100 * position_table / sum(position_table), 1)

labels_position <- paste(names(position_table), percentages_position, "%", sep = " ")

pie(position_table,
    main = "Distribution of Positions in Percentages",
    labels = labels_position,
    col = rainbow(length(labels_position)),
  )

#creating levels

position_grouped <- factor(c(data$Position))

levels(position_grouped)

#Correlation analysis

correlation_data=filter(data,Rating!='0')

cor(correlation_data$Rating, correlation_data$Market_Value, method="pearson")

corrmat_pearson <- cor(correlation_data[, c(4:6,10:30)],correlation_data[, c(4:6,10:30)], method="pearson")
View(corrmat_pearson)

corrplot(corrmat_pearson, method="circle")

#Clustering

#grouping by the positions
goalkeeper=filter(correlation_data,Position == 'Goalkeeper')
defender=filter(correlation_data,Position == 'Defender')
midfielder=filter(correlation_data,Position == 'Midfielder')
forward=filter(correlation_data,Position == 'Forward')

##########################################################################################################

#Goalkeeper K-Means clustering by performance matrixes
#removing the unnecessary columns
goalkeeper_removed <- goalkeeper[, -c(1:11,12:14,15:18,20:22,25:30)]

goalkeeper_removed_scaled <- scale(goalkeeper_removed)

View(goalkeeper_removed_scaled)

#elbow method
fviz_nbclust(goalkeeper_removed_scaled, kmeans, method = "wss", k.max=20)

#K-Means Clustering
set.seed(111)
km.out <- kmeans(goalkeeper_removed_scaled, centers =4, nstart=100)
print(km.out)

#visualizing the clustering algorithm
km.clusters <- km.out$cluster
fviz_cluster(list(data=goalkeeper_removed_scaled, cluster = km.clusters))

#dendrogram
distance_matrix_goalkeeper <- dist(goalkeeper_removed_scaled, method = "euclidean")
hc_goalkeeper <- hclust(distance_matrix_goalkeeper, method = "ward.D2")
plot(hc_goalkeeper, main = "Dendrogram", xlab = "Goalkeepers", sub = "", cex = 0.6)

##########################################################################################################

#defender K-Means clustering by performance matrixes 
#removing the unnecessary columns
defender_removed <-defender[,-c(1:20,23:30)]

defender_removed_scaled <- scale(defender_removed)

View(defender_removed_scaled)

#elbow method
fviz_nbclust(defender_removed_scaled, kmeans, method = "wss", k.max=20)

#K-Means Clustering
set.seed(111)
km.out3 <- kmeans(defender_removed_scaled, centers =4, nstart=100)
print(km.out3)

#visualizing the clustering algorithm
km.clusters <- km.out3$cluster
fviz_cluster(list(data=defender_removed_scaled, cluster = km.clusters))

#dendrogram
distance_matrix_defender <- dist(defender_removed_scaled, method = "euclidean")
hc_defender <- hclust(distance_matrix_defender, method = "ward.D2")
plot(hc_defender, main = "Dendrogram", xlab = "Defenders", sub = "", cex = 0.6)

##########################################################################################################
#calculating the difference between Goals_Scored and Expected_Goals(XG)
midfielder$Goals_Difference <- midfielder$Goals_Scored - midfielder$`Expected_Goals(xG)`
midfielder$Assist_Difference <- midfielder$Assists - midfielder$`Expected_Assists(xA)`

#Midfielder K-Means clustering by performance matrixes
#removing the unnecessary columns
midfielder_removed <-midfielder[,-c(1:18,21:30)]

midfielder_removed_scaled <- scale(midfielder_removed)

View(midfielder_removed_scaled)

#elbow method
fviz_nbclust(midfielder_removed_scaled, kmeans, method = "wss", k.max=20)

#K-Means Clustering
set.seed(111)
km.out4 <- kmeans(midfielder_removed_scaled, centers =3, nstart=100)
print(km.out4)

#visualizing the clustering algorithm
km.clusters <- km.out4$cluster
fviz_cluster(list(data=midfielder_removed_scaled, cluster = km.clusters))

#dendrogram
distance_matrix_midfielder <- dist(midfielder_removed_scaled, method = "euclidean")
hc_midfielder <- hclust(distance_matrix_midfielder, method = "ward.D2")
plot(hc_midfielder, main = "Dendrogram", xlab = "Midfielders", sub = "", cex = 0.6)


##########################################################################################################

#calculating the difference between Goals_Scored and Expected_Goals(XG)
forward$Goals_Difference <- forward$Goals_Scored - forward$`Expected_Goals(xG)`
forward$Assist_Difference <- forward$Assists - forward$`Expected_Assists(xA)`

#Midfielder K-Means clustering by performance matrixes
#removing the unnecessary columns
forward_removed <-forward[,-c(1:30)]

forward_removed_scaled <- scale(forward_removed)

View(forward_removed_scaled)

#elbow method
fviz_nbclust(forward_removed_scaled, kmeans, method = "wss", k.max=20)

#K-Means Clustering
set.seed(111)
km.out5 <- kmeans(forward_removed_scaled, centers =5, nstart=100)
print(km.out5)

#visualizing the clustering algorithm
km.clusters <- km.out5$cluster
fviz_cluster(list(data=forward_removed_scaled, cluster = km.clusters))

#dendrogram
distance_matrix_forward <- dist(forward_removed_scaled, method = "euclidean")
hc_forward <- hclust(distance_matrix_forward, method = "ward.D2")
plot(hc_forward, main = "Dendrogram", xlab = "Forwards", sub = "", cex = 0.6)

##########################################################################################################
#checking the correlation between fouls and cards 
corrmat_pearson_card <- cor(correlation_data[, c(26:29)],correlation_data[, c(26:29)], method="pearson")
View(corrmat_pearson_card)

corrplot(corrmat_pearson_card, method="circle")
