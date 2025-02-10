setwd("C:/Users/mpkhd/Documents/Uni Projects/Project1/wine+quality")

winequality.red <- read.csv("~/Uni Projects/Project1/wine+quality/winequality-red.csv", sep=";")
winequality.white <- read.csv("~/Uni Projects/Project1/wine+quality/winequality-white.csv", sep=";")


df1 <- data.frame(winequality.red,type = ('red'))
df2 <- data.frame(winequality.white,type = ('white'))
df0 <- rbind(df1,df2)

#-------------Loading Libraries----------------------
library(dplyr)
library(rsample)
library(ggplot2)
library(robustbase)
library(reshape2)
library(mdatools)
library(FactoMineR)

#--------------Data Preprocessing--------------------
#Checking for Duplicates
sum(duplicated(df0))
# obtaining de duplicated dataframe

df <- distinct(df0,.keep_all=T)
#write.csv(df, file = 'De-duplicated data', row.names = F)
sum(duplicated(df))
df <- df %>%
  mutate(ranked_category = case_when(
    quality < 5  ~ "Poor",
    quality >= 5 & quality <= 6  ~ "Normal",
    quality >= 7  ~ "Excellent",
    
  ))
df$quality_level <- as.factor(df$quality)
df$quality <- as.factor(df$ranked_category)
df$type <- as.factor(df$type)
df <- df[,-14]

# check for missing values
sum(is.na(df))

# summary statistcs
summary(df)

# number of observations from white wine and red wine
table(df$type)

#------------------------------Train Test Set Splitting---------------------------------
df <- df %>%
  mutate(strata_var = interaction(df$type, df$quality, sep = "_"))
df$strata_var <- as.factor(df$strata_var)

set.seed(42)
split <- rsample::initial_split(df, prop = 0.8, strata = strata_var )

training_set <- rsample::training(split)
testing_set <- rsample::testing(split)

#--------------------- creating the test and train data set files----------------------
write.csv(df, file = 'De_Duplicated_Full_Corrected', row.names = F)
write.csv(training_set, file = 'Train Set', row.names = F)
write.csv(testing_set, file = 'Test Set', row.names = F)

#----------------------Importing the train test set---------------------------
train <- read.csv("C:/Users/mpkhd/Documents/Uni Projects/Project1/wine+quality/Train Set", sep=",")
test <- read.csv("C:/Users/mpkhd/Documents/Uni Projects/Project1/wine+quality/Test Set", sep=",")
train_set <- data.frame(train)
test_set <- data.frame(test)

train_set$quality <- as.factor(train_set$quality)
train_set$type <- as.factor(train_set$type)
train_set <- train_set[,c(-15)]

test_set$quality <- as.factor(test_set$quality)
test_set$type <- as.factor(test_set$type)
test_set <- test_set[,c(-15)]


#------------------------------------EDA---------------------------------------
#Trivariate Analysis Boxplots
#total.sulphur.dioxide
ggplot(train_set, aes(x = quality, y = total.sulfur.dioxide, fill = type)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2,alpha = 0.5) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(
    title = "Total Sulfur Dioxide by Ranking",
    x = "Ranking",
    y = "Total Sulfur Dioxide (mg/l)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold") 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))
summary_TSO2 <- train_set %>%
  group_by(quality, type) %>%
  summarise(
    Min = min(total.sulfur.dioxide, na.rm = TRUE),
    Q1 = quantile(total.sulfur.dioxide, 0.25, na.rm = TRUE),
    Median = median(total.sulfur.dioxide, na.rm = TRUE),
    Q3 = quantile(total.sulfur.dioxide, 0.75, na.rm = TRUE),
    Max = max(total.sulfur.dioxide, na.rm = TRUE),
    Mean = mean(total.sulfur.dioxide, na.rm = TRUE)
  )
print(summary_TSO2)

#-----free.sulphur.dioxide
ggplot(train_set, aes(x = quality, y = free.sulfur.dioxide, fill = type)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2,alpha = 0.5) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(
    title = "Free Sulfur Dioxide by Ranking",
    x = "Ranking",
    y = "Free Sulfur Dioxide (mg/l)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold") 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))
summary_FSO2 <- train_set %>%
  group_by(quality, type) %>%
  summarise(
    Min = min(free.sulfur.dioxide, na.rm = TRUE),
    Q1 = quantile(free.sulfur.dioxide, 0.25, na.rm = TRUE),
    Median = median(free.sulfur.dioxide, na.rm = TRUE),
    Q3 = quantile(free.sulfur.dioxide, 0.75, na.rm = TRUE),
    Max = max(free.sulfur.dioxide, na.rm = TRUE),
    Mean = mean(free.sulfur.dioxide, na.rm = TRUE),
    )
print(summary_FSO2)

#-----sulphates
ggplot(train_set, aes(x = quality, y = sulphates, fill = type)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2,alpha = 0.5) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(
    title = "Sulphates by Ranking",
    x = "Ranking",
    y = "Sulphates (mg/l)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold") 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))
summary_Sulphate <- train_set %>%
  group_by(quality, type) %>%
  summarise(
    Min = min(sulphates, na.rm = TRUE),
    Q1 = quantile(sulphates, 0.25, na.rm = TRUE),
    Median = median(sulphates, na.rm = TRUE),
    Q3 = quantile(sulphates, 0.75, na.rm = TRUE),
    Max = max(sulphates, na.rm = TRUE),
    Mean = mean(sulphates, na.rm = TRUE)
  )
print(summary_Sulphate)

#-----density
ggplot(train_set, aes(x = quality, y = density, fill = type)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2,alpha = 0.5) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(
    title = "Density by Ranking",
    x = "Ranking",
    y = "Density (g/ml)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold") 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))
summary_density <- train_set %>%
  group_by(quality, type) %>%
  summarise(
    Min = min(density, na.rm = TRUE),
    Q1 = quantile(density, 0.25, na.rm = TRUE),
    Median = median(density, na.rm = TRUE),
    Q3 = quantile(density, 0.75, na.rm = TRUE),
    Max = max(density, na.rm = TRUE),
    Mean = mean(density, na.rm = TRUE)
  )
print(summary_density)

#bivariate density
ggplot(train_set, aes(x = ranked_category, y = density, fill = ranked_category)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(
    title = "Density by Ranking",
    x = "Ranking",
    y = "Density (g/ml)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold") 
  )

#------------------Anupama----------------------------
# Boxplot for Citric Acid vs Quality
ggplot(train_set, aes(x = quality, y = citric.acid, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Citric Acid vs Quality - Red and White Wine", x = "Quality", y = "Citric Acid")

# Boxplot for Fixed Acidity vs Quality
ggplot(train_set, aes(x = quality, y = fixed.acidity, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Fixed Acidity vs Quality - Red and White Wine", x = "Quality", y = "Fixed Acidity")

# Boxplot for Volatile Acidity vs Quality
ggplot(train_set, aes(x = quality, y = volatile.acidity, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Volatile Acidity vs Quality - Red and White Wine", x = "Quality", y = "Volatile Acidity")

# Boxplot for for pH VS quality
ggplot(data, aes(x = quality, y = pH, fill = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Boxplot of pH vs Quality", x = "Quality", y = "pH")


#-------------------------------------Scatter Plots----------------------------------
#free.sulphur.dioxide vs total.sulphur.dioxide
ggplot(data = train_set,mapping = aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)) + 
  geom_point(color = '#0041C2',alpha = 0.5) + 
  geom_smooth(method = lm, color = 'red', se = F) + 
  labs(
    title = "Scatterplot between Free Sulphur Dioxide and Total Sulphur Dioxide ",
    x = "Free Sulphur Dioxide (mg/l)", y = "Total Sulphur Dioxide (mg/l)") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal(base_size = 14)

#density vs chlorides
ggplot(data = train_set,mapping = aes(x = density, y = chlorides)) + 
  geom_point(color = '#0041C2',alpha = 0.5) + 
  geom_smooth(method = lm, color = 'red', se = F) + 
  labs(
    title = "Scatterplot between Density and chlorides ",
    x = "density (g/dm3)", y = "chlorides (mg/l)") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal(base_size = 14)

#-----------------Anupama-------------------
# Scatterplot for Fixed Acidity, Alcohol, and Type
ggplot(train_set, aes(x = alcohol, y = fixed.acidity, color = type)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Fixed Acidity, Alcohol, and Type", x = "Alcohol", y = "Fixed Acidity")

# Scatterplot for Citric Acid, Alcohol, and Type
ggplot(train_set, aes(x = alcohol, y = citric.acid, color = type)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Citric Acid, Alcohol, and Type", x = "Alcohol", y = "Citric Acid")

# Scatterplot for Volatile Acidity, Alcohol, and Type
ggplot(train_set, aes(x = alcohol, y = volatile.acidity, color = type)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("red" = "#D32F2F", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Volatile Acidity, Alcohol, and Type", x = "Alcohol", y = "Volatile Acidity")


#----------------------------------------Histogram---------------------------------------------------
#Comparing distributions of variables by type of wine 
#free.sulphur.dioixde
ggplot(train_set, aes(x = free.sulfur.dioxide, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40,color = 'black') + 
  #scale_fill_brewer(palette = "Pastel1") + 
  labs(
    title = "Free Sulfur Dioxide by wine"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))

#total.sulphur.dioxide
ggplot(train_set, aes(x = total.sulfur.dioxide, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40,color = 'black') + 
  labs(
    title = "Total Sulfur Dioxide by wine"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))

#density
ggplot(train_set, aes(x = density, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40,color = 'black') + 
  labs(
    title = "Density by wine"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))

#sulphates
ggplot(train_set, aes(x = sulphates, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40,color = 'black') + 
  labs(
    title = "Sulphates by wine"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    #legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) +
  scale_fill_manual(values = c(red = 'red', white = 'yellow'))

#---------------Anupama------------------------------
# Histogram for Volatile Acidity by Wine Type
ggplot(train_set, aes(x = volatile.acidity, fill = type)) +
  geom_histogram(binwidth = 0.01,color = "black", alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("red" = "pink", "white" ="#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Volatile Acidity with Red and White Wines", x = "Volatile Acidity", y = "Frequency")

# Histogram for Citric Acid
ggplot(train_set, aes(x = citric.acid, fill = type)) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("red" = "pink", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Histogram for Citric Acid with Red and White Wines", x = "Citric Acid", y = "Frequency")

#Histogram for Fixed Acidity
ggplot(train_set, aes(x = fixed.acidity, fill = type)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("red" = "pink", "white" = "#FFEB3B")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F0F0F0", color = NA), plot.title = element_text(hjust = 0.5))+
  labs(title = "Histogram for Fixed Acidity with Red and White Wines", x ="Fixed Acidity", y = "Frequency")

## Histogram for pH
ggplot(train_set, aes(x = pH, fill = type)) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("red" = "#C2185B", "white" = "#FFF9C4")) +  # Dark pink & light yellow
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F0F0F0", color = NA),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Histogram for pH - Red and White Wine", x = "pH", y = "Frequency") +
  scale_fill_manual(values = c("white" = "#FFF9C4", "red" = "#C2185B"))  # Ensuring red is on top


#---------------------------Heat Map--------------------------
# checking for relation with quality_level(categorical ordinal) so we add spearman
corr_mat_q <- round(cor(train_set[,c(1:11, 14)],method = 'spearman'),2)
melted_corr_mat_q <- melt(corr_mat_q)
head(melted_corr_mat_q)
ggplot(data = melted_corr_mat_q, aes(x=Var1, y=Var2, 
                                     fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4) +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  labs(title = "Spearman Correlation Matrix ")


#---------------------------PLS-----------------------
#Processing the data to apply PLS
Xt <- train_set[,c(1:11)]
Xt$type <- as.factor(train_set$type)
Xt$type <- ifelse(Xt$type == 'white',0,1)

Yt <- train_set[,c(12)]
Yt <- factor(Yt, levels = c("Excellent", "Normal", "Poor"), ordered = TRUE)
Yt <- as.numeric(Yt)

model <- pls(Xt, Yt, scale = TRUE, cv = 1, info = "Wine Quality Prediction")
summary(model)
plot(model)
plotXScores(model,show.labels =F)
plotXYLoadings(model,show.labels = T)
vip<- vipscores(model, ncomp = 2)
