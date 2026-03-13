# Flower classification

# Settings

install.packages("broom")
install.packages("showtext")
install.packages("vtreat")
install.packages("ranger")
install.packages("pscl")

library(dplyr)
library(purrr)
library(broom)
library(tidyverse)
library(ggplot2)
library(showtext)
library(vtreat)
library(ranger)
library(nnet)
library(pscl)


set.seed(123)

font_add("Cascadia", "C:/Windows/Fonts/CascadiaCode.ttf")
showtext_auto()

font_import(pattern = "Cascadia", prompt = FALSE)
loadfonts(device = "win")
fonts()

setwd("C:/Users/jutrz/OneDrive/Pulpit/Kaggle")
table <- readr::read_csv("flower-classification.csv")
head(table)
dt <- table
names(dt) <- gsub("[()']", "", names(dt))
names(dt) <- gsub(" ", "_", names(dt))     

dt$Flower_Type <- as.factor(dt$Flower_Type)
dt <- dt %>% select(-ID)
names(dt)[names(dt) == "Number_of_Petals/Sepals"] <- "Number_of_Petals_Sepals"
head(dt)

dc <- dt
# Visualize differences in flower features between species

dt_l <- tidyr::pivot_longer(dt, 
                            cols = -Flower_Type,
                            names_to = "variable",
                            values_to = "value")

ggplot(dt_l, aes(x = Flower_Type, y = value, fill = Flower_Type)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    text = element_text(family = "Cascadia"),
  )
# All features show visible variation across flower types, except Avg_R, which barely changes.

# Anova test

dt %>%
  select(where(is.numeric)) %>%
  map_df(~ tidy(aov(.x ~ dt$Flower_Type)), .id = "variable") %>%
  filter(term == "dt$Flower_Type") %>%
  select(variable, p.value)

# The p-value for Avg_R is greater than 0.05, so this variable is not statistically significant.

dt <- dt %>% select(-Avg_R)
head(dt, 1)

dt_l2 <- tidyr::pivot_longer(dt, 
                            cols = -Flower_Type,
                            names_to = "variable",
                            values_to = "value")

ggplot(dt_l2, aes(x = Flower_Type, y = value, fill = Flower_Type)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    text = element_text(family = "Cascadia", size = 16),
  )

# Cross validation

(nRows <- nrow(dt))
splitplan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# I create 3-fold cross validation, because data set is small

str(splitplan)

# Random forest model

(outcome <- "Flower_Type")

(vars <- c("Sepal_Length_cm", "Petal_Length_cm","Sepal_Width_cm", "Petal_Width_cm",
           "Flower_Radius_cm", "Number_of_Petals_Sepals", "Avg_G", "Avg_B"))

(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

names(dt)

results <- list()

for(i in 1:3) {
  train_idx <- splitplan[[i]]$train
  test_idx  <- splitplan[[i]]$app
  
  train_set <- dt[train_idx, ]
  test_set  <- dt[test_idx, ]
  
  flowers_model_rf <- ranger(
    formula = fmla,
    data = train_set,
    num.trees = 500,
    respect.unordered.factors = "order",
  )
  
  preds <- predict(flowers_model_rf, test_set)$predictions
  
  results[[i]] <- data.frame(
    truth = test_set[[outcome]],
    pred = preds
  )
}

cv_results <- do.call(rbind, results)

# Model evaluation

# truth vs prediction

cv_results

# confusion matrix

cm <- table(cv_results$truth, cv_results$pred)
cm_df <- as.data.frame(cm)

ggplot(cm_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(x = "Prediction", y = "True class")+
  theme(
    axis.text.x = element_blank(),
    text = element_text(family = "Cascadia", size = 15))+
  scale_fill_gradient(low = "white", high = "cyan3")

# The model got almost everything right, which means it probably overfitted.
# With so little data, it learned the training examples too well instead of general patterns.

# Logistic regression

gp <- runif(nRows)

dt_train <- dt[gp < 0.7,]
dt_test <- dt[gp >= 0.3,]

head(dt)

log_model <- multinom(Flower_Type~., dt_train)

pred <- predict(log_model, dt_test)

table(dt_test$Flower_Type, pred)

#   This model is probably also overfitting, so i will reduce the number of variables

ggplot(dt_l2, aes(x = Flower_Type, y = value, fill = Flower_Type)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    text = element_text(family = "Cascadia", size = 16),
  )

# The biggest differences between the classes I see are in the variables: Sepal_Width_cm and Petal_Length_cm

# Anova test

dt %>%
  select(where(is.numeric)) %>%
  map_df(~ tidy(aov(.x ~ dt$Flower_Type)), .id = "variable") %>%
  filter(term == "dt$Flower_Type") %>%
  select(variable, p.value)

# The smallest p-value have: Avg_G and Avg_B, but i prefer make model who doesn't rely on colors first

log_model2 <- multinom(Flower_Type ~ Sepal_Width_cm + Petal_Length_cm, dt_train)

pred2 <- predict(log_model2, dt_test)

table(dt_test$Flower_Type, pred2)

ggplot(dt, aes(Sepal_Width_cm, Petal_Length_cm, color = Flower_Type)) +
  geom_point(size = 3) +
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    text = element_text(family = "Cascadia", size = 16),
  )

# The model reached 100% accuracy because the two features separate the classes very clearly.
# This result reflects the structure of the data, not exceptional model performance.

# Logistic regression model rely on colors

# I will include AVG_R here, even though AVG_R was not significant before, to add more variation and make the color-based model less straightforward.

head(dc)

gp2 <- runif(nrow(dc))

dc_train <- dc[gp2 < 0.7,]
dc_test <- dc[gp2 >= 0.3,]

log_model3 <- multinom(Flower_Type ~ Avg_R + Avg_G + Avg_B, dc_train)

pred3 <- predict(log_model3, dc_test)

table(dc_test$Flower_Type, pred3)

# This model is underfitted and fails to detect Hibiscus, so I will remove Avg_R and try again.

log_model4 <- multinom(Flower_Type ~ Avg_G + Avg_B, dc_train)

pred4 <- predict(log_model4, dc_test)

table(dc_test$Flower_Type, pred4)

# The model performs better now, but Hibiscus and Rose are still hard to separate using color features only, so I add Petal_Width_cm

log_model5 <- multinom(Flower_Type ~ Avg_G + Avg_B + Petal_Width_cm, dc_train)

pred5 <- predict(log_model5, dc_test)

table(dc_test$Flower_Type, pred5)

# This model is overfitted, so i will add a noise

log_model6 <- multinom(Flower_Type ~ Avg_G + Avg_B + Petal_Width_cm + Avg_R, dc_train)

pred6 <- predict(log_model6, dc_test)

table(dc_test$Flower_Type, pred6)

# This model gives the most balanced and realistic results, combining color features with Petal_Width_cm.

pR2(log_model6)

# The McFadden R² is 0.97, which means the model fits the data very well. 
# The classes are strongly separated, so this is the best and final model, even if it is not perfect.