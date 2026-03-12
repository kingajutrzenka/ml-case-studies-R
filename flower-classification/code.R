install.packages("broom")
install.packages("showtext")
install.packages("vtreat")
install.packages("ranger")

library(dplyr)
library(purrr)
library(broom)
library(tidyverse)
library(ggplot2)
library(showtext)
library(vtreat)
library(ranger)

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
names(dt)[names(dt) == "Number_of_Petals/Sepals"] <- "Number_of_Petals_Sepals"
head(dt)

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
    text = element_text(family = "Cascadia", size = 15),
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
