#############################################
#############     DSEA EVENT ################
############# Author: Rachel Lee ############
############# November, 19, 2021 ############
#############################################

# Dataset for this workshop is inspired by the one from https://www.kaggle.com/ronitf/heart-disease-uci
# For this session, please download the dataset here: https://github.com/leerachel20/Exploratory-Data-Analysis-with-DSEA 

# load packages
library(tidyverse)

## python packages:
## import pandas as pd
## import numpy as np
## import mathplotlib.pyplot as plt
## import seaborn as sea
## import warnings
## warnings.filterwarnings('ignore')


# If your data is poorly prepped, unreliable results can plague your 
# work no matter how cutting-edge your statistical artistry may be.

## Garbage in, garbage out!


# Data cleaning/wrangling steps
# 1. Familiarize yourself with the data set
# 2. Check for structural errors
# 3. Check for data irregularities
# 4. Decide how to deal with missing values
# 5. Document data versions and changes made




#####################################################

# read data
## python: data = pd.read_csv("heart.csv")
data <- read.csv("heartdisease.csv")

#an initial look at the data frame
str(data)

## python: data.head()
head(data)
tail(data)
dim(data)
nrow(data)
ncol(data)
glimpse(data)
broom::glance(data)

#####################################################

# duplicated data
duplicated(data)
sum(duplicated(data))
data[duplicated(data),]

# Option 1: Remove duplicates
data.unique <- distinct(data)
sum(duplicated(data.unique))

# Option 2: dropping the duplicated data
data.unique <- data %>%
  distinct(.keep_all = TRUE)

# Option 3: keeping the unique data
data.unique <- unique(data)

#####################################################

# missing values
# install.packages("visdat")
library(visdat)
vis_miss(data.unique)

data_without_missing <- na.omit(data.unique)
vis_miss(data_without_missing)

dat <- ___________

#####################################################


# Binary: sex, fbs, exang, target
# Categorical: cp, restecg, slope, ca, thal
# Continuous: age, trestbps, chol, thalac, oldpeak

# Jumping into data visualization
dat %>% ggplot(aes(x = ___, y = ____)) + 
  geom_boxplot(fill = ___) + 
  labs(x = ___, y =___) +
  facet_grid(~__)

# mod <- lm(target ~ ., data = dat)
# cooksd <- cooks.distance(mod)
# plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm = T), col = "red")  # add cutoff line
# text(x = 1:length(cooksd) + 1, y=cooksd, labels = ifelse(cooksd > 4*mean(cooksd, na.rm = T), names(cooksd),""), col = "red")  # add labels

# influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm = T))])  # influential row numbers
# head(dat[influential, ], 10) 

################ Data Transformation ################
summary(data)

library(dplyr)
data2 <- dat %>%
  mutate(sex = if_else(sex == 1, "Male", "Female"), # coercion 
         cp = ____,
         fbs = ____,
         restecg = ____,
         exang = ____,
         slope = ____,
         ca = ____,
         thal = ____,
         target = ____ %>%
  mutate_if(___) %>%
  dplyr::select(target, sex, cp, fbs, restecg, exang, slope, ca, thal, everything())




################ Data Visualization #####################

### Bar plot for target (heart disease)
ggplot(data2, aes(x = ____, fill = ____)) +
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Presence/Absence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = ____)

# proportion table
prop.table(___)

# count the frequency of the values of age
data2 %>% ___

# comparing bp across chest pain
data2 %>% ___


### Correlation
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# install.packages("corrplot")
library(corrplot)
corrplot(cor(data2[,10:14]), method = 'square', type = 'upper')
