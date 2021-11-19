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

# read data
## python: data = pd.read_csv("heart.csv")
data <- read.csv("heartdisease.csv", fileEncoding = 'UTF-8-BOM') #byte-order-mark
# The UTF-8 representation of the BOM is the (hexadecimal) byte sequence

# colnames(data)[1] <- gsub('^...','', colnames(data)[1])

#an initial look at the data frame
str(data)


############### Data Wrangling ##############
# head
## python: data.head()
head(data, 10)
tail(data, 7)
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

dat <- data_without_missing

#####################################################


# Binary: sex, fbs, exang, target
# Categorical: cp, restecg, slope, ca, thal
# Continuous: age, trestbps, chol, thalac, oldpeak

# Jumping into data visualization
dat %>% ggplot(aes(x = sex, y = trestbps)) + 
  geom_boxplot(fill = 'green') + 
  labs(x = "gender", y = "blood pressure") +
  facet_grid(~cp)

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
         cp = if_else(cp == 0, "TYPICAL ANGINA",
                      if_else(cp == 1, "ATYPICAL-AGINAL PAIN", 
                              if_else(cp == 2, "NON-AGINAL PAIN", "ASYMPTOMATIC PAIN"))),
         fbs = if_else(fbs == 1, ">120", "<120"),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMAL", "PROBABLE OR DEFINITE")),
         exang = if_else(exang == 1, "YES", "NO"),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(target, sex, cp, fbs, restecg, exang, slope, ca, thal, everything())




################ Data Visualization #####################

### Bar plot for target (heart disease)
ggplot(data2, aes(x = target, fill = target)) +
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Presence/Absence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("Absence", "Presence"))

# proportion table
prop.table(table(data2$target))

# count the frequency of the values of age
data2 %>% group_by(age) %>%
  count() %>%
  filter(n > 10) %>%
  ggplot() + geom_col(aes(x = age, y = n), fill = 'blue') +
  ggtitle("Age analysis") + xlab("age") + ylab("age count")

# comparing bp across chest pain
data2 %>% ggplot(aes(x = sex, y = trestbps)) + 
  geom_boxplot(fill = 'green') + 
  labs(x = "gender", y = "blood pressure") +
  facet_grid(~cp)


### Correlation
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# install.packages("corrplot")
library(corrplot)
corrplot(cor(data2[,10:14]), method = 'square', type = 'upper')
