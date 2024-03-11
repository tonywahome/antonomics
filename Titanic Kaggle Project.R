#installing and loading libraries
install.packages("caret")
install.packages("mice")
install.packages("gridExtra")
install.packages('tidyverse')

library(tidyr)
library(ggplot2)
library(gridExtra)
library(mice)
library(caret)
library(tidyverse)
#loading data
test <- read.csv("test.csv")
train <- read.csv("train.csv")
View(test)
View(train)

#Plot 1
library(ggplot2)
ggplot(data = train) +
  geom_bar(mapping = aes(x = 'Transported', 
                         fill = Transported, )) +
  ggtitle(label = "Transported Count by Home planet") +
  facet_grid(~HomePlanet)

plot_gg(HomePlanet) + ggtitle(label = "Transported Counts By HomePlanet")

#Plot 2
ggplot(data = train) +
  geom_bar(mapping = aes(x = 'CryoSleep', 
                         fill = CryoSleep, )) +
  ggtitle(label = "Transported Count by CryoSleep") +
  facet_grid(~CryoSleep)

plot_gg(CryoSleep) + ggtitle(label = "Transported Counts By CryoSleep")

#Plot 3
ggplot(data = train) +
  geom_bar(mapping = aes(x = 'Destinantion',
                         fill = Destination, )) +
  ggtitle(label = "Transported Count by Destination") +
  facet_grid(~Destination)

plot_gg(Destination) +  ggtitle(label = "Transported Count by Destination")

#Total expenditure
train$total_spent = rowSums(train[c(8:12)], na.rm = TRUE)
train$total_spent_cat = ifelse(train$total_spent == 0, 1, 
                               ifelse(train$total_spent < 1000, 2,
                                      ifelse(train$total_spent < 2000, 3, 4)))

#categorial war
plot_gg = function(column){
  ggplot(data = train, mapping = aes(x = {{column}},
                                  fill = Transported)) +
    geom_bar(position = 'dodge') +scale_fill_manual('legend',
                                                    values = c("#F8766D", "#39954E"))
    
}

plot_gg(total_spent_cat) +
  ggtitle(label = "Transported Counts By HomePlanet")

#remove total spent
train <- subset.data.frame(train, select = -total_spent)
train <- subset.data.frame(train, select = -total_spent_cat)
View(train)  

#plot VIP
plot_gg(VIP) +
  ggtitle(label = "Transported Counts By VIP")

#split up cabin and passenger id columns

train2 = train %>%
  mutate(deck = str_sub(Cabin, 1, 1),
         num = as.numeric(str_sub(Cabin, 3, -3)),
         side = str_sub(Cabin, -1, -1),
         id1 = substring(PassengerId, 1, 4),
         id2 = substring(PassengerId, 6, 7)
         )
#calculate grp sizes
group_size = train2 %>%
  select(id1, id2) %>%
  group_by(id1) %>%
  summarize(size =max(as.numeric(id2))) %>%
  select(id1, size)

#add feature train 2
train2 = merge(x = train2, y = group_size,
               by = "id1") %>%
  mutate(size = ifelse(size>= 4, 4, size))
rm(group_size)


#plot the grp size feature
ggplot(data = train2, mapping = aes(x = size, fill = Transported)) +
  geom_bar(position = 'dodge') +
  ggtitle("Transported by Group Size") +
  scale_fill_manual('legend', values = c("#F8766D", "#39954E"))

#EDA using PassengerID
install.packages('ggExtra')
library(ggExtra)
 plot(1, type ='n', xlim = c(-1000, 10000), ylim = c(0, 0.00014),
     main = "PassengerId // Transported Distributions")
polygon(density.default(as.numeric(train2$id1[train2$Transported == 1]))
        , col = alpha(3, 0.5))
polygon(density.default(as.numeric(train2$id1[train2$Transported == 0]))
        , col = alpha(2, 0.5))
  legend(x = 8800, y = .00014, c("FALSE", "TRUE"), box.lty = 0,
         col = c(alpha(2, 0.5), alpha(3, 0.5)), pch = 20)
  #error in code 
  
  
#plot cabin
cabin1 = ggplot(data = train2, mapping = aes(x = deck, fill = Transported)) +
  geom_bar(position = 'dodge') +
  scale_fill_manual('legend', values = c("#F8766D", "#39954E")) +
  ggtitle("Transported by Deck")

cabin2 = ggplot(data = train2, mapping = aes(x = side, fill = Transported)) +
  geom_bar(position = 'dodge') +
  scale_fill_manual('legend', values = c("#F8766D", "#39954E")) +
  ggtitle("Transported by Side")

grid.arrange(cabin1, cabin2)  

#remove unwanted stuff
train3 = train2 %>%
  select(-c(Name, Cabin))
View(train3)  

#check missing values and props
na_prop = function(vec){
  sum(is.na(vec)) / length(vec)
}

lapply(train2[,1:ncol(train3)], na_prop)
1 - (nrow(drop_na(train2)) / nrow(train2))

train3 = train3 %>%
  mutate(RoomService = ifelse(is.na(RoomService) & CryoSleep == TRUE, 0, RoomService),
         FoodCourt = ifelse(is.na(FoodCourt) & CryoSleep == TRUE, 0, FoodCourt),
         ShoppingMall = ifelse(is.na(ShoppingMall) & CryoSleep == TRUE, 0, ShoppingMall),
         Spa =ifelse(is.na(VRDeck) & CryoSleep == TRUE, 0, VRDeck))

#check_na_prop
lapply(train3 %>% select(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck), na_prop)
train3_missing_hp = train3 %>%
  select(HomePlanet, id1) %>%
  filter(id1 %in% train3$id1[is.na(train3$HomePlanet)] & !is.na(HomePlanet)) %>%
  unique()
#merge
train3 = left_join(train3, train3_missing_hp, by = "id1") %>%
  select(-c(HomePlanet.x, HomePlanet.y))


#destination
train3_missing_dest = train3 %>% 
  select(Destination, id1) %>% 
  filter(id1 %in% train3$id1[is.na(train3$Destination)] & !is.na(Destination))  %>% 
  group_by(id1) %>%
  mutate(Destination = min(Destination)) %>% 
  unique()

#merge
train3 = left_join(train3, train3_missing_dest, by = "id1") %>%
  mutate(Destination = ifelse(!is.na(Destination.x), Destination.x, 
                              ifelse(is.na(Destination.y), NA, Destination.y))) %>%
  select(-c(Destination.x, Destination.y))

#deck
train3_missing_deck = train3 %>%
  select(deck, id1) %>%
  filter(id1 %in% train3$id1[is.na(train3$deck)] & !is.na(deck)) %>%
  group_by(id1) %>% 
  mutate(deck = min(deck)) %>%
  unique()

#merge
train3 = left_join(train3, train3_missing_deck, by="id1") %>%
  mutate(deck = ifelse(!is.na(deck.x), deck.x, ifelse(is.na(deck.y), NA, deck.y))) %>%
  select(-c(deck.x, deck.y))

#check props
lapply(train3 %>% select(HomePlanet, Destination, deck), na_prop)
View(na_prop)

#function for mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab ==max(tab)]
}

#lazy fill in
train3 = train2 %>%
  mutate(HomePlanet = ifelse(is.na(HomePlanet), "Mars", HomePlanet),
         deck = ifelse(is.na(deck), find_mode(train2$deck), deck),
         side = ifelse(is.na(side), find_mode(train2$side), side),
         num = ifelse(is.na(num), median(train2$num, na.rm = TRUE), num),
         CryoSleep = ifelse(is.na(CryoSleep), find_mode(train2$CryoSleep), CryoSleep),
         Destination = ifelse(is.na(Destination), find_mode(train2$Destination), Destination),
         VIP = ifelse(is.na(VIP), find_mode(train2$VIP), VIP),
         Transported = as.factor(as.numeric(Transported)))
         
rm(train3_missing_deck, train3_missing_dest, train3_missing_hp)


#linear model to predict age
train3 = train3 %>%
  mutate(id1 = as.numeric(id1),
         id2 = as.numeric(id2),
         side = as.factor(side),
         HomePlanet = as.factor(HomePlanet),
         Destination = as.factor(Destination),
         deck = as.factor(deck))
         
#rmse func
rmse = function(model){
  preds = predict(model, test_age)
  sqrt(mean(preds - test_age$Age) ^ 2, na.rm = TRUE)
}

#Data frame without nulls
age_train = train3 %>%
  filter(!is.na(Age))
na_age_train = train3 %>%
  filter(is.na(Age))

#test // train
set.seed(1867)
train_idx_age = sample(1:nrow(age_train), 0.8 * nrow(age_train))
train_age = age_train[train_idx_age, ] %>% select(-PassengerId)
test_age = age_train[-train_idx_age, ] %>% select(-PassengerId)

#linear fit
age_lm1 = lm(Age ~ ., train_age)
age_lm1
#checking model
summary(age_lm1)
rmse(age_lm1)

#linear fit 2
age_lm2 = lm(Age ~ VIP + size + HomePlanet + Destination + deck, data = train_age)
age_lm2

summary(age_lm2)
rmse(age_lm2)

sqrt(mean((train_age$Age - median(train_age$Age)) ^ 2))
head(train3)

#train on entire datset
age_lm2 = lm(Age ~ VIP +size +HomePlanet + Destination + deck, data = age_train)

#new age predictictions
train3$Age[is.na(train3$Age)] = predict(age_lm2, na_age_train)

#mice
amenity_train = train3 %>%
  select(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck, VIP, Age, side, deck, size)

#impute values
amenity_mice = mice(amenity_train, method = c(rep("pmm", 5), rep("", 5)), maxit = 20)

summary(amenity_train$RoomService)
amenity_train$imp$RoomService

final_clean_amenity_train = complete(amenity_mice, 1)

train3[,6:10] = final_clean_amenity_train[,1:5]

#modelling
train_idx =sample(1:nrow(train3), 0.8 * nrow(train3))
train = train3[train_idx, ] %>% select(-PassengerId)
