data2 <- read.csv("./March_Madness_data.csv", header = T, sep = ",")
class(data2)
data2 <- data.table(data2)
library(data.table)
library(dplyr)
library(caret)

# Break data into team 1 and 2 sets
madness_team1 <- data2[,.(season, team1_id, team1_fg2pct, team1_fg3pct, 
                         team1_ftpct, team1_blockpct, team1_ap_final)]
madness_team2 <- data2[,.(season, team2_id, team2_fg2pct, team2_fg3pct, 
                          team2_ftpct, team2_blockpct, team2_ap_final)]

# Replace all NA values with 0 in the ap_finAL rank. The only relevant ranks
# are from 1 to 25
madness_team1$team1_ap_final <- replace(madness_team1$team1_ap_final, 
                                       is.na(madness_team1$team1_ap_final), 0)
madness_team2$team2_ap_final <- replace(madness_team2$team2_ap_final, 
                                        is.na(madness_team2$team2_ap_final), 0)

# Filter out teams that failed to make the rank
team1_top25 <- madness_team1 %>%
  filter(team1_ap_final >= 1)
  
team2_top25 <- madness_team2 %>%
  filter(team2_ap_final >= 1)

# rename team1_id and team2_id to team_id and merge both datasets
setnames(team1_top25, old = c('team1_id', 'team1_fg2pct', 'team1_fg3pct', 
                              'team1_ftpct', 'team1_blockpct', 'team1_ap_final'), 
         new = c('team_id', 'fg2pct', 'fg3pct', 
                 'ftpct', 'blockpct', 'ranking'))
setnames(team2_top25, old = c('team2_id', 'team2_fg2pct', 'team2_fg3pct', 
                              'team2_ftpct', 'team2_blockpct', 'team2_ap_final'), 
         new = c('team_id', 'fg2pct', 'fg3pct', 
                 'ftpct', 'blockpct', 'ranking'))

top25 <- rbind(team1_top25, team2_top25) 

# group the data table by year and ensure there is no duplicate data
annual <- top25 %>%
  group_by(season, team_id)%>%
  distinct(ranking, fg2pct, fg3pct, 
           ftpct, blockpct) %>%
  mutate(avg_shooting_percentage = (fg2pct + fg3pct + ftpct)/3)

# plot 1 to have a visualization of team performance
library(ggplot2)
library(gridExtra)
plot1 <- ggplot(annual, aes(x = season, y = team_id))+ 
  geom_point(aes(color=factor(ranking)))
plot2 <- ggplot(annual, aes(x = avg_shooting_percentage, y = team_id))+ 
  geom_point(aes(color=factor(ranking)))

# modelin
annual2 <- annual[,c('team_id', 'ranking', 'season', 
                     'avg_shooting_percentage', 'blockpct')]
annual2$season <- as.factor(annual2$season)
annual2$ranking <- as.factor(annual2$ranking)
annual2$team_id <- as.factor(annual2$team_id)
preProcValues <- preProcess(annual2, method = c("center", "scale"))
building <- predict(preProcValues, annual2)
inTrain <- createDataPartition(y=building$ranking, p=0.7, list=FALSE)
training <- building[inTrain,]; testing <- building[-inTrain,]

require(randomForest);
set.seed(4400)
mod1 <- train(ranking ~.,method="rf",data=training, metric = "Accuracy",
              trControl = trainControl(method = "cv"), number=5)
mod2 <- train(ranking ~.,method="rpart",data=training)
mod3 <- train(ranking ~.,method="gbm",data=training)

pred1 <- predict(mod1, testing); pred2 <- predict(mod2, testing); 
pred3 <- predict(mod3, testing)

predDF <- data.frame(pred1,pred2,pred3,ranking=testing$ranking)
set.seed(23465)
combModFit <- train(ranking ~.,method="rpart",data=predDF)
combPred <- predict(combModFit,predDF)

confusionMatrix(pred1, testing$ranking)
confusionMatrix(pred2, testing$ranking)
confusionMatrix(pred3, testing$ranking)
confusionMatrix(combPred, testing$ranking)