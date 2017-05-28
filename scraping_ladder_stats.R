library(data.table)
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(jsonlite)

# Blizzard app 

## initiate keys from file

keys = read.csv2(file = "E:/R/blizzard/passfile.txt", sep = ",",as.is = TRUE)

# keys can be obtained at blizzard website after creating developer account
api_key = keys$key_value[which(keys$key_type == 'key')]
secret  = keys$key_value[which(keys$key_type == 'secret_key')]

# access token can be easily gotten from community API at same website
access_token  = keys$key_value[which(keys$key_type == 'access_token')]

# get current league
recent_league <- fromJSON(paste(c('https://eu.api.battle.net/data/sc2/season/current?access_token=',access_token), collapse = ""))


# 201 - legacy of the void 1v1
# 5 - master ladder

# Load all ladders in current season
all_ladders <- fromJSON(paste(c('https://eu.api.battle.net/data/sc2/league/',
                                  recent_league$id,'/201/0/5?access_token=',
                                  access_token), collapse = ""
                                )
                        )

master_ladders = data.table::rbindlist(all_ladders$tier$division)


alllad = c()
for (ladder_id in master_ladders$ladder_id) {
    target_url = paste(c('https://eu.api.battle.net/sc2/ladder/',ladder_id,
                         '?locale=en_GB&apikey=',
                         api_key), collapse = "")
    ladder = fromJSON(target_url)
    if (!is.null(ladder$ladderMembers)) {
        ladtab = flatten(ladder$ladderMembers, recursive = TRUE)
        alllad = rbind(alllad,ladtab)
        print('This one.')
    }
    else {
        print('Not this one.')
    }    
    }


# Smoothed and unsmoothed winrate distribution
aggregates = alllad %>% 
    group_by(character.displayName) %>% 
    summarise(winsum = sum(wins), losssum = sum(losses)) %>% 
    summarise(winavg = mean(winsum), lossavg = mean(losssum))

beta_score = log(length(unique(alllad$character.id)))/
    (aggregates[1]/(aggregates[1]+aggregates[2]))
alpha_score = log(length(unique(alllad$character.id)))

alllad %>% 
    group_by(character.displayName) %>% 
    summarise(winrate = (sum(wins)+as.integer(alpha_score))/ (sum(wins + losses)+as.integer(beta_score)), 
              winsum = sum(wins), losssum = sum(losses)) %>% 
    arrange(desc(winrate)) %>%
    ggplot(aes(x=winrate)) + geom_density(alpha=.5) + ggtitle('smoothed winrate')


alllad %>% 
    group_by(character.displayName) %>% 
    summarise(winrate = (sum(wins))/ (sum(wins + losses)), 
              winsum = sum(wins), losssum = sum(losses)) %>% 
    arrange(desc(winrate)) %>%
    ggplot(aes(x=winrate)) + geom_density(alpha=.5)+ ggtitle('raw winrate')


# winrate distribution by favorite race
alllad %>% 
    group_by(favoriteRaceP1) %>% 
    summarise(winrate = (sum(wins)+as.integer(alpha_score))/(sum(wins + losses)+as.integer(beta_score))) %>% 
    arrange(desc(winrate)) %>%
    ggplot(aes(x=favoriteRaceP1, y=winrate,fill=favoriteRaceP1)) +
    geom_bar(colour="black", stat="identity")+ ggtitle('race winrate distribution')





alllad %>% 
    group_by(character.displayName) %>% 
    summarise(winrate = (sum(wins)+as.integer(alpha_score))/ (sum(wins + losses)+as.integer(beta_score)), 
              winsum = sum(wins), losssum = sum(losses)) %>% 
    arrange(desc(winrate))




