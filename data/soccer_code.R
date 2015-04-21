library(plyr)
library(dplyr)
library(ggplot2)

yrs <- c('0304',
        '0405',
        '0506',
        '0607',
        '0708',
        '0809',
        '0910',
        '1011',
        '1112',
        '1213',
        '1314',
        '1415')

italy <- list()

for (i in 1:length(yrs)) {
    yr <- yrs[i]
    url <- paste("http://www.football-data.co.uk/mmz4281/",yr,"/I1.csv", sep="")
    tmp <- read.csv(file=url, stringsAsFactors=F)
    index <- paste("yr",yr, sep="")
    italy[[index]] <- tmp
}

my_cols <- c('Div',
             'Date',
             'HomeTeam',
             'AwayTeam',
             'FTHG',
             'FTAG',
             'FTR',
             'HS',
             'AS',
             'HF',
             'AF',
             'HY',
             'AY',
             'HR',
             'AR')

find_missing <- lapply(italy, function(x) intersect(names(x), my_cols))

bpl_sub <- lapply(bpl[3:12], function(x) x[my_cols])
spain_sub <- lapply(spain[3:12], function(x) x[my_cols])
germany_sub <- lapply(germany[3:12], function(x) x[my_cols])
italy_sub <- lapply(italy[3:12], function(x) x[my_cols])

seasons <- c('05/06',
         '06/07',
         '07/08',
         '08/09',
         '09/10',
         '10/11',
         '11/12',
         '12/13',
         '13/14',
         '14/15')
add_season <- function(x) {
    for (i in 1:length(x)) {
        x[[i]]$season <- seasons[i]
    }
    
    x
}

bpl_sub <- add_season(bpl_sub)
spain_sub <- add_season(spain_sub)
germany_sub <- add_season(germany_sub)
italy_sub <- add_season(italy_sub)

bpl_all <- ldply(bpl_sub)
spain_all <- ldply(spain_sub)
germany_all <- ldply(germany_sub)
italy_all <- ldply(italy_sub)

bpl_all <- bpl_all[complete.cases(bpl_all),]
spain_all <- spain_all[complete.cases(spain_all),]
germany_all <- germany_all[complete.cases(germany_all),]
italy_all <- italy_all[complete.cases(italy_all),]

season_group <- function(df) {
    tmp <- df %>%
        group_by(season) %>%
        summarize(TotalGames = n(),
                  TotalGoals = sum(FTHG) + sum(FTAG),
                  TotalShots = sum(HS) + sum(AS),
                  TotalFouls = sum(HF) + sum(AF),
                  TotalYellow = sum(HY) + sum(AY),
                  TotalRed = sum(HR) + sum(AR))
    tmp
                  
}

bpl_summary <- season_group(bpl_all)
spain_summary <- season_group(spain_all)
germany_summary <- season_group(germany_all)
italy_summary <- season_group(italy_all)

bpl_summary$league <- "England"
spain_summary$league <- "Spain"
germany_summary$league <- "Germany"
italy_summary$league <- "Italy"

total_summary <- rbind(bpl_summary, spain_summary, germany_summary, italy_summary)

ggplot(total_summary, aes(x=season, y=TotalFouls/(TotalYellow+TotalRed), group=league, color=league)) + geom_line()


fouls_per_game <- total_summary %>%
    mutate(fouls_per_game = TotalFouls/TotalGames) %>%
    select(season, league, fouls_per_game)

fouls_per_card <- total_summary %>%
    mutate(fouls_per_card = TotalFouls/(TotalYellow + TotalRed)) %>%
    select(season, league, fouls_per_card)

write.csv(fouls_per_game, file="fouls_per_game.csv")
write.csv(fouls_per_card, file="fouls_per_card.csv")

fouls <- merge(fouls_per_game, fouls_per_card)
fouls <- fouls %>% arrange(league, season)
write.csv(fouls, file="fouls.csv")
