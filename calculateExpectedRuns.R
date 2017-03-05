library(dplyr)
library(data.table)
library(stringr)

pitch.atbat <- fread('C:/baseball/data sets/pitch.atbat.2015_2016.csv')
pitch.atbat$year <- substr(pitch.atbat$gameday_link,5,8)
pitch.atbat$month <- substr(pitch.atbat$gameday_link,10,11)
pitch.atbat$day <- substr(pitch.atbat$gameday_link,13,14)
pitch.atbat <- pitch.atbat[!(year=='2015' & month=='10' & day > '04')]
pitch.atbat <- pitch.atbat[!(year=='2016' & month=='10' & day > '02')]
pitch.atbat <- pitch.atbat[month!='11']
pitch.atbat$year <- pitch.atbat$month <- pitch.atbat$day <- NULL

## we need player names mpped to id's for determining where baserunners are
## after a play, based on the atbat_des field.
pitcher.ids <- pitch.atbat[,c('pitcher','pitcher_name'),with=F]
hitter.ids <- pitch.atbat[,c('batter','batter_name'),with=F]
names(pitcher.ids) <- c('player.id','player.name')
names(hitter.ids) <- c('player.id','player.name')
player.ids <- rbind(pitcher.ids,hitter.ids)
player.ids <- unique(player.ids[,list(player.id,player.name)])

pitch.atbat$atbat_des <- gsub(".*upheld: ", "", pitch.atbat$atbat_des)
pitch.atbat$atbat_des <- gsub(".*overturned: ", "", pitch.atbat$atbat_des)

pitch.atbat$batter_name <- gsub('\\.','',pitch.atbat$batter_name)
player.ids$player_name <- gsub('\\.','',player.ids$player_name)
pitch.atbat$atbat_des <- gsub('\\. ','',pitch.atbat$atbat_des)
pitch.atbat$atbat_des <- ifelse(substr(pitch.atbat$atbat_des,2,2)==' ',
                                paste0(substr(pitch.atbat$atbat_des,1,1),
                                substr(pitch.atbat$atbat_des,3,
                                nchar(pitch.atbat$atbat_des))),pitch.atbat$atbat_des)
pitch.atbat$atbat_des <- ifelse(substr(pitch.atbat$atbat_des,3,4)=='  ',
                                paste0(substr(pitch.atbat$atbat_des,1,2),
                                substr(pitch.atbat$atbat_des,4,
                                nchar(pitch.atbat$atbat_des))),pitch.atbat$atbat_des)
pitch.atbat$des.batter <- word(string = pitch.atbat$atbat_des, start = 1, end = 2, sep = fixed(" "))
pitch.atbat$des.batter <- gsub('\\.','',pitch.atbat$des.batter)
pitch.atbat$batter_name <- ifelse(!str_detect(pitch.atbat$atbat_des,'intentionally walks'),pitch.atbat$des.batter,pitch.atbat$batter_name)
pitch.atbat$des.batter <- NULL

player.ids <- merge(player.ids,pitch.atbat,by.x='player.id',by.y='batter',all.x=F,all.y=F)[,c('player.id','player.name','batter_name'),with=F]
player.ids <- player.ids[!duplicated(player.name)]

pitch.atbat <- merge(pitch.atbat,player.ids,by.x='on_1b',by.y='player.id',all.x=T,all.y=F)
names(pitch.atbat)[names(pitch.atbat)=='player.name'] <- 'name.1b'
pitch.atbat <- merge(pitch.atbat,player.ids,by.x='on_2b',by.y='player.id',all.x=T,all.y=F)
names(pitch.atbat)[names(pitch.atbat)=='player.name'] <- 'name.2b'
pitch.atbat <- merge(pitch.atbat,player.ids,by.x='on_3b',by.y='player.id',all.x=T,all.y=F)
names(pitch.atbat)[names(pitch.atbat)=='player.name'] <- 'name.3b'
pitch.atbat <- pitch.atbat[order(gameday_link,num)]
pitch.atbat$name.1b[is.na(pitch.atbat$name.1b)] <- 0
pitch.atbat$name.2b[is.na(pitch.atbat$name.2b)] <- 0
pitch.atbat$name.3b[is.na(pitch.atbat$name.3b)] <- 0
pitch.atbat$on_1b[is.na(pitch.atbat$on_1b)] <- 0
pitch.atbat$on_2b[is.na(pitch.atbat$on_2b)] <- 0
pitch.atbat$on_3b[is.na(pitch.atbat$on_3b)] <- 0

pitch.atbat <- pitch.atbat[,c(1:20,22),with=F]
names(pitch.atbat)[11] <- 'batter_name'

pitch.atbat$event <- ifelse(str_detect(pitch.atbat$atbat_des,'homers'),'Home Run',
                        ifelse(str_detect(pitch.atbat$atbat_des,'triples'),'Triple',
                        ifelse(str_detect(pitch.atbat$atbat_des,'doubles|ground rule double'),'Double',
                        ifelse(str_detect(pitch.atbat$atbat_des,'flies out'),'Flyout',
                        ifelse(str_detect(pitch.atbat$atbat_des,'pops out'),'Pop Out',pitch.atbat$event)))))

### now we need to place runners on their end bases after a play.
## first we remove runners if they have advanced or are thrown out.
pitch.atbat$end_3b <- ifelse(str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.3b,'scores'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.3b,'out at home')),
                             0,pitch.atbat$on_3b)

pitch.atbat$end_2b <- ifelse(str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.2b,'scores'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.2b,'out at home'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.2b,'to 3rd'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.2b,'out at 3rd')),
                             0,pitch.atbat$on_2b)

pitch.atbat$end_1b <- ifelse(str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'scores'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'out at home'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'to 3rd'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'out at 3rd'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'to 2nd'))|
                             str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'out at 2nd')),
                             0,pitch.atbat$on_1b)

## next, we place the runners where they belong, based on the event and atbat_des


pitch.atbat$end_3b <- ifelse((pitch.atbat$event=='Triple' &! 
                               (str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'scores'))&
                                str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'out at home'))))|
                                str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.2b,'to 3rd')) | 
                                str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'to 3rd')) |
                                str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'to 3rd')),
                                 1,pitch.atbat$end_3b)

pitch.atbat$end_2b <- ifelse(pitch.atbat$event=='Double' & !
                                (str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'to 3rd'))|
                                 str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'scores'))|
                                 str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'out at third'))|
                                 str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'out at home')))|
                                 str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$name.1b,'to 2nd')) |
                                 str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'to 2nd')),
                                  1,pitch.atbat$end_2b)

pitch.atbat$end_1b <- ifelse((pitch.atbat$event %in% c('Catcher Interference','Field Error',
                                                      'Fielders Choice','Fielders Choice Out','Forceout',
                                                      'Hit By Pitch','Intent Walk','Single','Walk')|
                              str_detect(pitch.atbat$atbat_des,
                              paste(pitch.atbat$batter_name,'to 1st'))) &! 
                              (str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'to 2nd'))|
                               str_detect(pitch.atbat$atbat_des[12],paste(pitch.atbat$batter_name[12],'to 3rd'))|
                               str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'scores'))|
                               str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'out at 2nd'))|
                               str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'out at 3rd'))|
                               str_detect(pitch.atbat$atbat_des,paste(pitch.atbat$batter_name,'out at home'))),
                                1,pitch.atbat$end_1b)


## in order to find the outs before the action, we need to subtract the outs that 
## occurred on the play, including runners thrown out, and event outs.
pitch.atbat$running.outs <- str_count(pitch.atbat$atbat_des,'out at')

pitch.atbat$event.outs <- ifelse(pitch.atbat$event %in% c('Batter Interference',
                                                          'Bunt Groundout','Bunt Lineout','Bunt Pop Out',
                                                          'Fan interference','Fielders Choice Out','Flyout',
                                                          'Forceout','Groundout','Lineout','Pop Out','Sac Bunt',
                                                          'Sac Fly','Strikeout'),1,
                                 ifelse(str_detect(pitch.atbat$event,'Double Play|DP'),2,
                                        ifelse(str_detect(pitch.atbat$event,'Triple Play'),3,0)))

## we need to be sure not to double subtract for these events.
pitch.atbat$start.outs <- ifelse(str_detect(pitch.atbat$event,
                                            'Double Play|DP|Forceout|Fielders Choice Out'),
                                 pitch.atbat$o-pitch.atbat$event.outs,
                                 pitch.atbat$o-
                                   (pitch.atbat$running.outs+pitch.atbat$event.outs))

pitch.atbat$start.outs <- ifelse(str_detect(pitch.atbat$atbat_des,
                                            paste(pitch.atbat$batter_name,'to')) & 
                                   pitch.atbat$event %in% c('Sac Bunt','Strikeout'),
                                   pitch.atbat$start.outs+1,pitch.atbat$start.outs)

pitch.atbat$start.outs <- ifelse(str_detect(pitch.atbat$atbat_des,'triple play'),
                                 0,pitch.atbat$start.outs)

## we need to know how many runs scored on the play to know the team runs before the play.
pitch.atbat$away_team_runs <- as.numeric(pitch.atbat$away_team_runs)
pitch.atbat$home_team_runs <- as.numeric(pitch.atbat$home_team_runs)
pitch.atbat$runs.on.play <- str_count(pitch.atbat$atbat_des,'scores')
pitch.atbat$runs.on.play <- ifelse(pitch.atbat$event=='Home Run',
                                   pitch.atbat$runs.on.play+1,pitch.atbat$runs.on.play)
pitch.atbat$start.runs <- ifelse(pitch.atbat$inning_side=='top',
                                 pitch.atbat$away_team_runs-pitch.atbat$runs.on.play,
                                 pitch.atbat$home_team_runs-pitch.atbat$runs.on.play)
pitch.atbat$end.runs <- ifelse(pitch.atbat$inning_side=='top',
                               pitch.atbat$away_team_runs,pitch.atbat$home_team_runs)

## if there are three outs, remove all baserunners at the end of the play.
pitch.atbat$end_1b <- ifelse(pitch.atbat$o>2,0,pitch.atbat$end_1b)
pitch.atbat$end_2b <- ifelse(pitch.atbat$o>2,0,pitch.atbat$end_2b)
pitch.atbat$end_3b <- ifelse(pitch.atbat$o>2,0,pitch.atbat$end_3b)

pitch.atbat$on_1b <- ifelse(pitch.atbat$on_1b!=0,1,pitch.atbat$on_1b)
pitch.atbat$end_1b <- ifelse(pitch.atbat$end_1b!=0,1,pitch.atbat$end_1b)
pitch.atbat$on_2b <- ifelse(pitch.atbat$on_2b!=0,1,pitch.atbat$on_2b)
pitch.atbat$end_2b <- ifelse(pitch.atbat$end_2b!=0,1,pitch.atbat$end_2b)
pitch.atbat$on_3b <- ifelse(pitch.atbat$on_3b!=0,1,pitch.atbat$on_3b)
pitch.atbat$end_3b <- ifelse(pitch.atbat$end_3b!=0,1,pitch.atbat$end_3b)

pitch.atbat$start.state <- paste0(pitch.atbat$start.outs,pitch.atbat$on_1b,
                                  pitch.atbat$on_2b,pitch.atbat$on_3b)
pitch.atbat$end.state <- paste0(pitch.atbat$o,pitch.atbat$end_1b,
                                pitch.atbat$end_2b,pitch.atbat$end_3b)

## .N is the last row in the 'by' group.  We need to know the team's runs at the end of
## the inning in order to know the runs over the rest of the inning for each at bat.
pitch.atbat <- pitch.atbat[,':='(away.runs.roi=away_team_runs[.N]),
                           by=c('gameday_link','inning','inning_side')]
pitch.atbat <- pitch.atbat[,':='(home.runs.roi=home_team_runs[.N]),
                           by=c('gameday_link','inning','inning_side')]

## roi is 'rest of inning'
pitch.atbat$away.runs.roi <- ifelse(pitch.atbat$inning_side=='top',
                                    pitch.atbat$away.runs.roi-pitch.atbat$start.runs,
                                    pitch.atbat$away.runs.roi)
pitch.atbat$home.runs.roi <- ifelse(pitch.atbat$inning_side=='bottom',
                                    pitch.atbat$home.runs.roi-pitch.atbat$start.runs,
                                    pitch.atbat$home.runs.roi)
pitch.atbat$runs.roi <- ifelse(pitch.atbat$inning_side=='top',
                               pitch.atbat$away.runs.roi,pitch.atbat$home.runs.roi)

## if a runner gets picked off or is caught stealing for the third out, 
## we don't want to count the current at bat, since he will lead off next inning.
pitch.atbat <- pitch.atbat[!(o==3 & str_detect(pitch.atbat$atbat_des,
                                               'caught stealing|picks off'))]

## There are just a few odd cases where I get negative outs, so I'm just removing 
## these records.  << 1%, so not mmuch harm.
expected.runs <- pitch.atbat[,.(exp.runs=mean(runs.roi)),
                             by=c('start.state')][substr(start.state,1,1)!='-']
expected.runs <- expected.runs[order(start.state)]
names(expected.runs)[names(expected.runs)=='start.state'] <- 'state'
expected.runs$exp.runs <- round(expected.runs$exp.runs,2)

temp <- pitch.atbat[,c('batter_name','start.state','end.state','runs.on.play','event','o','inning_side','atbat_des'),with=F]

write.csv(expected.runs,'C:/baseball/data sets/expected.runs.csv',row.names=F)
write.csv(pitch.atbat,'C:/baseball/data sets/expected.runs.data.csv',row.names=F)

