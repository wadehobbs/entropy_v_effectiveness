library(gplots)
library(RColorBrewer)
library(plyr)
library(data.table)
library(tidyverse)
library(rlist)
library(entropy)
library(beepr)
library(bayesplot)
library(rjags)
library(gridExtra)
library(ggridges)

####Set data up ####
all_data <- readRDS('all_data')
aus <- filter(all_data, TeamName == "AUS")
blr_all_games <- filter(all_data, TeamName == "BLR")
bra <- filter(all_data, TeamName == "BRA")
fra <- filter(all_data, TeamName == "FRA")
jpn <- filter(all_data, TeamName == "JPN" | TeamName == "JAP")
tur <- filter(all_data, TeamName == "TUR")
usa <- filter(all_data, TeamName == "USA")

#Remove games so each set has the same number of games (10)
aus <- aus %>% filter(!(GameID %in% 'AUS v USA Q1' | GameID %in% 'AUS v USA Q2' | GameID %in% 'AUS v USA Q3' |GameID %in% 'AUS v USA Q4'))
blr <- blr %>% filter(!(GameID %in% 'BLR v ARG Q1' | GameID %in% 'BLR v ARG Q2' | GameID %in% 'BLR v ARG Q3' |GameID %in% 'BLR v ARG Q4' |
                        GameID %in% 'BLR v ARG Q5' | GameID %in% 'BLR v ARG Q6' | GameID %in% 'BLR v ARG Q7' |GameID %in% 'BLR v ARG Q8'))
fra <- fra %>% filter(!(GameID %in% 'FRA v ESP Q1' | GameID %in% 'FRA v ESP Q2' | GameID %in% 'FRA v ESP Q3' |GameID %in% 'FRA v ESP Q4'))
jpn <- jpn %>% filter(!(GameID %in% 'AUS v JPN austourg2(v2)' | GameID %in% 'AUS v JPN austourg2(v2)Q2' | GameID %in% 'AUS v JPN austourg2(v2)Q3' |GameID %in% 'AUS v JPN austourg2(v2)Q4' |
                        GameID %in% 'Aus v Jap G1Q1' | GameID %in% 'Aus v Jap G1Q2' | GameID %in% 'Aus v Jap G1Q3' |GameID %in% 'Aus v Jap G1Q4'))
usa <- usa %>% filter(!(GameID %in% 'USA v SEN Q1' | GameID %in% 'USA v SEN Q2' | GameID %in% 'USA v SEN Q3' |GameID %in% 'USA v SEN Q4'))

#Get shots only
data <- blr_all_games
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
        tmp <- data[ data$RecordID==rID, ]
        my_df <- rbind(my_df, tmp)
}
blr_all_shots = my_df

data <- blr
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
        tmp <- data[ data$RecordID==rID, ]
        my_df <- rbind(my_df, tmp)
}
blr_shot = my_df

data <- fra
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
        tmp <- data[ data$RecordID==rID, ]
        my_df <- rbind(my_df, tmp)
}
fra_shot = my_df

data <- jpn
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
        tmp <- data[ data$RecordID==rID, ]
        my_df <- rbind(my_df, tmp)
}
jpn_shot = my_df

data <- tur
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
        tmp <- data[ data$RecordID==rID, ]
        my_df <- rbind(my_df, tmp)
}
tur_shot = my_df

data <- usa
shot_vec <- data[ which(apply(data, 1, function(r) any(r %in% c("Shot")))),4]
shot_vec <- as.vector(shot_vec$RecordID)
my_df <- data.frame()
tmp <- data.frame()
for(rID in shot_vec){ 
        tmp <- data[ data$RecordID==rID, ]
        my_df <- rbind(my_df, tmp)
}
usa_shot = my_df

####Functions####
xy_extract_fun_pol <- function(curstates) {
        if(curstates == 1){
                x <- 1
                y <- 1
                xy <- list(x,y)
        } else if (curstates == 2) {
                x <- 2
                y <- 1
                xy <- list(x,y)
        } else if (curstates == 3) {
                x <- 4
                y <- 1
                xy <- list(x,y)
        } else if (curstates == 4) {
                x <- 5
                y <- 1
                xy <- list(x,y)
        } else if (curstates == 5) {
                x <- 1
                y <- 2
                xy <- list(x,y)
        } else if (curstates == 6) {
                x <- 2
                y <- 2
                xy <- list(x,y)
        } else if (curstates == 7) {
                x <- 3
                y <- 2
                xy <- list(x,y)
        } else if (curstates == 8) {
                x <- 5
                y <- 2
                xy <- list(x,y)
        } else if (curstates == 9) {
                x <- 1
                y <- 3
                xy <- list(x,y)
        } else if (curstates == 10) {
                x <- 2
                y <- 3
                xy <- list(x,y)
        } else if (curstates == 11) {
                x <- 3
                y <- 3
                xy <- list(x,y)
        } else if (curstates == 12) {
                x <- 4
                y <- 3
                xy <- list(x,y)
        } else if (curstates == 13) {
                x <- 1
                y <- 4
                xy <- list(x,y)
        } else if (curstates == 14) {
                x <- 2
                y <- 4
                xy <- list(x,y)
        } else if (curstates == 15) {
                x <- 3
                y <- 4
                xy <- list(x,y)
        } else if (curstates == 16) {
                x <- 4
                y <- 4
                xy <- list(x,y)
        } else if (curstates == 17) {
                x <- 5
                y <- 4
                xy <- list(x,y)
        } else if (curstates == 20) {
                x <- 3
                y <- 1
                xy <- list(x,y)
        } else if (curstates == 21) {
                x <- 4
                y <- 2
                xy <- list(x,y)
        } else if (curstates == 22) {
                x <- 5
                y <- 3
                xy <- list(x,y)
        } else if (curstates == 18) {
                x <- 1
                y <- 5
                xy <- list(x,y)
        } else if (curstates == 19) {
                x <- 2
                y <- 5
                xy <- list(x,y)
        }
        return(xy)
}


cart2pol = function(x, y)
{
        y = y - 16
        x = x - 31
        r = sqrt(x^2 + y^2)
        t = atan(x/y)  
        # t = (t/pi)*360
        # t = t-(pi/2)
        poldata = list(r,t)
}

cart2pol_midlineR = function(x, y)
{
        y = y - 16
        x = x - 6
        r = sqrt(x^2 + y^2)
        t = atan(x/y)
        # t = (t/pi)*360
        # t = t-(pi/2)
        poldata = list(r,t)
        return(poldata)
}

cart2pol_midlineL = function(x, y)
{
        y = y - 16
        x = x - 56
        r = sqrt(x^2 + y^2)
        t = atan(x/y)
        # t = (t/pi)*360
        # t = t-(pi/2)
        poldata = list(r,t)
        return(poldata)
}

cart2pol_highR = function(x, y)
{
        y = y - 12
        x = x - 14.3
        r = sqrt(x^2 + y^2)
        t = atan(x/y)
        # t = (t/pi)*360
        # t = t-(pi/2)
        poldata = list(r,t)
        return(poldata)
}

cart2pol_highL = function(x, y)
{
        y = y - 12
        x = x - 47.7
        r = sqrt(x^2 + y^2)
        t = atan(x/y)
        # t = (t/pi)*360
        # t = t-(pi/2)
        poldata = list(r,t)
        return(poldata)
}

std_fun <- 
function(n,p) {
        sd = sqrt(p*(1-p)/n)
        std = p/sd
        return(std)
}

####eff function specification####
loc <- c(1,2,3,3,2,1,1,2,3,3,2,1,5,4,4,4,4,5,5,5)
p0_fun_pol = function(game){
        data= usa_shot
        traj_vec <- unique(data$RecordID)
        short_df = data.frame()
        p_0 = data.frame()
        p_table = data.frame()
        last_label = data.frame()
        
        for (rID in traj_vec){
                #if(data[ data$RecordID==rID, 2] == "1st Quarter"){
                if (length(unique(seq(min(data[ data$RecordID==rID, 5]), max(data[ data$RecordID==rID, 5]),1))) <= 4){
                        short_tmp <- data[ data$RecordID==rID, ]
                        short_df <- rbind(short_df, short_tmp)
                        #write.table(short_df, "delete")
                        #seperates out the trajectories that are shorter than 4 seconds in length (cant be used in spline function)
                } else {
                        my_df <- data[ data$RecordID==rID, ]
                        poly_x <- smooth.spline(my_df$Time, my_df$x, df= length(unique(my_df$Time)))
                        poly_y <- smooth.spline(my_df$Time, my_df$y, df= length(unique(my_df$Time)))
                        pred_time <- seq(min(my_df$Time), max(my_df$Time), 1)
                        #Have created the poly function for the traj and created a time ref vector, now two df are created with the new values for x and y
                        pred_tmp_x <- data.frame(predict(poly_x, pred_time))
                        pred_tmp_y <- data.frame(predict(poly_y, pred_time))
                        pred_tmp <- data.frame(pred_tmp_x[,2],pred_tmp_y[,2])
                        #rename cols, create the curstates, dis and rad cols
                        colnames(pred_tmp) <- c("x", "y")
                        pred_tmp$curstates = 0
                        
                        pred_tmp$dis <- map2(pred_tmp$x, pred_tmp$y, cart2pol) %>% list.map(.[1])
                        pred_tmp$dis = unlist(pred_tmp$dis)
                        
                        pred_tmp$radml <- map2(pred_tmp$x, pred_tmp$y, cart2pol_midlineL) %>% list.map(.[2])
                        pred_tmp$radml = unlist(pred_tmp$radml)
                        
                        pred_tmp$radmr <- map2(pred_tmp$x, pred_tmp$y, cart2pol_midlineR) %>% list.map(.[2])
                        pred_tmp$radmr = unlist(pred_tmp$radmr)
                        
                        pred_tmp$radhl <- map2(pred_tmp$x, pred_tmp$y, cart2pol_highL) %>% list.map(.[2])
                        pred_tmp$radhl = unlist(pred_tmp$radhl)
                        
                        pred_tmp$radhr <- map2(pred_tmp$x, pred_tmp$y, cart2pol_highR) %>% list.map(.[2])
                        pred_tmp$radhr = unlist(pred_tmp$radhr)
                        
                        for (i in 1:nrow(pred_tmp)) {
                                if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] < 12 & pred_tmp$x[i] > 6) {
                                        pred_tmp$curstates[i] = 1
                                } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] <= 25 & pred_tmp$x[i] > 11) {
                                        pred_tmp$curstates[i] = 2
                                } else if (pred_tmp$y[i] < 21 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 31 & pred_tmp$x[i] < 37) {
                                        pred_tmp$curstates[i] = 4 #right side of under basket
                                } else if (pred_tmp$y[i] < 21 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <= 31) {
                                        pred_tmp$curstates[i] = 3 #Left side of under basket
                                } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] >= 37 & pred_tmp$x[i] <=50) {
                                        pred_tmp$curstates[i] = 5
                                } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] < 56 & pred_tmp$x[i] > 50) {
                                        pred_tmp$curstates[i] = 6
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 19 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {   #dis > 20 as the 3point line ranges from 19.2 to 20.55 in this section so an average was taken
                                        pred_tmp$curstates[i] = 7  
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] <=25 & pred_tmp$x[i] >= 12 & pred_tmp$dis[i] <= 20 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {
                                        pred_tmp$curstates[i] = 8
                                } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 31 & pred_tmp$x[i] < 37) {
                                        pred_tmp$curstates[i] = 10 #Right side of the mid key
                                } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <=31) {
                                        pred_tmp$curstates[i] = 9 #Left side of the mid key
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] >=37 & pred_tmp$x[i] < 51 & pred_tmp$dis[i] <= 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) {
                                        pred_tmp$curstates[i] = 11
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) { 
                                        pred_tmp$curstates[i] = 12 
                                } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 13
                                } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] < 26 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 14
                                } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] > 36 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 17
                                } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 18
                                } else if (pred_tmp$y[i] < 59 & pred_tmp$x[i] < 32 & pred_tmp$dis[i]  > 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 19
                                } else if (pred_tmp$y[i] < 59 & pred_tmp$y[i] > 31 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  > 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 20
                                } else if (pred_tmp$y[i] >= 59 & pred_tmp$y[i] <= 106 & pred_tmp$x[i] <=56 & pred_tmp$x[i] >= 6) {
                                        pred_tmp$curstates[i] = 30  
                                } else if (pred_tmp$y[i] > 106 | pred_tmp$y[i] < 12 | pred_tmp$x[i] < 6 | pred_tmp$x[i] > 56) {
                                        pred_tmp$curstates[i] = 40
                                } else if (pred_tmp$y[i] > 30 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 16 #Right elbow
                                } else if (pred_tmp$y[i] >30 & pred_tmp$x[i] <= 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 15} #Left elbow
                                #cat('i=',i,'   ')   
                                
                        }
                        

                        #takes the last z value from tmp_vec and binds them into a vector
                        p_tmp <- tail(pred_tmp$curstates,1)
                        p_0 <- rbind(p_tmp, p_0)
                        #adds the last z vector to a data frame of outcome of each possession
                        last_tmp <- tail(my_df[8:11], 1)
                        last_label <- rbind(last_tmp, last_label)
                        p_table <- cbind(last_label, p_0)
                        colnames(p_table) = c("Label1", "Label2", "Label3", "Label4", "z")
                }}#}
        
        #Convert df to dt
        setDT(p_table)
        #Takes "shot" from col 1 and turns it into whatever the results was (Score, miss)
        p_table[Label1 == "Shot", Label1 := Label2]
        #If "shot" has ended up in col 2 (becblre there was an on-ball or something), it will replace it with the end result of the play (Score, miss)
        p_table[Label2 == "Shot", Label1 := Label3]
        #If "on-ball" occurs at the same time as the play ending outcome, this will replace it with the outcome
        p_table[Label1 == "On-ball", Label1 := Label2]
        #If there is a mistake (Shot|TO) where it should be TO, this will fix it
        p_table[Label2 == "TO", Label1 := Label2]
        #Same as above
        p_table[Label3 == "TO", Label1 := Label3]
        
        #delete out of bounds cells
        #p_table = p_table[ ! p_table$X18 %in% ofb,]
        
        #Reorders the table to show z's as col names and labels as row names with counts of each
        p_table_cast = data.frame(dcast(p_table, Label1 ~ z))
        
        #Rename rows
        rownames(p_table_cast) = make.names(p_table_cast[,1])
        p_table_cast = (p_table_cast[,-1])
        
        #Remove unwanted rows
        # p_table_cast <- p_table_cast %>% rownames_to_column() %>%
                # filter(!(rowname %in% c('Stoppage', 'Foul', 'X'))) %>% #Remove the TO and foul columns - should be small numbers
                # select(-1)
                # t %>% #Tanspose 
                # as.data.frame() #Set as df
        p_table_cast <- p_table_cast %>%
                rownames_to_column %>%
                gather(var, val, -rowname) %>%
                spread(rowname, val) %>%
                select(.data$var, .data$Miss, .data$Score, .data$Shooting.Foul) %>%
                mutate(var = as.numeric(str_replace(var, "X", ""))) %>%
                arrange(var) %>%
                filter(!(var %in% c('30', '40'))) %>%
                #Add the efficiency column and total possessions
                mutate(Possessions = Miss + Score + Shooting.Foul, 
                                Eff = ((Score + Shooting.Foul) / (Miss + Score + Shooting.Foul)))
       
        #This part adds regions, converts 3p shot regions to true shooting % and adds standardised eff.
        # p_table_cast$region <- loc
        # p_table_cast$tmp <- p_table_cast$Eff    
        # p_table_cast$tmp <- ((p_table_cast$Score + p_table_cast$Shooting.Foul + 0.5 * p_table_cast$Score)/p_table_cast$Possessions)
        # p_table_cast$Eff[p_table_cast$region == 1 | p_table_cast$region == 5] <- p_table_cast$tmp[p_table_cast$region == 1 | p_table_cast$region == 5]
        # p_table_cast <- select(p_table_cast, -tmp)
        # p_table_cast$std <- std_fun(p_table_cast$Possessions, p_table_cast$Eff)
        return(p_table_cast)
}

aus_eff_table <- p0_fun_pol(aus_shot)
blr_eff_table <- p0_fun_pol(blr_shot)
fra_eff_table <- p0_fun_pol(fra_shot)
jpn_eff_table <- p0_fun_pol(jpn_shot)
tur_eff_table <- p0_fun_pol(tur_shot)
usa_eff_table <- p0_fun_pol(usa_shot)



#Combined std entropy
all_teams_stdeff <- tibble(aus = aus_eff_table$std, blr = blr_eff_table$std, fra = fra_eff_table$std, jpn = jpn_eff_table$std, tur = tur_eff_table$std, usa = usa_eff_table$std)
all_teams_stdeff <- rbind(all_teams_stdeff, summarise_all(all_teams_stdeff, mean))

####Ent function####

data = blr_all_shots
short_df <- data.frame()
short_tmp <- data.frame()
occ_array <- array(0, dim=c(5,5,25))
traj_vec <- unique(data$RecordID)
#Start of for loop - takes the raw data in and goes through each trajectory, applying following code to each individual trajectory
for (rID in traj_vec){
        #if(data[ data$RecordID==rID, 2] == "4th Quarter"){
        if (length(unique(seq(min(data[ data$RecordID==rID, 5]), max(data[ data$RecordID==rID, 5]),1))) <= 4){
                short_tmp <- data[ data$RecordID==rID, ]
                short_df <- rbind(short_df, short_tmp)
                write.table(short_df, "delete")
                #seperates out the trajectories that are shorter than 4 seconds in length (cant be used in spline function)
        } else {
                my_df <- data[ data$RecordID==rID, ]
                poly_x <- smooth.spline(my_df$Time, my_df$x, df= length(unique(my_df$Time)))
                poly_y <- smooth.spline(my_df$Time, my_df$y, df= length(unique(my_df$Time)))
                pred_time <- seq(min(my_df$Time), max(my_df$Time), 1)
                #Have created the poly function for the traj and created a time ref vector, now two df are created with the new values for x and y   
                pred_tmp_x <- data.frame(predict(poly_x, pred_time))[2]
                pred_tmp_y <- data.frame(predict(poly_y, pred_time))[2]
                pred_tmp <- data.frame(pred_tmp_x,pred_tmp_y)
                #rename cols, create the curstates, dis and rad cols
                colnames(pred_tmp) <- c("x", "y")
                pred_tmp$curstates = 0
                #use the cart2pol function to get dis and rad values for each entry in the predicted possession
                pred_tmp$dis <- map2(pred_tmp$x, pred_tmp$y, cart2pol) %>% list.map(.[1])
                pred_tmp$dis = unlist(pred_tmp$dis)
                
                pred_tmp$radml <- map2(pred_tmp$x, pred_tmp$y, cart2pol_midlineL) %>% list.map(.[2])
                pred_tmp$radml = unlist(pred_tmp$radml)
                
                pred_tmp$radmr <- map2(pred_tmp$x, pred_tmp$y, cart2pol_midlineR) %>% list.map(.[2])
                pred_tmp$radmr = unlist(pred_tmp$radmr)
                
                pred_tmp$radhl <- map2(pred_tmp$x, pred_tmp$y, cart2pol_highL) %>% list.map(.[2])
                pred_tmp$radhl = unlist(pred_tmp$radhl)
                
                pred_tmp$radhr <- map2(pred_tmp$x, pred_tmp$y, cart2pol_highR) %>% list.map(.[2])
                pred_tmp$radhr = unlist(pred_tmp$radhr)
                
                for (i in 1:nrow(pred_tmp)) {
                        if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] < 12 & pred_tmp$x[i] > 6) {
                                pred_tmp$curstates[i] = 1
                        } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] <= 25 & pred_tmp$x[i] > 11) {
                                pred_tmp$curstates[i] = 2
                        } else if (pred_tmp$y[i] < 21 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 31 & pred_tmp$x[i] < 37) {
                                pred_tmp$curstates[i] = 4 #right side of under basket
                        } else if (pred_tmp$y[i] < 21 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <= 31) {
                                pred_tmp$curstates[i] = 3 #Left side of under basket
                        } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] >= 37 & pred_tmp$x[i] <=50) {
                                pred_tmp$curstates[i] = 5
                        } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] < 56 & pred_tmp$x[i] > 50) {
                                pred_tmp$curstates[i] = 6
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 19 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {   #dis > 20 as the 3point line ranges from 19.2 to 20.55 in this section so an average was taken
                                pred_tmp$curstates[i] = 7  
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] <=25 & pred_tmp$x[i] >= 12 & pred_tmp$dis[i] <= 20 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {
                                pred_tmp$curstates[i] = 8
                        } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 31 & pred_tmp$x[i] < 37) {
                                pred_tmp$curstates[i] = 10 #Right side of the mid key
                        } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <=31) {
                                pred_tmp$curstates[i] = 9 #Left side of the mid key
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] >=37 & pred_tmp$x[i] < 51 & pred_tmp$dis[i] <= 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) {
                                pred_tmp$curstates[i] = 11
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) { 
                                pred_tmp$curstates[i] = 12 
                        } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 13
                        } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] < 26 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 14
                        } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] > 36 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 17
                        } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 18
                        } else if (pred_tmp$y[i] < 59 & pred_tmp$x[i] < 32 & pred_tmp$dis[i]  > 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 19
                        } else if (pred_tmp$y[i] < 59 & pred_tmp$y[i] > 31 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  > 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 20
                        } else if (pred_tmp$y[i] >= 59 & pred_tmp$y[i] <= 106 & pred_tmp$x[i] <=56 & pred_tmp$x[i] >= 6) {
                                pred_tmp$curstates[i] = 30  
                        } else if (pred_tmp$y[i] > 106 | pred_tmp$y[i] < 12 | pred_tmp$x[i] < 6 | pred_tmp$x[i] > 56) {
                                pred_tmp$curstates[i] = 40
                        } else if (pred_tmp$y[i] > 30 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 16 #Right elbow
                        } else if (pred_tmp$y[i] >30 & pred_tmp$x[i] <= 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 15} #Left elbow
                        #cat('i=',i,'   ')   
                        
                }
                
                #Cuts whole possession down to the last 5 seconds or whatever number i specifiy and reverses cell order so the occ_array is filled for the cell that the shot was taken in. 
                tmp_vec <- rev(tail(pred_tmp$curstates, 5))
                
                #Generate quantized values for each unique time point (x^ and y^)   
                #CREATE BREAK IF TMP_VEC AFTER FIRST VALUE == 15
                xy <- map(tmp_vec, xy_extract_fun_pol)
                xy <- matrix(unlist(xy), ncol=2, byrow = TRUE)
                #xy is a matrix of x and y grid locations for each of the values in the sliding window   
                y <- xy[,1]
                x <- xy[,2]
                
                my_matrix <- matrix(0, nrow=5, ncol=5)
                
                my_matrix[x[2],y[2]] = my_matrix[x[2],y[2]]+1
                my_matrix[x[3],y[3]] = my_matrix[x[3],y[3]]+1
                my_matrix[x[4],y[4]] = my_matrix[x[4],y[4]]+1
                my_matrix[x[5],y[5]] = my_matrix[x[5],y[5]]+1
                #adds a 1 to the matrix of the court in the cells that the ball has passed through    
                occ_array[,,tmp_vec[1]] = occ_array[,,tmp_vec[1]] + my_matrix
        }}
        shan_ent <- apply(occ_array, c(3), entropy)


####Entropy extraction####
aus_ent <- shan_ent
blr_ent <- shan_ent
fra_ent <- shan_ent
jpn_ent <- shan_ent
tur_ent <- shan_ent
usa_ent <- shan_ent


#aus_ent_rio <- ent_pol_fun_5(aus_rio)

all_teams_ent <- tibble(aus_ent, blr_ent, fra_ent, jpn_ent, tur_ent, usa_ent)
all_teams_ent <- all_teams_ent[c(1:20),]
#all_teams_ent <- rbind(all_teams_ent, summarise_all(all_teams_ent, mean))

#Count number of shots in dataset
s <- str_count(aus$Label1, 'Shot')
length(s[s == 1])

#Also have code to get entropy by shot clock - ie entropy in the first 8 seconds, middle 8 and last 8. 
#Decide if its going to be in the study, if not dont waste time doing it. 

####Combine variables and Plot raw data####

#Combined ent and eff for each team
loc <- as.factor(c(1,2,3,3,2,1,1,2,3,3,2,1,5,4,4,4,4,5,5,5))

aus_ent_eff <- data.frame(ent = all_teams_ent[1:20,1], eff = aus_eff_table$Eff, loc = loc)
blr_ent_eff <- data.frame(ent = all_teams_ent[1:20,2], eff = blr_eff_table$Eff, loc = loc)
fra_ent_eff <- data.frame(ent = all_teams_ent[1:20,3], eff = fra_eff_table$Eff, loc = loc)
jpn_ent_eff <- data.frame(ent = all_teams_ent[1:20,4], eff = jpn_eff_table$Eff, loc = loc)
tur_ent_eff <- data.frame(ent = all_teams_ent[1:20,5], eff = tur_eff_table$Eff, loc = loc)
usa_ent_eff <- data.frame(ent = all_teams_ent[1:20,6], eff = usa_eff_table$Eff, loc = loc)

aus_ent_stdeff <- data.frame(ent = all_teams_ent[1:20,1], eff = aus_eff_table$std, loc = loc)
blr_ent_stdeff <- data.frame(ent = all_teams_ent[1:20,2], eff = blr_eff_table$std, loc = loc)
fra_ent_stdeff <- data.frame(ent = all_teams_ent[1:20,3], eff = fra_eff_table$std, loc = loc)
jpn_ent_stdeff <- data.frame(ent = all_teams_ent[1:20,4], eff = jpn_eff_table$std, loc = loc)
tur_ent_stdeff <- data.frame(ent = all_teams_ent[1:20,5], eff = tur_eff_table$std, loc = loc)
usa_ent_stdeff <- data.frame(ent = all_teams_ent[1:20,6], eff = usa_eff_table$std, loc = loc)


raw_std_aus <- ggplot(aus_ent_stdeff, aes(x = aus_ent, y = eff, group = loc, colour = loc)) +
        geom_point() + 
        geom_smooth(method = lm, se = FALSE)

raw_std_blr <- ggplot(blr_ent_stdeff, aes(x = blr_ent, y = eff, group = loc, colour = loc)) +
        geom_point() + 
        geom_smooth(method = lm, se = FALSE)

raw_std_fra <- ggplot(fra_ent_stdeff, aes(x = fra_ent, y = eff, group = loc, colour = loc)) +
        geom_point() + 
        geom_smooth(method = lm, se = FALSE)

raw_std_jpn <- ggplot(jpn_ent_stdeff, aes(x = jpn_ent, y = eff, group = loc, colour = loc)) +
        geom_point() + 
        geom_smooth(method = lm, se = FALSE)

raw_std_tur <- ggplot(tur_ent_stdeff, aes(x = tur_ent, y = eff, group = loc, colour = loc)) +
        geom_point() + 
        geom_smooth(method = lm, se = FALSE)

raw_std_usa <- ggplot(usa_ent_stdeff, aes(x = usa_ent, y = eff, group = loc, colour = loc)) +
        geom_point() + 
        geom_smooth(method = lm, se = FALSE)

grid.arrange(raw_std_aus, raw_std_blr, raw_std_fra, raw_std_jpn, raw_std_tur, raw_std_usa)

####BHM code####
#Model with random intercept and random slope
hi_mod_string_ris_cor <- "model {

#-----liklihood of the data-----#

for (i in 1:length(y)) {                                      #For loop loops over all of the y data points
y[i] ~ dnorm(mu[i], prec)                                 #The ith response (eff) comes from a normal distribution with mean mu and precision prec
mu[i] = a[region[i]] + b[region[i]]*entropy[i]            #mean (mu[i]) of the liklihood is a linear model
}                                                             #each region gets its own intercept, as shown at the start of the lm

#------Prior for the precision in the liklihood------#

prec ~ dgamma(0.001, 0.001)                                    
sig = sqrt(1/prec)

#-----for loop for the region variable (y-intercept and slope)-----#                                                    

for (j in 1:max(region)) {                                    #each region intercept and slope are represented in a matrix
a[j]<-B[j,1]                                              #B is a matrix of random slopes and intercepts. a is intercept and b is slope. B is the matrix
b[j]<-B[j,2]                                              #This code sets up the multivariate normal probability model for the matrix of random effects
B[j,1:2]~dmnorm(B.hat[j,], Tau.B[,])
B.hat[j,1]<-mu.a
B.hat[j,2]<-mu.b                                        
}                                                            

#-----Priors for the above for loop-----#

mu.a~dnorm(0,.0001)                                           #mu.a and mu.b are the mean distributions for the B matrix
mu.b~dnorm(0,.0001)
Tau.B[1:2,1:2]<- inverse(Sigma.B[,])                          #Tau.B is the variance for the B matrix

#-----Priors for the variance and covariance of the random effects-----#

#variances of the random effects
Sigma.B[1,1]<-pow(sigma.a,2)                                  #Sigma.B denotes the covariance matrix
sigma.a~dunif(0,100)                                          #sigma.b denotes an entry in the matrix
Sigma.B[2,2]<-pow(sigma.b,2)
sigma.b~dunif(0,100)

#correlation
Sigma.B[1,2]<-rho*sigma.a*sigma.b
Sigma.B[2,1]<-Sigma.B[1,2]
rho~dunif(-1,1)

}"

#Run Model
set.seed(99)
data_jags_eff <- list(y = usa_ent_stdeff$eff, entropy = usa_ent_stdeff$usa_ent, region = usa_ent_stdeff$loc)

# list(y = aus_ent_stdeff$eff, entropy = aus_ent_stdeff$aus_ent, region = aus_ent_stdeff$loc)
# list(y = blr_ent_stdeff$eff, entropy = blr_ent_stdeff$blr_ent, region = blr_ent_stdeff$loc)
# list(y = fra_ent_stdeff$eff, entropy = fra_ent_stdeff$fra_ent, region = fra_ent_stdeff$loc)
# list(y = jpn_ent_stdeff$eff, entropy = jpn_ent_stdeff$jpn_ent, region = jpn_ent_stdeff$loc)
# list(y = tur_ent_stdeff$eff, entropy = tur_ent_stdeff$tur_ent, region = tur_ent_stdeff$loc)
# list(y = usa_ent_stdeff$eff, entropy = usa_ent_stdeff$usa_ent, region = usa_ent_stdeff$loc)

params_eff <- c("B", "sig", "mu.a", "sigma.a", "mu.b", "sigma.b", "rho")

intis <- function() { list(B=array(rnorm(2*J),c(J,2)), sigma.y=runif(1), mu.a=rnorm(1), sigma.a=runif(1), mu.b=rnorm(1), sigma.b=runif(1), rho=runif(1))}

hi_mod_pol <- jags.model(textConnection(hi_mod_string_ris_cor), data = data_jags_eff)
update(hi_mod_pol, 100000) #burn in

#posterior samples
hi_mod_pol_sim_std_usa = coda.samples(model = hi_mod_pol, variable.names = params_eff,n.iter = 10000000,thin = 5000)

#Basic plots
plot(hi_mod_sim_eff_no_cor, ask=TRUE)

#DIC test
dic.samples(hi_mod_eff_pol_ris_no_cor, n.iter = 100000)

#Make dfs
hi_mod_pol_sim_std_aus_df <- data.frame(hi_mod_pol_sim_std_aus[[1]])
hi_mod_pol_sim_std_blr_df <- data.frame(hi_mod_pol_sim_std_blr[[1]])
hi_mod_pol_sim_std_fra_df <- data.frame(hi_mod_pol_sim_std_fra[[1]])
hi_mod_pol_sim_std_jpn_df <- data.frame(hi_mod_pol_sim_std_jpn[[1]])
hi_mod_pol_sim_std_tur_df <- data.frame(hi_mod_pol_sim_std_tur[[1]])
hi_mod_pol_sim_std_usa_df <- data.frame(hi_mod_pol_sim_std_usa[[1]])

#Colnames for figures
colnames(hi_mod_pol_sim_fra_df ) <- c('int 1', 'int 2', 'int 3', 'int 4', 'int 5', 
                                'slope 1', 'slope 2', 'slope 3', 'slope 4', 'slope 5',
                                "mean a", 'mean b', 'rho', "sig", "sigma a", 'sigma b')
#Colnames for other things
colnames(hi_mod_pol_sim_std_aus_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                                     'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                                     "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b')
colnames(hi_mod_pol_sim_std_blr_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                                     'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                                     "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b')
colnames(hi_mod_pol_sim_std_fra_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                                         'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                                         "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b')
colnames(hi_mod_pol_sim_std_jpn_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                                         'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                                         "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b')
colnames(hi_mod_pol_sim_std_tur_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                                         'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                                         "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b')
colnames(hi_mod_pol_sim_std_usa_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                                         'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                                         "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b')


#####Plots####
#PLots
color_scheme_set('blue')
#These plots are for the posterior distributions
mcmc_intervals(hi_mod_pol_sim_std_fra_df2, pars = 
                       c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                         'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5'),
               prob = 0.8,
               prob_outer = 0.95,
               point_est = 'mean')
#plot the actual densities
mcmc_areas(hi_mod_pol_sim_ausstd_df, pars = 
                   c("sig", "mu.a", "sigma.a", "mu.b", "sigma.b", "rho", "B[1,1]", "B[2,1]", "B[3,1]", "B[4,1]", "B[5,1]", "B[1,2]", "B[2,2]", "B[3,2]", "B[4,2]", "B[5,2]"),
           prob = 0.8,
           prob_outer = 0.95,
           point_est = 'mean') #Not great with my results
#plot the histograms (use mcmc_hist or mcmc_dens for a density plot)
mcmc_dens(hi_mod_pol_sim_ausstd_df, pars = 
                  c('int 1', 'int 2', 'int 3', 'int 4', 'int 5', 
                    'slope 1', 'slope 2', 'slope 3', 'slope 4', 'slope 5'),
          prob = 0.8,
          prob_outer = 0.95,
          point_est = 'mean') #this one works well to show the distributions


#These are trace plots
color_scheme_set('viridis')
mcmc_trace(hi_mod_pol_sim_std_usa_df, pars = 
                   c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 
                     'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5',
                     "mean.a", 'mean.b', 'rho', "sig", "sigma.a", 'sigma.b'))

#plot the model fit lines
#Set up data 
pol_plot_data <- summarise_all(cor_pol_ris_df, mean)
names(pol_plot_data)  <- c('int 1', 'int 2', 'int 3', 'int 4', 'int 5', 
                           'slope 1', 'slope 2', 'slope 3', 'slope 4', 'slope 5',
                           "mean a", 'mean b', 'rho', "sig", "sigma a", 'sigma b')
pol_plot_data <- melt(pol_plot_data)
pol_plot_data <- data.frame(int = filter(pol_plot_data, str_detect(variable, 'int')), 
                            slope = filter(pol_plot_data, str_detect(variable, 'slope')))
pol_plot_data <- pol_plot_data[,c(2,4)]
pol_plot_data$region <- c(1:5)

colnames(pol_plot_data) <- c('int', 'slope', 'Loc')
test <- rbind(melt(ent_eff_combpol[,c(4,8,9)]), melt(pol_plot_data))

ggplot(data = test, aes(x = test[1:20,3], y = test[21:40,3])) +
        geom_point(size =3) +
        geom_abline(intercept = test[41:45,3], slope = test[46:50,3]) 


#Plot points and lines as same colour 
ggplot() +
        geom_point(data = ent_eff_combpol, aes(x = ent5, y = Eff, group = Loc, colour = Loc), size =3) +
        geom_abline(data = pol_plot_data, aes(intercept = int, slope = slope, group = Loc, colour =Loc))

#plotting all draws of posterior
ggplot() +
        geom_abline(data = pol_plot_data_model1, aes(intercept = int_1, slope = slope_1, colour = '#3366FF'))

names(final_pol_ris_df) <- c('int.1', 'int.2', 'int.3', 'int.4', 'int.5', 'slope.1', 'slope.2', 'slope.3', 'slope.4', 'slope.5', 'mu.a', 'mu.b', 'sig', 'sigma.a', 'sigma.b')
mean(final_pol_ris_df[,6] > 0)
mean(final_pol_ris_df[,7] > 0)
mean(final_pol_ris_df[,8] > 0)
mean(final_pol_ris_df[,9] > 0)
mean(final_pol_ris_df[,10] > 0)

#Plot all int together
plot_data_int <- cor_6x6_ris_df[,1:9]
plot_data_int <- cbind(plot_data_int, cor_pol_ris_df[,1:5])
plot_data_int <- cbind(plot_data_int, cor_4x4_ris_df[,1:4])
names(plot_data_int) <- c('1 6x6', '2 6x6', '3 6x6', '4 6x6', '5 6x6', '6 6x6', '7 6x6', '8 6x6', '9 6x6','1 pol', '2 pol', '3 pol', '4 pol', '5 pol', '1 4x4', '2 4x4', '3 4x4', '4 4x4' )
#Plot all slopes together
plot_data_slope <- cbind(hi_mod_pol_sim_std_aus_df[,6:10], hi_mod_pol_sim_std_blr_df[,6:10],hi_mod_pol_sim_std_fra_df[,6:10], 
                         hi_mod_pol_sim_std_jpn_df[,6:10], hi_mod_pol_sim_std_tur_df[,6:10], hi_mod_pol_sim_std_usa_df[,6:10])

plot_data_slope_tidy <- gather(plot_data_slope)
plot_data_slope_tidy$team <- as.factor(c(rep("AUS", 10000), rep("BLR", 10000), rep("FRA", 10000),
                                         rep("JPN", 10000), rep("TUR", 10000), rep("USA", 10000)))
plot_data_slope_tidy$key <- as.factor(plot_data_slope_tidy$key)



mcmc_areas(plot_data_slope,
           prob = 0.8,
           prob_outer = 0.95,
           point_est = 'mean')

#ggplot attempt
aus_dens_gplot <- ggplot(plot_data_slope_tidy[1:10000,], aes(x=value, colour=key)) + 
        geom_density()
blr_dens_gplot <- ggplot(plot_data_slope_tidy[10001:20000,], aes(x=value, fill=key)) + 
        geom_density(alpha=.3)
fra_dens_gplot <- ggplot(plot_data_slope_tidy[20001:30000,], aes(x=value, fill=key)) + 
        geom_density(alpha=.3)
jpn_dens_gplot <- ggplot(plot_data_slope_tidy[30001:40000,], aes(x=value, fill=key)) + 
        geom_density(alpha=.3)
tur_dens_gplot <- ggplot(plot_data_slope_tidy[40001:50000,], aes(x=value, fill=key)) + 
        geom_density(alpha=.3)
usa_dens_gplot <- ggplot(plot_data_slope_tidy[50001:60000,], aes(x=value, fill=key)) + 
        geom_density(alpha=.3)

slope_grid <- grid.arrange(aus_dens_gplot, blr_dens_gplot, fra_dens_gplot, 
                           jpn_dens_gplot, tur_dens_gplot, usa_dens_gplot, ncol = 2, nrow = 3)

#Boxplot version with raw points plotted
comb_box_raw_slope <- ggplot(plot_data_slope_tidy, aes(x = reorder(key, desc(key)), y = value)) +
        geom_point(alpha=0.3, position = "jitter", aes(colour = team)) +
        geom_boxplot(alpha = 0) + 
        geom_hline(yintercept = 0, linetype="dotted", size = 0.5) +
        coord_flip() +
        scale_color_brewer(palette ="Dark2", name = 'Team') + 
        theme_minimal(base_size = 12, base_family = "Helvetica") +
        
        
        #Save plot with specific dims      
        ggsave(file = 'comb_box_raw_slope.tiff', comb_box_raw_slope, device = 'tiff', width = 20, height = 15, units = 'cm', dpi = 300)


#Density plots in ggplot overlapping with colour
ggplot(plot_data_slope_tidy, aes(x = value, y = key, fill = team)) + 
        geom_density_ridges2(rel_min_height = 0.001, alpha = 0.4, scale = 1.1) + 
        theme_ridges() + 
        scale_fill_brewer(palette ="Dark2")

#Plot all raw ent and eff data with team colours and the model lines coloured by team. 
ent_eff_comb <- cbind(all_teams_ent_tidy$value, gather(all_teams_stdeff), gather(all_teams_eff))
ent_eff_comb <- ent_eff_comb[,-4]
names(ent_eff_comb) <- c('ent', 'team', 'std_eff', 'eff')
all_teams_eff <- tibble(aus = aus_eff_table$Eff, blr = blr_eff_table$Eff, fra = fra_eff_table$Eff, 
                        jpn = jpn_eff_table$Eff, tur = tur_eff_table$Eff, usa = usa_eff_table$Eff)
#Get the mean lm lines
aus_mean_lm_lines <- hi_mod_pol_sim_std_aus_df[1:10] %>% 
        summarise_all(mean) %>%
        gather() %>%
        add_column(team = "AUS")
blr_mean_lm_lines <- hi_mod_pol_sim_std_blr_df[1:10] %>% 
        summarise_all(mean) %>%
        gather() %>%
        add_column(team = "BLR")
fra_mean_lm_lines <- hi_mod_pol_sim_std_fra_df[1:10] %>% 
        summarise_all(mean) %>%
        gather() %>%
        add_column(team = "FRA")
jpn_mean_lm_lines <- hi_mod_pol_sim_std_jpn_df[1:10] %>% 
        summarise_all(mean) %>%
        gather() %>%
        add_column(team = "JPN")
tur_mean_lm_lines <- hi_mod_pol_sim_std_tur_df[1:10] %>% 
        summarise_all(mean) %>%
        gather() %>%
        add_column(team = "TUR")
usa_mean_lm_lines <- hi_mod_pol_sim_std_usa_df[1:10] %>% 
        summarise_all(mean) %>%
        gather() %>%
        add_column(team = "USA")

pol_plot_data <- rbind(aus_mean_lm_lines, blr_mean_lm_lines, fra_mean_lm_lines,
                       jpn_mean_lm_lines, tur_mean_lm_lines, usa_mean_lm_lines)
raw_mean_plot_data <- data.frame(int = filter(pol_plot_data, str_detect(key, 'int')), 
                                 slope = filter(pol_plot_data, str_detect(key, 'slope')))
raw_mean_plot_data <- raw_mean_plot_data[,c(2,5,6)]


ggplot(ent_eff_comb, aes(x = ent, y = std_eff, group = team, colour = team)) +
        geom_point(size = 3) +
        
        #Plot points and lines as same colour - TEAM
        ggplot() +
        geom_point(data = ent_eff_comb, aes(x = ent, y = std_eff, group = team, colour = team), size =3, alpha = 0.4) +
        geom_abline(data = raw_mean_plot_data, aes(intercept = int.value, slope = slope.value, group = slope.team, colour = slope.team)) + 
        scale_color_brewer(palette ="Dark2") +
        theme_minimal()
#Plot points and lines as same colour - LOCATION
ggplot() +
        geom_point(data = ent_eff_comb, aes(x = ent, y = std_eff, group = Loc, colour = Loc), size =3, alpha = 0.4) +
        geom_abline(data = raw_mean_plot_data, aes(intercept = int.value, slope = slope.value, group = slope.team, colour = Loc)) + 
        scale_color_brewer(palette ="Dark2") +
        theme_minimal()

#plot heatmap 
#ggplot needs x and y coords for cells
heat_map_test <- data.frame(ent = aus_ent_eff$aus_ent,
                            x = as.factor(c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,3,4)),
                            y = as.factor(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4)))

ggplot(heat_map_test, aes(x = x, y = y)) +
        geom_tile(aes(fill = ent))+
        scale_fill_gradient(low = 'white', high = 'steelblue')
#OR
ggplot(heat_map_test, aes(x = x, y = y)) +
        geom_tile(aes(fill = ent))+
        scale_color_brewer(palette ="PuBuGn")

####Plotting raw trajectories on the court for figure ####
#need to start by drawing the court


