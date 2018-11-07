library(gplots)
library(RColorBrewer)
library(plyr)
library(data.table)
library(tidyverse)
library(rlist)
library(entropy)
library(beepr)

####Set data up ####
all_data <- readRDS('all_data')
aus <- filter(all_data, TeamName == "AUS")
blr <- filter(all_data, TeamName == "BLR")
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


####P score code####
#pol
p0_fun_pol = function(game){
        data= game
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
                        write.table(short_df, "delete")
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
                                        pred_tmp$curstates[i] = 3 #right side of under basket
                                } else if (pred_tmp$y[i] < 21 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <= 31) {
                                        pred_tmp$curstates[i] = 20 #Left side of under basket
                                } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] >= 37 & pred_tmp$x[i] <=50) {
                                        pred_tmp$curstates[i] = 4
                                } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] < 56 & pred_tmp$x[i] > 50) {
                                        pred_tmp$curstates[i] = 5
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 19 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {   #dis > 20 as the 3point line ranges from 19.2 to 20.55 in this section so an average was taken
                                        pred_tmp$curstates[i] = 6  
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] <=25 & pred_tmp$x[i] >= 12 & pred_tmp$dis[i] <= 20 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {
                                        pred_tmp$curstates[i] = 7
                                } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 31 & pred_tmp$x[i] < 37) {
                                        pred_tmp$curstates[i] = 8 #Right side of the mid key
                                } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <=31) {
                                        pred_tmp$curstates[i] = 21 #Left side of the mid key
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] >=37 & pred_tmp$x[i] < 51 & pred_tmp$dis[i] <= 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) {
                                        pred_tmp$curstates[i] = 9
                                } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) { 
                                        pred_tmp$curstates[i] = 10 
                                } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 11
                                } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] < 26 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 12
                                } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] > 36 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 14
                                } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 15
                                } else if (pred_tmp$y[i] < 59 & pred_tmp$x[i] < 32 & pred_tmp$dis[i]  > 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 16
                                } else if (pred_tmp$y[i] < 59 & pred_tmp$y[i] > 31 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  > 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 17
                                } else if (pred_tmp$y[i] >= 59 & pred_tmp$y[i] <= 106 & pred_tmp$x[i] <=56 & pred_tmp$x[i] >= 6) {
                                        pred_tmp$curstates[i] = 18  
                                } else if (pred_tmp$y[i] > 106 | pred_tmp$y[i] < 12 | pred_tmp$x[i] < 6 | pred_tmp$x[i] > 56) {
                                        pred_tmp$curstates[i] = 19
                                } else if (pred_tmp$y[i] > 30 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                        pred_tmp$curstates[i] = 13 #Right elbow
                                } else if (pred_tmp$y[i] >30 & pred_tmp$x[i] <= 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                        pred_tmp$curstates[i] = 22} #Left elbow
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
        return(p_table)
}


####Ent function####
ent_pol_fun_5 <- function(game){
data = usa_shot
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
                                pred_tmp$curstates[i] = 3 #right side of under basket
                        } else if (pred_tmp$y[i] < 21 & pred_tmp$y[i] >= 12 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <= 31) {
                                pred_tmp$curstates[i] = 20 #Left side of under basket
                        } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] >= 37 & pred_tmp$x[i] <=50) {
                                pred_tmp$curstates[i] = 4
                        } else if (pred_tmp$y[i] >= 12 & pred_tmp$y[i] < 21 & pred_tmp$x[i] < 56 & pred_tmp$x[i] > 50) {
                                pred_tmp$curstates[i] = 5
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 19 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {   #dis > 20 as the 3point line ranges from 19.2 to 20.55 in this section so an average was taken
                                pred_tmp$curstates[i] = 6  
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] <=25 & pred_tmp$x[i] >= 12 & pred_tmp$dis[i] <= 20 & pred_tmp$radml[i] < cart2pol_midlineL(6,31)[2]) {
                                pred_tmp$curstates[i] = 7
                        } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 31 & pred_tmp$x[i] < 37) {
                                pred_tmp$curstates[i] = 8 #Right side of the mid key
                        } else if (pred_tmp$y[i] < 31 & pred_tmp$y[i] > 20 & pred_tmp$x[i] > 25 & pred_tmp$x[i] <=31) {
                                pred_tmp$curstates[i] = 21 #Left side of the mid key
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 29 & pred_tmp$x[i] >=37 & pred_tmp$x[i] < 51 & pred_tmp$dis[i] <= 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) {
                                pred_tmp$curstates[i] = 9
                        } else if (pred_tmp$y[i] > 20 & pred_tmp$y[i] < 32 & pred_tmp$dis[i]  > 20 & pred_tmp$radmr[i] > cart2pol_midlineR(56,31)[2]) { 
                                pred_tmp$curstates[i] = 10 
                        } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 11
                        } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] < 26 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radml[i] > cart2pol_midlineL(6,31)[2] & pred_tmp$radhl[i] < cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 12
                        } else if (pred_tmp$y[i] > 24 & pred_tmp$x[i] > 36 & pred_tmp$dis[i] <= 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 14
                        } else if (pred_tmp$y[i] > 28 & pred_tmp$dis[i]  > 20.5 & pred_tmp$radmr[i] < cart2pol_midlineR(56,31)[2] & pred_tmp$radhr[i] > cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 15
                        } else if (pred_tmp$y[i] < 59 & pred_tmp$x[i] < 32 & pred_tmp$dis[i]  > 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 16
                        } else if (pred_tmp$y[i] < 59 & pred_tmp$y[i] > 31 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  > 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 17
                        } else if (pred_tmp$y[i] >= 59 & pred_tmp$y[i] <= 106 & pred_tmp$x[i] <=56 & pred_tmp$x[i] >= 6) {
                                pred_tmp$curstates[i] = 18  
                        } else if (pred_tmp$y[i] > 106 | pred_tmp$y[i] < 12 | pred_tmp$x[i] < 6 | pred_tmp$x[i] > 56) {
                                pred_tmp$curstates[i] = 19
                        } else if (pred_tmp$y[i] > 30 & pred_tmp$x[i] > 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhr[i] < cart2pol_highR(56,47)[2]) {
                                pred_tmp$curstates[i] = 13 #Right elbow
                        } else if (pred_tmp$y[i] >30 & pred_tmp$x[i] <= 31 & pred_tmp$dis[i]  < 21 & pred_tmp$radhl[i] > cart2pol_highL(6,47)[2]) {
                                pred_tmp$curstates[i] = 22} #Left elbow
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
        return(shan_ent)
}

####Entropy extraction####
aus_ent <- ent_pol_fun_5(aus_shot)
blr_ent <- ent_pol_fun_5(blr_shot)
fra_ent <- ent_pol_fun_5(fra_shot)
jpn_ent <- ent_pol_fun_5(jpn_shot)
tur_ent <- ent_pol_fun_5(tur_shot)
usa_ent <- ent_pol_fun_5(usa_shot)
beep(sound = 1, expr = NULL)

aus_ent_rio <- ent_pol_fun_5(aus_rio)

all_teams_ent <- tibble(aus_ent, blr_ent, fra_ent, jpn_ent, tur_ent, usa_ent)
all_teams_ent <- all_teams_ent[c(1:17, 20, 21, 22),]
all_teams_ent <- rbind(all_teams_ent, summarise_all(all_teams_ent, mean))

#Count number of shots in dataset
s <- str_count(aus_$Label1, 'Shot')
length(s[s == 1])

#Remove a game from dataset
aus <- aus %>% filter(!(GameID %in% 'AUS v USA Q1' | GameID %in% 'AUS v USA Q2' | GameID %in% 'AUS v USA Q3' |GameID %in% 'AUS v USA Q4'))
#Also have code to get entropy by shot clock - ie entropy in the first 8 seconds, middle 8 and last 8. 
#Decide if its going to be in the study, if not dont waste time doing it. 

