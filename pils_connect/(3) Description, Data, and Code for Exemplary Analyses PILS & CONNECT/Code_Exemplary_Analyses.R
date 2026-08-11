############################################################################################
##                                                                                        ##
## This file contains the R code that we used to analyze data for:                        ##
## Geukes, K., Breil, S.M., Hutteman, R., Nestler, S., Küfner, C. P., & Back, M.B. (2018).##
## Explaining the Longitudinal Interplay of Personality and Social Relationships in the   ##
## Laboratory and in the Field: The PILS and CONNECT Study                                ##
## Katharina Geukes (katharina.geukes@uni-muenster.de)                                    ##
##                                                                                        ##
############################################################################################

## GETTING STARTED

### load necessary packages
#install.packages("lme4")
library(lme4) #for multilevel models
#install.packages("lmerTest")
library(lmerTest) #for multilevel models significane testing
#install.packages("haven")
library(haven) #import of spss rawdata
#install.packages("dplyr")
library(dplyr) #data structuring
#install.packages("RSA")
library (RSA) # response surface analysis
#install.packages("reshape2")
library(reshape2) #data structuring
#install.packages("statnet")
library(statnet)# network models

setwd("") #please add your own working directory here

#read relevant datasets PILS
Pils_Traits <- read.table("pils_onlinesurvey_self.txt",sep=";",dec=".",header =T)
Pils_Session_Long <- read.table("pils_sessiondata_long.txt",sep=";",dec=".",header =T)

#read relevant datasets CONNECT
Connect_Traits_t1 <- read.table("connect_onlinesurvey_self.txt",sep=";",dec=".",header =T)
Connect_Diary_Long<- read.table("connect_timebasedassessment_long.txt",sep=";",dec=".",header =T)
Connect_App_Long<- read.table("connect_eventbasedassessment_long.txt",sep=";",dec=".",header =T)

#Order Variables by id_a
Pils_Traits <- Pils_Traits [order(Pils_Traits$id_a),]
Pils_Session_Long <- Pils_Session_Long [order(Pils_Session_Long$id_a),]
Connect_Traits_t1 <- Connect_Traits_t1 [order(Connect_Traits_t1$id_a),]
Connect_Diary_Long <- Connect_Diary_Long [order(Connect_Diary_Long$id_a),]
Connect_App_Long <- Connect_App_Long [order(Connect_App_Long$id_a),]

############################################################################################
## 1 Individual-level research question                                                   ##
############################################################################################


###PILS###
#we are interested in other-reports (do others see me as a leader) so we only select cases if id_a is not equal to id_p. 
Pils_Session_Other <- as.data.frame(subset(Pils_Session_Long, Pils_Session_Long$id_a != Pils_Session_Long$id_p))

#select relevant variables and choose timepoint 1 to 10
Pils_Session_Other <- as.data.frame(select(Pils_Session_Other, id_p, leader,timepoint, group_number))
Pils_Session_Other <- as.data.frame(subset(Pils_Session_Other, Pils_Session_Other$timepoint < 11))

#aggregate per id_p (the person being judged), per timepoint and per groupnumber
Pils_Session_Other_agg <- aggregate(Pils_Session_Other, by=list(Pils_Session_Other$id_p,Pils_Session_Other$timepoint,Pils_Session_Other$group_number),FUN=mean, na.rm=TRUE)

#select relevant variables
Pils_Session_Other_agg <- select(Pils_Session_Other_agg, id_p, timepoint,leader,group_number)

#rename variables for matching. To match the ids with the trais we need to rename id_p to id_a
names(Pils_Session_Other_agg)[names(Pils_Session_Other_agg)=="id_p"] <-"id_a"

#z-standardize Shyness
Pils_Traits$Zshy_gen <-scale (Pils_Traits$shy_gen)

#merge Leader data with shyness
Pils_leadership <- merge (Pils_Session_Other_agg,as.data.frame(select(Pils_Traits, id_a, Zshy_gen)), by ="id_a" )

#recode timepoints so the first timepoint is coded as zero
Pils_leadership$timepoint [Pils_leadership$timepoint == 1] <- 0
Pils_leadership$timepoint [Pils_leadership$timepoint == 2] <- 1
Pils_leadership$timepoint [Pils_leadership$timepoint == 3] <- 2
Pils_leadership$timepoint [Pils_leadership$timepoint == 4] <- 3
Pils_leadership$timepoint [Pils_leadership$timepoint == 5] <- 4
Pils_leadership$timepoint [Pils_leadership$timepoint == 6] <- 5
Pils_leadership$timepoint [Pils_leadership$timepoint == 7] <- 6
Pils_leadership$timepoint [Pils_leadership$timepoint == 8] <- 7
Pils_leadership$timepoint [Pils_leadership$timepoint == 9] <- 8
Pils_leadership$timepoint [Pils_leadership$timepoint == 10] <- 9

#Multilevel model
leader_p <- lmer(leader~1+Zshy_gen*timepoint+(timepoint|id_a), data = Pils_leadership)
summary (leader_p)

#Plot the results
#Subset Individuals with high shyness
Pils_HighShy <- subset(Pils_leadership, Pils_leadership$Zshy_gen > 0)
#Subset Individuals with Low Shyness
Pils_LowShy <- subset(Pils_leadership, Pils_leadership$Zshy_gen <= 0)
#getrid of missing data
Pils_HighShy <- na.omit(Pils_HighShy)
Pils_LowShy <- na.omit(Pils_LowShy)
#get slopes for individuals with High Shyness
Pils_HighShySlopes <- as.data.frame(coef(lmList(leader~1+timepoint|id_a, data = Pils_HighShy)))
Pils_HighShySlopes
#get slopes for individuals with Low Shyness
Pils_LowShySlopes <- as.data.frame(coef(lmList(leader~1+timepoint|id_a, data = Pils_LowShy)))
Pils_LowShySlopes
#These individuals slopes were used for the plot which was done in excel.


###CONNECT###
#we are interested in other reports (do other's see me as a leader) so we only select cases if id_a is not equal to id_p. 
Connect_Diary_Other <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a != Connect_Diary_Long$id_p))

#select relevant variables
Connect_Diary_Other <- as.data.frame(select(Connect_Diary_Other,id_p,leader,diary_nr_total))

#aggregate per id_p and per diarynumber
Connect_Diary_Other_agg <- aggregate(Connect_Diary_Other, by=list(Connect_Diary_Other$id_p,Connect_Diary_Other$diary_nr_total),FUN=mean, na.rm=TRUE)

#select relevant variables
Connect_Diary_Other_agg <- select(Connect_Diary_Other_agg, id_p, diary_nr_total,leader)

#rename variables for matching. To match the ids with the trais we need to rename id_p to id_a
names(Connect_Diary_Other_agg)[names(Connect_Diary_Other_agg)=="id_p"] <-"id_a"

#z-standardize shyness
Connect_Traits_t1$Zshy_gen <-scale (Connect_Traits_t1$shy_gen_t1)

#merge Leader data with shyness
Connect_leader <- merge (Connect_Diary_Other_agg,as.data.frame(select(Connect_Traits_t1, id_a, Zshy_gen)), by ="id_a" )

#Select relevant diaries in which leader was assessed
Connect_leader2 <- (subset(Connect_leader, Connect_leader$diary_nr_total == 7 | Connect_leader$diary_nr_total == 11 | Connect_leader$diary_nr_total == 13 | Connect_leader$diary_nr_total == 15 | Connect_leader$diary_nr_total == 17 | Connect_leader$diary_nr_total == 19 | Connect_leader$diary_nr_total == 21  ))

#renumber diaries (one up for every two weeks (except between 5 & 6 because Christmas))
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 7] <- 0
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 11] <- 1
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 13] <- 2
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 15] <- 3
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 17] <- 4
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 19] <- 5
Connect_leader2$diary_nr_total [Connect_leader2$diary_nr_total == 21] <- 6

#Multilevel model
leader_c <- lmer(leader~1+Zshy_gen*diary_nr_total+(diary_nr_total|id_a), data = Connect_leader2)
summary (leader_c)

#Plot
#Subset Individuals with high shyness
Connect_HighShy <- subset(Connect_leader2, Connect_leader2$Zshy_gen > 0)
#Subset Individuals with Low Shyness
Connect_LowShy <- subset(Connect_leader2, Connect_leader2$Zshy_gen <= 0)
#getrid of missing data
Connect_HighShy <- na.omit(Connect_HighShy)
Connect_LowShy <- na.omit(Connect_LowShy)
#get slopes for individuals with High Shyness
Connect_HighShySlopes <- as.data.frame(coef(lmList(leader~1+diary_nr_total|id_a, data = Connect_HighShy)))
Connect_HighShySlopes
#get slopes for individuals with Low Shyness
Connect_LowShySlopes <- as.data.frame(coef(lmList(leader~1+diary_nr_total|id_a, data = Connect_LowShy)))
Connect_LowShySlopes
#These individuals slopes were used for the plot which was done in excel.


############################################################################################
## 2 Dyadic-level research question                                                       ##
############################################################################################

###PILS###
#Select timepoint 1 from session data
Timepoint1<- subset(Pils_Session_Long, Pils_Session_Long$timepoint == 1 )
#select other ratings
Pils_Session_Other <- as.data.frame(subset(Timepoint1, Timepoint1$id_a != Timepoint1$id_p))
#rename relevant variable
names(Pils_Session_Other)[names(Pils_Session_Other)=="assertiveness"] <-"perceived_assertiveness"
#select self-ratings ratings
Pils_Session_Self <- as.data.frame(subset(Timepoint1, Timepoint1$id_a == Timepoint1$id_p))
#rename relevant variable
names(Pils_Session_Self)[names(Pils_Session_Self)=="assertiveness"] <-"self_reported_assertiveness"
#merge datsets: self data to other data
T1 <- merge (as.data.frame(Pils_Session_Other),as.data.frame(select(Pils_Session_Self, id_a, self_reported_assertiveness, timepoint)), by ="id_a"  )
#RSA at T1
RSAT1 <- RSA(liking ~ self_reported_assertiveness*perceived_assertiveness, data = T1,na.rm=FALSE,missing="listwise"  )
plot(RSAT1,legend = FALSE,param = FALSE ,xlab = "self-reported assertiveness", ylab ="perceived assertiveness of others", project=c("PA1"), pal=colorRampPalette(c("hotpink4","hotpink","deepskyblue","blue4"))(30))
aictab(RSAT1) #model comparison
summary(RSAT1)#relevant parameters


###CONNECT###
#recode relevant variables
Connect_App_Long$IR_positive_negative <- 8 - Connect_App_Long$IR_positive_negative
#renamce variables
names(Connect_App_Long)[names(Connect_App_Long)=="OR_dominant_submissive"] <-"perceived_dominance"
names(Connect_App_Long)[names(Connect_App_Long)=="SR_dominant_submissive"] <-"self_reported_dominance"
names(Connect_App_Long)[names(Connect_App_Long)=="IR_positive_negative"] <-"positivity"
#set phase 1
Connect_App_Long_Phase1 <- as.data.frame(subset(Connect_App_Long, day > 2 & day <24))
#RSA at Phase 1
RSAPhase1 <- RSA(positivity ~ self_reported_dominance*perceived_dominance, data = Connect_App_Long_Phase1,na.rm=TRUE,missing="listwise" )
plot(RSAPhase1,legend = FALSE,param = FALSE, xlab = "self-reported dominance", ylab ="perceived dominance of others",project=c("PA1", "LOC"),pal=colorRampPalette(c("hotpink4","hotpink","deepskyblue","blue4"))(30), axes=c("LOC","LOIC","PA1","PA2"))
aictab(RSAPhase1) #model comparison
summary(RSAPhase1) #relevant parameters
getPar(RSAPhase1, model ="SRRR","coef") #relevant parameters


############################################################################################
## 3 Network-level research question                                                      ##
############################################################################################


###PILS###
#select only other-ratings
Pils_Session_Other <- as.data.frame(subset(Pils_Session_Long, Pils_Session_Long$id_a != Pils_Session_Long$id_p))
#select relevant variables
Pils_session <- as.data.frame(select(Pils_Session_Other, id_a, id_p, friend, timepoint, group_number))
#delete missings and 7
Pils_session <- (subset(Pils_session, Pils_session$friend<7))
#make friendship selection binary. 5 & 6 = friend
Pils_session$friend [Pils_session$friend == 1] <- 0
Pils_session$friend [Pils_session$friend == 2] <- 0
Pils_session$friend [Pils_session$friend == 3] <- 0
Pils_session$friend [Pils_session$friend == 4] <- 0
Pils_session$friend [Pils_session$friend == 5] <- 1
Pils_session$friend [Pils_session$friend == 6] <- 1
#subset relevant timepoints
Pils_session_T1 <- subset(Pils_session, Pils_session$timepoint==1)   
Pils_session_T10 <- subset(Pils_session, Pils_session$timepoint==10) 

#Network statistics 
#number of mutal and asymmetrical dyads & transitivity

#T1
#restructure data
Pils_session_T1_allgroups_wide <- dcast(Pils_session_T1[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Pils_session_T1_allgroups_wide <- as.matrix(Pils_session_T1_allgroups_wide[,-1])
#define network model
Networkmodel_T1 <- as.network (x = Pils_session_T1_allgroups_wide, directed = T, loops = F, matrix.type = "adjacency")
#give out statistics
dyad.census(Networkmodel_T1) #number of mutal and asymmetrical dyads
gtrans(Networkmodel_T1,measure="weak", use.adjacency=FALSE) # transitivity

#T10
#delete IDs that received but did not provide ratings
Pils_session_T10_allgroups_wide <- subset(Pils_session_T10, Pils_session_T10$id_p!=1196 & Pils_session_T10$id_p!=1180 & Pils_session_T10$id_p!=1289 & Pils_session_T10$id_p!=1218 & Pils_session_T10$id_p!=1104 & Pils_session_T10$id_p!=1294)
#restructure data
Pils_session_T10_allgroups_wide <- dcast(Pils_session_T10_allgroups_wide[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Pils_session_T10_allgroups_wide <- as.matrix(Pils_session_T10_allgroups_wide[,-1])
#define network model
Networkmodel_T10 <- as.network (x = Pils_session_T10_allgroups_wide, directed = T, loops = F, matrix.type = "adjacency")
#Networkstatistics
dyad.census(Networkmodel_T10) #number of mutal and asymmetrical dyads
gtrans(Networkmodel_T10,measure="weak", use.adjacency=FALSE) # transitivity

#Network statistics 
#Extraversion & Indegrees

#T1
#select relevant data
Pils_session_T1_indegree <- select (Pils_session_T1, id_a, id_p, friend)
#aggregate being chosen as a friend per id_p
Pils_session_T1_indegree <- aggregate (Pils_session_T1_indegree, by = list(Pils_session_T1_indegree$id_p), FUN = mean, na.rm =T)
#select relevant data
Pils_session_T1_indegree <- select (Pils_session_T1_indegree, id_p, friend)
#rename id_p to id_a for matching with personality
names(Pils_session_T1_indegree)[names(Pils_session_T1_indegree)=="id_p"] <-"id_a"
#merge with personality
Pils_session_T1_indegree <- merge(Pils_session_T1_indegree, Pils_Traits, by = "id_a")
#calculate correlation
cor.test(Pils_session_T1_indegree$friend,Pils_session_T1_indegree$big5_e )

#T10
#select relevant data
Pils_session_T10_indegree <- select (Pils_session_T10, id_a, id_p, friend)
#aggregate being chosen as a friend per id_p
Pils_session_T10_indegree <- aggregate (Pils_session_T10_indegree, by = list(Pils_session_T10_indegree$id_p), FUN = mean, na.rm =T)
#select relevant data
Pils_session_T10_indegree <- select (Pils_session_T10_indegree, id_p, friend)
#rename id_p to id_a for matching with personality
names(Pils_session_T10_indegree)[names(Pils_session_T10_indegree)=="id_p"] <-"id_a"
#merge with personality
Pils_session_T10_indegree <- merge(Pils_session_T10_indegree, Pils_Traits, by = "id_a")
#calculate correlation
cor.test(Pils_session_T10_indegree$friend,Pils_session_T10_indegree$big5_e )

#Network statistics 
#Extraversion & Outdegrees

#T1
#select relevant data
Pils_session_T1_outdegree <- select (Pils_session_T1, id_a, friend)
#aggregate choose a friend per id_a
Pils_session_T1_outdegree <- aggregate (Pils_session_T1_outdegree, by = list(Pils_session_T1_outdegree$id_a), FUN = mean, na.rm =T)
#select relevant data
Pils_session_T1_outdegree <- select (Pils_session_T1_outdegree, id_a, friend)
#merge with personality
Pils_session_T1_outdegree <- merge(Pils_session_T1_outdegree, Pils_Traits, by = "id_a")
#calculate correlation
cor.test(Pils_session_T1_outdegree$friend,Pils_session_T1_outdegree$big5_e )

#T10
#select relevant data
Pils_session_T10_outdegree <- select (Pils_session_T10, id_a, friend)
#aggregate choose a friend per id_a
Pils_session_T10_outdegree <- aggregate (Pils_session_T10_outdegree, by = list(Pils_session_T10_outdegree$id_a), FUN = mean, na.rm =T)
#select relevant data
Pils_session_T10_outdegree <- select (Pils_session_T10_outdegree, id_a, friend)
#merge with personality
Pils_session_T10_outdegree <- merge(Pils_session_T10_outdegree, Pils_Traits, by = "id_a")
#calculate correlation
cor.test(Pils_session_T10_outdegree$friend,Pils_session_T10_outdegree$big5_e )

#Plot Networks
Pils_Traits <- Pils_Traits [order(Pils_Traits$id_a),]
Pils_session <- Pils_session [order(Pils_session$id_a),]


#T1 Group 7#
#set group and timepoint
Pils_session_NW <- subset(Pils_session, Pils_session$timepoint==1 & Pils_session$group_number == 7 )  
#restructure data
Pils_session_NW_wide <- dcast(Pils_session_NW[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
# remove first column (which contains the ids)
Pils_session_NW_wide <- as.matrix(Pils_session_NW_wide[,-1])
#create network model
Networkmodel <- as.network (x = Pils_session_NW_wide, directed = T, loops = F, matrix.type = "adjacency")
#check model
summary(Networkmodel)
#set Ids 1 to 6
network.vertex.names(Networkmodel) <- c(1:6)
#take relevant extraversion scores
extraversion <- as.double(select(subset(Pils_Traits, group_number == 7), big5_e)$big5_e)
#check for missings
extraversion
#set extraversion to networkmodel
set.vertex.attribute(Networkmodel,"extraversion",extraversion)
#take relevant sexes
sex <- as.double(select(subset(Pils_Traits, group_number == 7), sex)$sex)              
#check for missings                      
sex                                                                                    
#set sex to networkmodel
set.vertex.attribute(Networkmodel,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:6){ if(get.node.attr(Networkmodel,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }
#plot network
plot.network(Networkmodel,vertex.col =node_colors,label.cex = 2, vertex.cex = (extraversion^2)/10, displaylabels = T)

#T10 Group 7#
#set group and timepoint
Pils_session_NW <- subset(Pils_session, Pils_session$timepoint==10 & Pils_session$group_number == 7 )  
#restructure data
Pils_session_NW_wide <- dcast(Pils_session_NW[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
# remove first column (which contains the ids)
Pils_session_NW_wide <- as.matrix(Pils_session_NW_wide[,-1])
#create network model
Networkmodel <- as.network (x = Pils_session_NW_wide, directed = T, loops = F, matrix.type = "adjacency")
#check model
summary(Networkmodel)
#set Ids 1 to 6
network.vertex.names(Networkmodel) <- c(1:6)
#take relevant extraversion scores
extraversion <- as.double(select(subset(Pils_Traits, group_number == 7), big5_e)$big5_e)
#check for missings
extraversion
#set extraversion to networkmodel
set.vertex.attribute(Networkmodel,"extraversion",extraversion)
#take relevant sexes
sex <- as.double(select(subset(Pils_Traits, group_number == 7), sex)$sex)              
#check for missings                      
sex                                                                                    
#set sex to networkmodel
set.vertex.attribute(Networkmodel,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:6){ if(get.node.attr(Networkmodel,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }
#plot network
plot.network(Networkmodel,vertex.col =node_colors,label.cex = 2, vertex.cex = (extraversion^2)/10, displaylabels = T)

#T1 Group 37#
#set group and timepoint
Pils_session_NW <- subset(Pils_session, Pils_session$timepoint==1 & Pils_session$group_number == 37 )  
#restructure data
Pils_session_NW_wide <- dcast(Pils_session_NW[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
# remove first column (which contains the ids)
Pils_session_NW_wide <- as.matrix(Pils_session_NW_wide[,-1])
#create network model
Networkmodel <- as.network (x = Pils_session_NW_wide, directed = T, loops = F, matrix.type = "adjacency")
#check model
summary(Networkmodel)
#set Ids 1 to 6
network.vertex.names(Networkmodel) <- c(1:6)
#take relevant extraversion scores
extraversion <- as.double(select(subset(Pils_Traits, group_number == 37), big5_e)$big5_e)
#check for missings
extraversion
#set extraversion to networkmodel
set.vertex.attribute(Networkmodel,"extraversion",extraversion)
#take relevant sexes
sex <- as.double(select(subset(Pils_Traits, group_number == 37), sex)$sex)              
#check for missings                      
sex                                                                                    
#set sex to networkmodel
set.vertex.attribute(Networkmodel,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:6){ if(get.node.attr(Networkmodel,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }
#plot network
plot.network(Networkmodel,vertex.col =node_colors,label.cex = 2, vertex.cex = (extraversion^2)/10, displaylabels = T)

#T10 Group 37#
#set group and timepoint
Pils_session_NW <- subset(Pils_session, Pils_session$timepoint==10 & Pils_session$group_number == 37 )  
#restructure data
Pils_session_NW_wide <- dcast(Pils_session_NW[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
# remove first column (which contains the ids)
Pils_session_NW_wide <- as.matrix(Pils_session_NW_wide[,-1])
#create network model
Networkmodel <- as.network (x = Pils_session_NW_wide, directed = T, loops = F, matrix.type = "adjacency")
#check model
summary(Networkmodel)
#set Ids 1 to 6
network.vertex.names(Networkmodel) <- c(1:6)
#take relevant extraversion scores
extraversion <- as.double(select(subset(Pils_Traits, group_number == 37), big5_e)$big5_e)
#check for missings
extraversion
#set extraversion to networkmodel
set.vertex.attribute(Networkmodel,"extraversion",extraversion)
#take relevant sexes
sex <- as.double(select(subset(Pils_Traits, group_number == 37), sex)$sex)              
#check for missings                      
sex                                                                                    
#set sex to networkmodel
set.vertex.attribute(Networkmodel,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:6){ if(get.node.attr(Networkmodel,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }
#plot network
plot.network(Networkmodel,vertex.col =node_colors,label.cex = 2, vertex.cex = (extraversion^2)/10, displaylabels = T)

#T1 Group 67#
#set group and timepoint
Pils_session_NW <- subset(Pils_session, Pils_session$timepoint==1 & Pils_session$group_number == 67 )  
#restructure data
Pils_session_NW_wide <- dcast(Pils_session_NW[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
# remove first column (which contains the ids)
Pils_session_NW_wide <- as.matrix(Pils_session_NW_wide[,-1])
#create network model
Networkmodel <- as.network (x = Pils_session_NW_wide, directed = T, loops = F, matrix.type = "adjacency")
#check model
summary(Networkmodel)
#set Ids 1 to 6
network.vertex.names(Networkmodel) <- c(1:6)
#take relevant extraversion scores
extraversion <- as.double(select(subset(Pils_Traits, group_number == 67), big5_e)$big5_e)
#check for missings
extraversion
#set extraversion to networkmodel
set.vertex.attribute(Networkmodel,"extraversion",extraversion)
#take relevant sexes
sex <- as.double(select(subset(Pils_Traits, group_number == 67), sex)$sex)              
#check for missings                      
sex                                                                                    
#set sex to networkmodel
set.vertex.attribute(Networkmodel,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:6){ if(get.node.attr(Networkmodel,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }
#plot network
plot.network(Networkmodel,vertex.col =node_colors,label.cex = 2, vertex.cex = (extraversion^2)/10, displaylabels = T)

#T10 Group 67#
#set group and timepoint
Pils_session_NW <- subset(Pils_session, Pils_session$timepoint==10 & Pils_session$group_number == 67 )  
#restructure data
Pils_session_NW_wide <- dcast(Pils_session_NW[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
# remove first column (which contains the ids)
Pils_session_NW_wide <- as.matrix(Pils_session_NW_wide[,-1])
#create network model
Networkmodel <- as.network (x = Pils_session_NW_wide, directed = T, loops = F, matrix.type = "adjacency")
#check model
summary(Networkmodel)
#set Ids 1 to 6
network.vertex.names(Networkmodel) <- c(1:6)
#take relevant extraversion scores
extraversion <- as.double(select(subset(Pils_Traits, group_number == 67), big5_e)$big5_e)
#check for missings
extraversion
#set extraversion to networkmodel
set.vertex.attribute(Networkmodel,"extraversion",extraversion)
#take relevant sexes
sex <- as.double(select(subset(Pils_Traits, group_number == 67), sex)$sex)              
#check for missings                      
sex                                                                                    
#set sex to networkmodel
set.vertex.attribute(Networkmodel,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:6){ if(get.node.attr(Networkmodel,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }
#plot network
plot.network(Networkmodel,vertex.col =node_colors,label.cex = 2, vertex.cex = (extraversion^2)/10, displaylabels = T)


###CONNECT###
#select only other-ratings
Connect_Diary_other <- as.data.frame(subset(Connect_Diary_Long, Connect_Diary_Long$id_a != Connect_Diary_Long$id_p))
#delete dropouts
Connect_Diary_other <- as.data.frame(subset(Connect_Diary_other, Connect_Diary_other$id_p != 556 & Connect_Diary_other$id_p != 619 & Connect_Diary_other$id_p != 593 & Connect_Diary_other$id_p != 513 & Connect_Diary_other$id_p != 562))
#select relevant variables
Connect_Diary_other <- as.data.frame(select(Connect_Diary_other, id_a, id_p, friend,diary_nr_total))
#set NAs in friend to zero -> not named as friend
Connect_Diary_other$friend [Connect_Diary_other$friend == "NaN"] <-0
#subset relevant timepoints
Connect_Diary_7 <- subset(Connect_Diary_other, Connect_Diary_other$diary_nr_total==7)   
Connect_Diary_15 <- subset(Connect_Diary_other, Connect_Diary_other$diary_nr_total==15) 
Connect_Diary_21 <- subset(Connect_Diary_other, Connect_Diary_other$diary_nr_total==21)

#Network statistics 
#number of mutal and asymmetrical dyads & transitivity

#Diary7
#restructure data
Connect_Diary_7_Network <- dcast(Connect_Diary_7[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Connect_Diary_7_Network <- as.matrix(Connect_Diary_7_Network[,-1])
#define network model
Networkmodel_Diary7 <- as.network (x = Connect_Diary_7_Network, directed = T, loops = F, matrix.type = "adjacency")
#give out statistics
dyad.census(Networkmodel_Diary7) #number of mutal and asymmetrical dyads
gtrans(Networkmodel_Diary7,measure="weak", use.adjacency=FALSE) # transitivity

#Diary15
#restructure data
Connect_Diary_15_Network <- dcast(Connect_Diary_15[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Connect_Diary_15_Network <- as.matrix(Connect_Diary_15_Network[,-1])
#define network model
Networkmodel_Diary15 <- as.network (x = Connect_Diary_15_Network, directed = T, loops = F, matrix.type = "adjacency")
#give out statistics
dyad.census(Networkmodel_Diary15) #number of mutal and asymmetrical dyads
gtrans(Networkmodel_Diary15,measure="weak", use.adjacency=FALSE) # transitivity

#Diary21
#restructure data
Connect_Diary_21_Network <- dcast(Connect_Diary_21[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Connect_Diary_21_Network <- as.matrix(Connect_Diary_21_Network[,-1])
#define network model
Networkmodel_Diary21 <- as.network (x = Connect_Diary_21_Network, directed = T, loops = F, matrix.type = "adjacency")
#give out statistics
dyad.census(Networkmodel_Diary21) #number of mutal and asymmetrical dyads
gtrans(Networkmodel_Diary21,measure="weak", use.adjacency=FALSE) # transitivity


#Network statistics 
#Extraversion & Indegrees

#Diary7
#select relevant data
Connect_Diary_7_Network_indegree <- select (Connect_Diary_7, id_a, id_p, friend)
#aggregate being chosen as a friend per id_p
Connect_Diary_7_Network_indegree <- aggregate (Connect_Diary_7_Network_indegree, by = list(Connect_Diary_7_Network_indegree$id_p), FUN = mean, na.rm =T)
#select relevant data
Connect_Diary_7_Network_indegree <- select (Connect_Diary_7_Network_indegree, id_p, friend)
#rename id_p to id_a for matching with personality
names(Connect_Diary_7_Network_indegree)[names(Connect_Diary_7_Network_indegree)=="id_p"] <-"id_a"
#merge with personality
Connect_Diary_7_Network_indegree <- merge(Connect_Diary_7_Network_indegree, Connect_Traits_t1, by = "id_a")
#calculate correlation
cor.test(Connect_Diary_7_Network_indegree$friend,Connect_Diary_7_Network_indegree$big5_e )

#Diary15
#select relevant data
Connect_Diary_15_Network_indegree <- select (Connect_Diary_15, id_a, id_p, friend)
#aggregate being chosen as a friend per id_p
Connect_Diary_15_Network_indegree <- aggregate (Connect_Diary_15_Network_indegree, by = list(Connect_Diary_15_Network_indegree$id_p), FUN = mean, na.rm =T)
#select relevant data
Connect_Diary_15_Network_indegree <- select (Connect_Diary_15_Network_indegree, id_p, friend)
#rename id_p to id_a for matching with personality
names(Connect_Diary_15_Network_indegree)[names(Connect_Diary_15_Network_indegree)=="id_p"] <-"id_a"
#merge with personality
Connect_Diary_15_Network_indegree <- merge(Connect_Diary_15_Network_indegree, Connect_Traits_t1, by = "id_a")
#calculate correlation
cor.test(Connect_Diary_15_Network_indegree$friend,Connect_Diary_15_Network_indegree$big5_e )

#Diary21
#select relevant data
Connect_Diary_21_Network_indegree <- select (Connect_Diary_21, id_a, id_p, friend)
#aggregate being chosen as a friend per id_p
Connect_Diary_21_Network_indegree <- aggregate (Connect_Diary_21_Network_indegree, by = list(Connect_Diary_21_Network_indegree$id_p), FUN = mean, na.rm =T)
#select relevant data
Connect_Diary_21_Network_indegree <- select (Connect_Diary_21_Network_indegree, id_p, friend)
#rename id_p to id_a for matching with personality
names(Connect_Diary_21_Network_indegree)[names(Connect_Diary_21_Network_indegree)=="id_p"] <-"id_a"
#merge with personality
Connect_Diary_21_Network_indegree <- merge(Connect_Diary_21_Network_indegree, Connect_Traits_t1, by = "id_a")
#calculate correlation
cor.test(Connect_Diary_21_Network_indegree$friend,Connect_Diary_21_Network_indegree$big5_e )


#Network statistics 
#Extraversion & Outdegrees

#Diary7
#select relevant data
Connect_Diary_7_Network_outdegree <- select (Connect_Diary_7, id_a, id_p, friend)
#aggregate choose a friend per id_a
Connect_Diary_7_Network_outdegree <- aggregate (Connect_Diary_7_Network_outdegree, by = list(Connect_Diary_7_Network_outdegree$id_a), FUN = mean, na.rm =T)
#select relevant data
Connect_Diary_7_Network_outdegree <- select (Connect_Diary_7_Network_outdegree, id_a, friend)
#merge with personality
Connect_Diary_7_Network_outdegree <- merge(Connect_Diary_7_Network_outdegree, Connect_Traits_t1, by = "id_a")
#calculate correlation
cor.test(Connect_Diary_7_Network_outdegree$friend,Connect_Diary_7_Network_outdegree$big5_e)

#Diary15
#select relevant data
Connect_Diary_15_Network_outdegree <- select (Connect_Diary_15, id_a, id_p, friend)
#aggregate choose a friend per id_a
Connect_Diary_15_Network_outdegree <- aggregate (Connect_Diary_15_Network_outdegree, by = list(Connect_Diary_15_Network_outdegree$id_a), FUN = mean, na.rm =T)
#select relevant data
Connect_Diary_15_Network_outdegree <- select (Connect_Diary_15_Network_outdegree, id_a, friend)
#merge with personality
Connect_Diary_15_Network_outdegree <- merge(Connect_Diary_15_Network_outdegree, Connect_Traits_t1, by = "id_a")
#calculate correlation
cor.test(Connect_Diary_15_Network_outdegree$friend,Connect_Diary_15_Network_outdegree$big5_e)

#Diary21
#select relevant data
Connect_Diary_21_Network_outdegree <- select (Connect_Diary_21, id_a, id_p, friend)
#aggregate choose a friend per id_a
Connect_Diary_21_Network_outdegree <- aggregate (Connect_Diary_21_Network_outdegree, by = list(Connect_Diary_21_Network_outdegree$id_a), FUN = mean, na.rm =T)
#select relevant data
Connect_Diary_21_Network_outdegree <- select (Connect_Diary_21_Network_outdegree, id_a, friend)
#merge with personality
Connect_Diary_21_Network_outdegree <- merge(Connect_Diary_21_Network_outdegree, Connect_Traits_t1, by = "id_a")
#calculate correlation
cor.test(Connect_Diary_21_Network_outdegree$friend,Connect_Diary_21_Network_outdegree$big5_e)


#Exponential Random Graph Models & Plots
#for plots and Exponential Random Graph Models take only those id_a and id_p that provided extraversion ratings
#merge diaries with extraversion scores id_a
Connect_Diary_ext <- merge (Connect_Diary_other,as.data.frame(select(Connect_Traits_t1, id_a, big5_e_t1)), by ="id_a" )
#merge diaries with extraversion scores id_p
Connect_Traits_t1_2 <-Connect_Traits_t1
names(Connect_Traits_t1_2)[names(Connect_Traits_t1_2)=="id_a"] <-"id_p"
Connect_Diary_ext <- merge (Connect_Diary_ext,as.data.frame(select(Connect_Traits_t1_2, id_p, big5_e_t1)), by ="id_p" )
#select only those ids and idps with values in extraversion (for plots and results)
Connect_Diary_ext <- subset(Connect_Diary_ext,Connect_Diary_ext$big5_e_t1.x >0 & Connect_Diary_ext$big5_e_t1.y > 0 )


# create Networks with Extraversion and Sex
#Network Model Diary7
#choose diary number
Connect_Diary_7_2 <- subset(Connect_Diary_ext, Connect_Diary_ext$diary_nr_total==7)
#restructure data
Connect_Diary_7_2 <- dcast(Connect_Diary_7_2[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Connect_Diary_7_2 <- as.matrix(Connect_Diary_7_2[,-1])
#create network model
Connect_Diary_7_2 <- as.network (x = Connect_Diary_7_2, directed = T, loops = F, matrix.type = "adjacency")
summary(Connect_Diary_7_2)
#create Ids
network.vertex.names(Connect_Diary_7_2) <- c(701:824)      #set IDS
##take relevant extraversion scores
extraversion <- as.double(select(subset(Connect_Traits_t1, big5_e_t1>=1), big5_e_t1)$big5_e_t1)
#check for missings
extraversion                                                                            
##set extraversion to networkmodel
set.vertex.attribute(Connect_Diary_7_2,"extraversion",extraversion)
##take relevant sexes
sex <- as.double(select(subset(Connect_Traits_t1, big5_e_t1>1), sex_t1)$sex_t1)              #set group
#check for missings                      
sex                                                                                     #check for missings
##set sex to networkmodel
set.vertex.attribute(Connect_Diary_7_2,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:124){ if(get.node.attr(Connect_Diary_7_2,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }

#Network Model Diary15
#choose diary number
Connect_Diary_15_2 <- subset(Connect_Diary_ext, Connect_Diary_ext$diary_nr_total==15)
#restructure data
Connect_Diary_15_2 <- dcast(Connect_Diary_15_2[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Connect_Diary_15_2 <- as.matrix(Connect_Diary_15_2[,-1])
#create network model
Connect_Diary_15_2 <- as.network (x = Connect_Diary_15_2, directed = T, loops = F, matrix.type = "adjacency")
summary(Connect_Diary_15_2)
#create Ids
network.vertex.names(Connect_Diary_15_2) <- c(701:824)      #set IDS
##take relevant extraversion scores
extraversion <- as.double(select(subset(Connect_Traits_t1, big5_e_t1>=1), big5_e_t1)$big5_e_t1)
#check for missings
extraversion                                                                            
##set extraversion to networkmodel
set.vertex.attribute(Connect_Diary_15_2,"extraversion",extraversion)
##take relevant sexes
sex <- as.double(select(subset(Connect_Traits_t1, big5_e_t1>1), sex_t1)$sex_t1)              #set group
#check for missings                      
sex                                                                                     #check for missings
##set sex to networkmodel
set.vertex.attribute(Connect_Diary_15_2,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:124){ if(get.node.attr(Connect_Diary_15_2,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }

#Network Model Diary21
#choose diary number
Connect_Diary_21_2 <- subset(Connect_Diary_ext, Connect_Diary_ext$diary_nr_total==21)
#restructure data
Connect_Diary_21_2 <- dcast(Connect_Diary_21_2[,c("id_a","id_p","friend")], id_a~id_p, value.var="friend")
#remove first column (which contains the ids)
Connect_Diary_21_2 <- as.matrix(Connect_Diary_21_2[,-1])
#create network model
Connect_Diary_21_2 <- as.network (x = Connect_Diary_21_2, directed = T, loops = F, matrix.type = "adjacency")
summary(Connect_Diary_21_2)
#create Ids
network.vertex.names(Connect_Diary_21_2) <- c(701:824)      #set IDS
##take relevant extraversion scores
extraversion <- as.double(select(subset(Connect_Traits_t1, big5_e_t1>=1), big5_e_t1)$big5_e_t1)
#check for missings
extraversion                                                                            
##set extraversion to networkmodel
set.vertex.attribute(Connect_Diary_21_2,"extraversion",extraversion)
##take relevant sexes
sex <- as.double(select(subset(Connect_Traits_t1, big5_e_t1>1), sex_t1)$sex_t1)              #set group
#check for missings                      
sex                                                                                     #check for missings
##set sex to networkmodel
set.vertex.attribute(Connect_Diary_21_2,"sex",sex)
#set node colors
node_colors <- rep("",6)
for (i in 1:124){ if(get.node.attr(Connect_Diary_21_2,"sex")[i] == 1) { node_colors[i] <- "hotpink"} else { node_colors[i] <- "deepskyblue"  } }

#ERGM Models Including including actor attribute effect of extraversion on outdegrees and indegress, similarity in extraversion, controlling for egdges and dyads
#Diary 7
ERGM1 <- ergm(Connect_Diary_7_2 ~ edges + mutual+ nodeocov("extraversion") + nodeicov("extraversion") + smalldiff("extraversion", 0.55), burnin=15000, MCMCsamplesize=30000, verbose=FALSE)
summary(ERGM1)
#Diary15
ERGM2 <- ergm(Connect_Diary_15_2 ~ edges + mutual+ nodeocov("extraversion") + nodeicov("extraversion") + smalldiff("extraversion", 0.55), burnin=15000, MCMCsamplesize=30000, verbose=FALSE)
summary(ERGM2)
#Diary21
ERGM3 <- ergm(Connect_Diary_21_2 ~ edges + mutual+ nodeocov("extraversion") + nodeicov("extraversion") + smalldiff("extraversion", 0.55), burnin=15000, MCMCsamplesize=30000, verbose=FALSE)
summary(ERGM3)

#Define Layout of plots
network.layout.modeconnect <- function (nw, layout.par) 
{
  n <- network.size(nw)
  d <- as.matrix.network(nw, matrix.type = "edgelist")[, 1:2, 
                                                       drop = FALSE]
  if (is.null(layout.par$niter)) 
    niter <- 10000
  else niter <- layout.par$niter
  if (is.null(layout.par$max.delta)) 
    max.delta <- n^3
  else max.delta <- layout.par$max.delta
  if (is.null(layout.par$area)) 
    area <- n^6
  else area <- layout.par$area
  if (is.null(layout.par$cool.exp)) 
    cool.exp <- 3
  else cool.exp <- layout.par$cool.exp
  if (is.null(layout.par$repulse.rad)) 
    repulse.rad <- area * log(n)
  else repulse.rad <- layout.par$repulse.rad
  if (is.null(layout.par$ncell)) 
    ncell <- ceiling(n^2)
  else ncell <- layout.par$ncell
  if (is.null(layout.par$cell.jitter)) 
    cell.jitter <- 2
  else cell.jitter <- layout.par$cell.jitter
  if (is.null(layout.par$cell.pointpointrad)) 
    cell.pointpointrad <- 0
  else cell.pointpointrad <- layout.par$cell.pointpointrad
  if (is.null(layout.par$cell.pointcellrad)) 
    cell.pointcellrad <- 20
  else cell.pointcellrad <- layout.par$cell.pointcellrad
  if (is.null(layout.par$cellcellcellrad)) 
    cell.cellcellrad <- ncell^10
  else cell.cellcellrad <- layout.par$cell.cellcellrad
  if (is.null(layout.par$seed.coord)) {
    tempa <- sample((0:(n - 1))/n)
    x <- n/(2 * pi) * sin(2 * pi * tempa)
    y <- n/(2 * pi) * cos(2 * pi * tempa)
  }
  else {
    x <- layout.par$seed.coord[, 1]
    y <- layout.par$seed.coord[, 2]
  }
  d <- unique(rbind(d, d[, 2:1]))
  layout <- .C("network_layout_fruchtermanreingold_R", as.double(d), 
               as.double(n), as.double(NROW(d)), as.integer(niter), 
               as.double(max.delta), as.double(area), as.double(cool.exp), 
               as.double(repulse.rad), as.integer(ncell), as.double(cell.jitter), 
               as.double(cell.pointpointrad), as.double(cell.pointcellrad), 
               as.double(cell.cellcellrad), x = as.double(x), y = as.double(y), 
               PACKAGE = "network")
  cbind(layout$x, layout$y)
}

#Plot Networks
plot.network(Connect_Diary_7_2,vertex.col =node_colors, vertex.cex = (extraversion^1.8)/15, mode = "modeconnect", arrowhead.cex = 0.5)
plot.network(Connect_Diary_15_2,vertex.col =node_colors, vertex.cex = (extraversion^1.8)/15, mode = "modeconnect", arrowhead.cex = 0.5)
plot.network(Connect_Diary_21_2,vertex.col =node_colors, vertex.cex = (extraversion^1.8)/15, mode = "modeconnect", arrowhead.cex = 0.5)

