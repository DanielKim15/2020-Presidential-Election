library(tidyverse)
library(gridExtra)
library(usmap)
library(ggplot2)
library(usmap)
library(lubridate) #works with dates and times
library(dplyr)
install.packages("plyr")
library(plyr)

##Step 1: load the data
#election dataset
election2020 <- read_csv("/Users/dannykim/Downloads/USElection2020-NYT-Results-master/data/latest/presidential.csv", na=c(" ","NA","NULL","PrivacySuppressed"))
#election2020 %>% select("fips","name","votes","absentee_votes","results_trumpd","results_bidenj","last_updated")
election2020 <- select(election2020,"fips","name","votes","absentee_votes","results_trumpd","results_bidenj", "results_absentee_trumpd", "results_absentee_bidenj","last_updated","state")
election2020
str(election2020)
dim(election2020)
#Note: seems like we just need to focus on the 11/15/20

#electoral votes data set
electoral_number <- read.csv("/Users/dannykim/Downloads/electoralvotes (1).csv")
electoral_number$state <- as.character(electoral_number$state)
electoral_number

electoral_number[1,1] <- "Alabama"
electoral_number[2,1] <- "Alaska"
electoral_number[3,1] <- "Arizona"
electoral_number[4,1] <- "Arkansas"
electoral_number[5,1] <- "California"
electoral_number[6,1] <- "Colorado"
electoral_number[7,1] <- "Connecticut"
electoral_number[8,1] <- "Delaware"
electoral_number[9,1] <- "District of Columbia"
electoral_number[10,1] <- "Florida"
electoral_number[11,1] <- "Georgia"
electoral_number[12,1] <- "Hawaii"
electoral_number[13,1] <- "Idaho"
electoral_number[14,1] <- "Illinois"
electoral_number[15,1] <- "Indiana"
electoral_number[16,1] <- "Iowa"
electoral_number[17,1] <- "Kansas"
electoral_number[18,1] <- "Kentucky"
electoral_number[19,1] <- "Louisiana"
electoral_number[20,1] <- "Maine"
electoral_number[21,1] <- "Maryland"
electoral_number[22,1] <- "Massachusetts"
electoral_number[23,1] <- "Michigan"
electoral_number[24,1] <- "Minnesota"
electoral_number[25,1] <- "Mississippi"
electoral_number[26,1] <- "Missouri"
electoral_number[27,1] <- "Montana"
electoral_number[28,1] <- "Nebraska"
electoral_number[29,1] <- "Nevada"
electoral_number[30,1] <- "New Hampshire"
electoral_number[31,1] <- "New Jersey"
electoral_number[32,1] <- "New Mexico"
electoral_number[33,1] <- "New York"
electoral_number[34,1] <- "North Carolina"
electoral_number[35,1] <- "North Dakota"
electoral_number[36,1] <- "Ohio"
electoral_number[37,1] <- "Oklahoma"
electoral_number[38,1] <- "Oregon"
electoral_number[39,1] <- "Pennsylvania"
electoral_number[40,1] <- "Rhode Island"
electoral_number[41,1] <- "South Carolina"
electoral_number[42,1] <- "South Dakota"
electoral_number[43,1] <- "Tennessee"
electoral_number[44,1] <- "Texas"
electoral_number[45,1] <- "Utah"
electoral_number[46,1] <- "Vermont"
electoral_number[47,1] <- "Virginia"
electoral_number[48,1] <- "Washington"
electoral_number[49,1] <- "West Virginia"
electoral_number[50,1] <- "Wisconsin"
electoral_number[51,1] <- "Wyoming"


#Step 2: prepare data
dat1 <- election2020 %>% arrange(fips) #arranges the data through state codes
dat1
view(dat1)


dat1[1:67,10] <- "Alabama"
dat1[68:107,10] <- "Alaska"
dat1[108:122,10] <- "Arizona"
dat1[123:197,10] <- "Arkansas"
dat1[198:255,10] <- "California"
dat1[256:319,10] <- "Colorado"
dat1[320:327,10] <- "Connecticut"
dat1[328:330,10] <- "Delaware"
dat1[331:338,10] <- "District of Columbia"
dat1[339:405,10] <- "Florida"
dat1[406:564,10] <- "Georgia"
dat1[565:568,10] <- "Hawaii"
dat1[569:612,10] <- "Idaho"
dat1[613:714,10] <- "Illinois"
dat1[715:806,10] <- "Indiana"
dat1[807:905,10] <- "Iowa"
dat1[906:1010,10] <- "Kansas"
dat1[1011:1130,10] <- "Kentucky"
dat1[1131:1194,10] <- "Louisiana"
dat1[1195:1210,10] <- "Maine"
dat1[1211:1234,10] <- "Maryland"
dat1[1235:1248,10] <- "Massachusetts"
dat1[1249:1331,10] <- "Michigan"
dat1[1332:1418,10] <- "Minnesota"
dat1[1419:1500,10] <- "Mississippi"
dat1[1501:1615,10] <- "Missouri"
dat1[1616:1671,10] <- "Montana"
dat1[1672:1764,10] <- "Nebraska"
dat1[1765:1781,10] <- "Nevada"
dat1[1782:1791,10] <- "New Hampshire"
dat1[1792:1812,10] <- "New Jersey"
dat1[1813:1845,10] <- "New Mexico"
dat1[1846:1907,10] <- "New York"
dat1[1908:2007,10] <- "North Carolina"
dat1[2008:2060,10] <- "North Dakota"
dat1[2061:2148,10] <- "Ohio"
dat1[2149:2225,10] <- "Oklahoma"
dat1[2226:2261,10] <- "Oregon"
dat1[2262:2328,10] <- "Pennsylvania"
dat1[2329:2333,10] <- "Rhode Island"
dat1[2334:2379,10] <- "South Carolina"
dat1[2380:2445,10] <- "South Dakota"
dat1[2446:2540,10] <- "Tennessee"
dat1[2541:2794,10] <- "Texas"
dat1[2795:2823,10] <- "Utah"
dat1[2824:2837,10] <- "Vermont"
dat1[2838:2970,10] <- "Virginia"
dat1[2971:3009,10] <- "Washington"
dat1[3010:3064,10] <- "West Virginia"
dat1[3065:3136,10] <- "Wisconsin"
dat1[3137:3159,10] <- "Wyoming"


view(dat1)

str(dat1)

####Step 3: Make and Run the Function
plot_election_results <- function(electoral=F,vote_type="total",region = c()){
  #regions <- region
  #regions <- NULL
  total_votes <- NULL
  Biden_total_votes <- NULL
  Trump_total_votes <- NULL
  Absentee_votes <- NULL
  Biden_Absentee_votes <- NULL
  Trump_Absentee_votes <- NULL
  Biden_Wins <- NULL
  elect <- NULL
  longitude <- NULL
  latitude <- NULL
  
  #Setting up coordinates and electoral votes
  state_centers <- usmap_transform(tibble(state.center$x,state.center$y,state.name)) 
  #cases_tdate=cases_tdate[-9,]  #get rid of District of Columbia because state.centers doesn't have it
  #cases_tdate=mutate(cases_tdate,center_long=state_centers$state.center.x.1,center_lat=state_centers$state.center.y.1)
  
  electoral_number <- electoral_number[-9,] #gets rid of distric of columbia
  electoral_number <- mutate(electoral_number, center_long=state_centers$state.center.x.1,center_lat=state_centers$state.center.y.1)
  
  electoral_number[2,3]=-1203560  #Alaska
  electoral_number[2,4]=-1837070
  electoral_number[11,3]=-450000 #Hawii correct coordinates
  electoral_number[11,4]=-2130070
  
  #Make an if else statement over here for region
  if(is.null(region)){
    region = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
  }
  state <- region
  #For loop for total votes
  for(i in region){
    #print(i)
    hi <- filter(dat1, state == i) 
    total_votes[i] <- sum(hi[,3])
    Biden_total_votes[i] <- sum(hi[,6])
    Trump_total_votes[i] <-  sum(hi[,5])
    Biden_Wins[i] = Biden_total_votes[i] > Trump_total_votes[i] #not working for some reason, possible solution: just make this outside of the for loop
    
  }
  
  
  #For loop for absentee votes
  for(a in region){
    test1 <- filter(dat1, state == a) 
    Absentee_votes[a] <- sum(test1[,4])
    Biden_Absentee_votes[a] <- sum(test1[,8])
    Trump_Absentee_votes[a] <-  sum(test1[,7])
  }
  #For loop for electoral number votes and it coordinates
  for (e in region){
    number <- filter(electoral_number, state == e)
    elect[e] <- number[,2]
    longitude[e] <- number[,3]
    latitude[e] <- number[,4]
    
  }
  
  
  #Fixing the data now
  in_person <- total_votes-Absentee_votes
  in_person_Biden <- Biden_total_votes - Biden_Absentee_votes
  in_person_Trump <- Trump_total_votes - Trump_Absentee_votes
  
  #Percentages
  Biden_percentage_totalvote <- (100*Biden_total_votes/total_votes)
  Trump_percentage_totalvote <- (100*Trump_total_votes/total_votes)
  
  Biden_percentage_Absentee <- (100*Biden_Absentee_votes/Absentee_votes)
  Trump_percentage_Absentee <- (100*Trump_Absentee_votes/Absentee_votes)
  
  Biden_percentage_inperson <- (100*in_person_Biden/in_person)
  Trump_percentage_inperson <- (100*in_person_Trump/in_person)
  
  
  #New dataframe
  dat2 <- data_frame(total_votes,Biden_total_votes,Trump_total_votes,Absentee_votes,Biden_Absentee_votes,Trump_Absentee_votes,in_person,in_person_Biden,in_person_Trump,Biden_percentage_totalvote,Trump_percentage_totalvote, Biden_percentage_Absentee,Trump_percentage_Absentee, Biden_percentage_inperson, Trump_percentage_inperson,region,state,elect,longitude,latitude,Biden_Wins)
  
  #Error statement:
  #ifelse(region == c("alabama","alaska","arizona","arkansas","california","colorado","connecticut","delaware","florida","georgia","hawaii","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts","michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia","washington","west virginia","wisconsin","wyoming"), print("Error, capitlize the first letter on the states"))
  
  #If else moment
  if((electoral==F)&(vote_type=="total"))
  {
    p1 <-  plot_usmap(data=dat2,values="Biden_percentage_totalvote", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_continuous(low="white",high="red", name = "Total Votes Biden (%)", label = scales::comma,limits=NULL) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      ggtitle(paste("Total votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
    p2 <-  plot_usmap(data=dat2,values="Trump_percentage_totalvote", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_continuous(low="white",high="red", name = "Total Votes Trump (%)", label = scales::comma,limits=NULL) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      ggtitle(paste("Total votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
    grid.arrange(p1,p2,nrow=2)
  } else if((electoral==F)&(vote_type=="absentee"))
  {
    p1 <-  plot_usmap(data=dat2,values="Biden_percentage_Absentee", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_continuous(low="white",high="red", name = "Absentee Votes Biden (%)", label = scales::comma,limits=NULL) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      ggtitle(paste("Absentee votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
    p2 <-  plot_usmap(data=dat2,values="Trump_percentage_Absentee", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_continuous(low="white",high="red", name = "Absentee Votes Trump (%)", label = scales::comma,limits=NULL) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      ggtitle(paste("Absentee votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
    grid.arrange(p1,p2,nrow=2)
  } else if((electoral==F)&(vote_type=="in-person"))
  {
    p1 <-  plot_usmap(data=dat2,values="Biden_percentage_inperson", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_continuous(low="white",high="red", name = "In-person Votes Biden (%)", label = scales::comma,limits=NULL) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      ggtitle(paste("Inperson votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
    p2 <-  plot_usmap(data=dat2,values="Trump_percentage_inperson", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_continuous(low="white",high="red", name = "In-person Votes Trump (%)", label = scales::comma,limits=NULL) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      ggtitle(paste("Inperson votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
    grid.arrange(p1,p2,nrow=2)
  } else if((electoral==T)&(vote_type=="total"))
  {
    #Write something in here
    plot_usmap(data=dat2,values="Biden_Wins", regions="states",include=region) + #while usmap isn't ggplot, it allows ggplot features
      scale_fill_manual(values = c("red","blue"),labels = c("Trump","Biden")) +
      theme(legend.position = "right") + #allows you to changle location of the legend
      geom_text(data=dat2,aes(x=longitude,y=latitude,label=elect)) +
      ggtitle(paste("electoral votes ",sep="")) +
      theme(plot.title = element_text(size=12)) 
  }else{
    print("Error, please adjust your input. For electoral, the option is T or F. For vote_type its total, absentee, or in-person. 
          For region it has to be a state or list of state with a capital letter in the beginning, and if there's two words like New York, don't use -, just use space")
  }
}

plot_election_results(electoral = F, vote_type = "total", region = c("Georgia","Florida","Alabama")) #OH MA GAWD I DID IT









#Biden_Wins
#Note: the states must be written with a capital letter in the beginning

#.south_atlantic = c("Deleware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia", "West Virginia")
#.south_region = c("Deleware","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia", "West Virginia","Alabama","Kentucky","Massachusetts","Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
#.south_region

#.midwest_region = c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")

#.north_central_region = c("Illinois","Indiana","Michigan","Ohio","Wisconsin","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
#.northeast_region = c("Connecticut","Massachusetts","Maine","New Hampshire", "Rhode Island","Vermont","New Jersey","New York","Pennsylvania")

#.east_north_central = c("Illinois","Indiana","Michigan","Ohio","Wisconsin")
#.mid_atlantic = c("New Jersey","New York","Pennsylvania")

#.new_england = c("Connecticut", "Massachusetts","Maine","New Hampshire","Rhode Island","Vermont")

#.west_region = c("Arizona","Colorado","Indiana","Montana","Nevada","New Mexico","Utah","Wyoming","Arkansas","California","Hawii","Oregon","Washington")
#.west_north_central = c("Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
#.west_south_central = c("Arkansas","Louisiana","Oklahoma","Texas")





