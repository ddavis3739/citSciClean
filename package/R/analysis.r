rm(list=ls(all=TRUE)) # clear any background

fish <- read.csv('fish.csv', header = T)
fish<- fish[!is.na(fish$Common.Name),] # remove NAs for no common name

fish.names <- read.csv('fish_names.csv', header = T)

##### exploratory plots
#plot(fish$Common.Name, las=3) # 

# lots of recoding needed, as multiple spelling

#plot(fish$Local..Name, las=3) #  worse... 

# data clean up

library(stringdist)


name.fish <- levels(fish.names$Fish.Common.Name)
length(name.fish) # initially 154

name.fish2 <- levels(fish$Common.Name)
length(name.fish2) # initially 256

store.names <- array(NA,c(length(name.fish), 20))
store.names[,1]<- name.fish

#name.fish2<- levels()



for(y in 1:length(name.fish)){
  count<-2 # reset for each name, allows movement along name array
  for(x in 1:dim(fish)[1]){
    if((amatch(name.fish[y], fish$Common.Name[x], maxDist=3,nomatch=0))==1){
        if(fish$Common.Name[x] %in% store.names[y,]){
          #do nothing
        }
      else{
        store.names[y,count]<- as.character(fish$Common.Name[x])
        count <- count + 1
        #print(x)
        #print(fish$Common.Name[x])
        }
      }
  }
}

#fix(store.names) # not on server version of R studio, but allows manual correction

#fish$Common.Name[5371]
#[1] Yellowfin goatfish
fish$Common.Name<-as.character(fish$Common.Name)

for(y in 1:length(name.fish)){
  for(x in 1:dim(fish)[1]){
    if(as.character(fish$Common.Name[x]) %in% store.names[y,2:20]){
      print(c(as.character(fish$Common.Name[x]), as.character(store.names[y,1])))
      fish$Common.Name[x] <- as.character(store.names[y,1])
    }}}
### rewrites species names to all be the same as in the species list
  
name.fish2 <- levels(as.factor(fish$Common.Name))
length(name.fish2) # initially 256 # now 215 - but lots of duplicates, as not in the intial species list

for(x in 1:length(name.fish2)){
  for(y in 1:dim(fish)[1]){
    if(ain(name.fish2[x],fish$Common.Name[y],maxDist=4)){
      fish$Common.Name[y] <- name.fish2[x]
      }
    }
  }

for(x in 1:length(name.fish2)){
  for(y in 1:dim(fish)[1]){
    if(name.fish2[x]=='Grouper' || name.fish2[x]=='Wrasse'){
      #do nothing
    }
    else if(agrepl(name.fish2[x],fish$Common.Name[y],max.distance = 0.1, fixed=T)==T){
      fish$Common.Name[y] <- name.fish2[x]
    }
  }
}

name.fish3 <- levels(as.factor(fish$Common.Name))
length(name.fish3) # initially 256 # then 215 # should be 102
### all good- but loads of 'grouper' and 'wrasse' which need not to be aggregated at final stage

plot(as.factor(fish$Common.Name), las=3)
### sorted as best as possible. Lots of 'grouper'


############## Trips #############

trips <- read.csv('trips.csv', header = T)

#head(fish)
#head(trips)
### plan. Work out ownership? Plot by that.
### plot cpue vs date (cpue = time*no.fishers), by location (by gear type?)
### plot size of fish vs date (by location, by gear type?)

#levels(trips$Ownership)
#plot(trips$Ownership, las=3)
### ifalug, lamotrek & satawal - and spelling issues

trips <- trips[trips$Ownership %in% c("Satawal","satawal","satwal","lemotrek", "Lemotrek" , "lamotrek", "ifalik", "Ifalik"  , "ifalug", "Ifaluk"),]

trips$Ownership<-as.character(trips$Ownership)

for(y in 1:dim(trips)[1]){
    if(ain("Satawal",trips$Ownership[y],maxDist=3)){
      trips$Ownership[y] <- "Satawal"
    }
    if(ain("Lemotrek",trips$Ownership[y],maxDist=3)){
      trips$Ownership[y] <- "Lemotrek"
    }
    if(ain("Ifalik",trips$Ownership[y],maxDist=3)){
      trips$Ownership[y] <- "Ifalik"
    }
  }

#levels(as.factor(trips$Ownership))
#plot(as.factor(trips$Ownership),las=3)

#levels(trips$Fishing.Gear)
#plot(trips$Fishing.Gear, las=3) 
### just use things which contain Hook or hook

trips<-trips[trips$Fishing.Gear %in% c("hoo/line" ,  "hook", "Hook /Line","Hook & Line", "Hook and line", "hook/line",  "Hook/line",  "Hook/Line" ,"hook/line/spear fishing", "Hook/Line& spear" ),]
# fishing gear now fine, as all identical


trips$Start<-as.character(trips$Start)
trips <- trips[grepl(":", trips$Start), ]

dtparts = t(as.data.frame(strsplit(trips$Start,':')))

mins<-dtparts[,2]
mins<-as.numeric(mins)/60
hours<-as.numeric(dtparts[,1])
trips$Start <- as.numeric(hours+mins)

trips$Stop<-as.character(trips$Stop)
trips <- trips[grepl(":", trips$Stop), ]

dtparts = t(as.data.frame(strsplit(trips$Stop,':')))
dtparts[90,]

mins<-dtparts[,2]
mins<-as.numeric(mins)/60
hours<-as.numeric(dtparts[,1])
trips$Stop <- as.numeric(hours+mins)

trips<- trips[!is.na(trips$Stop),]
trips<- trips[!is.na(trips$Start),]

time.trip <- trips$Stop - trips$Start

time.trip <- ifelse(time.trip<0 , time.trip+12 , time.trip) # add 12 if < 0, due to 12 h clock

effort <- time.trip*trips$X..0f.Fishermen  # could also try time.trip*sqrt(trips$X..0f.Fishermen) # or log transformation here 

trips$X.NAME.<-as.Date(trips$X.NAME., format='%d/%m/%y')
day.trip <- as.numeric(trips$X.NAME.) - 17313
### day.trip is time in days since 28th May 2017

trips <- cbind(trips, effort, day.trip)

#### end of trips manipulation

effort2<-array(NA,dim(fish[1]))
day.trip2<-array(NA,dim(fish[1]))
location2<-array(NA,dim(fish[1]))

for(x in 1:dim(trips)[1]){
  for(y in 1:dim(fish)[1]){
    if(trips$fishing.trip..[x] == fish$Fishing.Trip..[y]){
      effort2[y]<-effort[x]
      day.trip2[y]<-day.trip[x]
      location2[y]<-trips$Ownership[x]
    }
  }
}

fish<-cbind(fish,effort2,day.trip2,location2)

#tapply(fish$Common.Name,fish$Common.Name,length)

spnumber <- 5 ## how many species to look at
mainsp<-sort(table(fish$Common.Name),decreasing=TRUE)[1:spnumber] # sort top 10 species for analysis
mainsp<-names(mainsp) # just names


for(x in 1:5){
  aa<- fish[fish$Common.Name %in% mainsp[x],]
  assign(paste(mainsp[x]),aa)
}

#### easiest to check for common species variables now...

Grouper<-Grouper # cheat - but change this, and titles of graphs to do further plots
Grouper <-  `Coachwip Trevally`

levels(Grouper$location2)

Grouper.Satawal<-Grouper[Grouper$location2 %in% 'Satawal',]
Grouper.Ifalik<-Grouper[Grouper$location2 %in% 'Ifalik',]

sat.grouper.by.trip<-tapply(Grouper.Satawal$Fishing.Trip..,Grouper.Satawal$Fishing.Trip..,length )
sat.effort.grouper.by.trip<-tapply(Grouper.Satawal$effort2,Grouper.Satawal$Fishing.Trip..,mean )
sat.time.grouper.by.trip<-tapply(Grouper.Satawal$day.trip2,Grouper.Satawal$Fishing.Trip..,mean )
sat.cpue.grouper.by.trip<-sat.grouper.by.trip/sat.effort.grouper.by.trip

sat.grouper<-cbind(sat.grouper.by.trip,sat.effort.grouper.by.trip,sat.time.grouper.by.trip,  sat.cpue.grouper.by.trip)
sat.grouper<-as.data.frame(sat.grouper)

ifa.grouper.by.trip<-tapply(Grouper.Ifalik$Fishing.Trip..,Grouper.Ifalik$Fishing.Trip..,length )
ifa.effort.grouper.by.trip<-tapply(Grouper.Ifalik$effort2,Grouper.Ifalik$Fishing.Trip..,mean )
ifa.time.grouper.by.trip<-tapply(Grouper.Ifalik$day.trip2,Grouper.Ifalik$Fishing.Trip..,mean )
ifa.cpue.grouper.by.trip<-ifa.grouper.by.trip/ifa.effort.grouper.by.trip

ifa.grouper<-cbind(ifa.grouper.by.trip,ifa.effort.grouper.by.trip,ifa.time.grouper.by.trip,  ifa.cpue.grouper.by.trip)
ifa.grouper<-as.data.frame(ifa.grouper)

############################## graphs and functions #############################
library(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
############################################################################
p1 <- ggplot(sat.grouper, aes(x=sat.effort.grouper.by.trip, y=sat.grouper.by.trip)) + geom_point(size=1, shape=23) + geom_smooth(method=lm)+ 
  labs(title="Coachwip Trevally Satawal", x="Fishing Effort", y = "Catch")+theme_classic() + scale_color_grey()

p2 <- ggplot(sat.grouper, aes(x=sat.time.grouper.by.trip, y=sat.cpue.grouper.by.trip)) + geom_point(size=1, shape=23) + geom_smooth(method=lm)+ 
  labs(title="Coachwip Trevally Satawal", x="Date", y = "CPUE")+theme_classic() + scale_color_grey()

p3 <- ggplot(Grouper.Satawal, aes(x=Grouper.Satawal$day.trip2, y=as.numeric(Grouper.Satawal$Total.Length..cm.))) + geom_point(size=1, shape=23) + geom_smooth(method=lm)+ 
  labs(title="Coachwip Trevally Satawal", x="Date", y = "Length")+theme_classic() + scale_color_grey()

#Ifalik

p4 <- ggplot(ifa.grouper, aes(x=ifa.effort.grouper.by.trip, y=ifa.grouper.by.trip)) + geom_point(size=1, shape=23) + geom_smooth(method=lm)+ 
  labs(title="Coachwip Trevally Ifalik", x="Fishing Effort", y = "Catch")+theme_classic() + scale_color_grey()

p5 <- ggplot(ifa.grouper, aes(x=ifa.time.grouper.by.trip, y=ifa.cpue.grouper.by.trip)) + geom_point(size=1, shape=23) + geom_smooth(method=lm)+ 
  labs(title="Coachwip Trevally Ifalik", x="Date", y = "CPUE")+theme_classic() + scale_color_grey()

p6 <- ggplot(Grouper.Ifalik, aes(x=Grouper.Ifalik$day.trip2, y=as.numeric(Grouper.Ifalik$Total.Length..cm.))) + geom_point(size=1, shape=23) + geom_smooth(method=lm)+ 
  labs(title="Coachwip Trevally Ifalik", x="Date", y = "Length")+theme_classic() + scale_color_grey()

pdf("rplot_Coachwip_Trevally.pdf") 

multiplot(p1, p2, p3, p4, p5, p6, cols=2)

dev.off() 

