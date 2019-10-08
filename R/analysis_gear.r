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

levels(trips$Fishing.Gear)
plot(trips$Fishing.Gear, las=3) 
### just use things which contain Hook or hook

HookLine<-trips[trips$Fishing.Gear %in% c("hoo/line" ,  "hook", "Hook /Line","Hook & Line", "Hook and line", "hook/line",  "Hook/line",  "Hook/Line" ),]
GillNet <- trips[trips$Fishing.Gear %in% c("Gill net" ,"gill net" ),] ## 2 observations - ignore
Net <- trips[trips$Fishing.Gear %in% c("big net", "Big Net", "fishing net", "net", "Net", "net fishing", "Net fishing"),]
# 10 obs...
Spear <-  trips[trips$Fishing.Gear %in% c("spear", "Spear", "Spear ", "Spear and rubber", "spear fishing", "speargun", "Speargun"),]
## 16 obs
FishTrap <- trips[trips$Fishing.Gear %in% c("fish trap", "Fish Trap", "fish trap "),]
# 34 obs

trips<- trips[!is.na(trips$fishing.trip..),]

gear2<-array(NA,dim(fish[1]))

for(x in 1:dim(trips)[1]){
  for(y in 1:dim(fish)[1]){
    if(trips$fishing.trip..[x] == fish$Fishing.Trip..[y]){
      gear2[y]<-as.character(trips$Fishing.Gear[x])
      #day.trip2[y]<-day.trip[x]
      #location2[y]<-trips$Ownership[x]
    }
  }
}

fish<-cbind(fish,gear2) # writing gear type to each fish caught


HookLine<-fish[fish$gear2 %in% c("hoo/line" ,  "hook", "Hook /Line","Hook & Line", "Hook and line", "hook/line",  "Hook/line",  "Hook/Line" ),]
#GillNet <- trips[trips$Fishing.Gear %in% c("Gill net" ,"gill net" ),] ## 2 observations - ignore
Net <- fish[fish$gear2 %in% c("big net", "Big Net", "fishing net", "net", "Net", "net fishing", "Net fishing"),]
# 10 obs...
Spear <-  fish[fish$gear2 %in% c("spear", "Spear", "Spear ", "Spear and rubber", "spear fishing", "speargun", "Speargun"),]
## 16 obs
FishTrap <- fish[fish$gear2 %in% c("fish trap", "Fish Trap", "fish trap "),]
# 34 obs

FishTrap<-Net #cheat to avoid copying so much code

count.FishTrap<-tapply(FishTrap$Common.Name,list(FishTrap$Common.Name, FishTrap$Fishing.Trip..),length)
length.FishTrap <- tapply(as.numeric(FishTrap$Total.Length..cm.),list(FishTrap$Common.Name, FishTrap$Fishing.Trip..),mean, na.rm=TRUE)
length.FishTrap.sd <- tapply(as.numeric(FishTrap$Total.Length..cm.),list(FishTrap$Common.Name, FishTrap$Fishing.Trip..),sd, na.rm=TRUE)

count.FishTrap<-as.data.frame(count.FishTrap)
count.FishTrap[is.na(count.FishTrap)] <- 0
Fish.Trap.Mean.Count<-apply(count.FishTrap, 1, mean)
Fish.Trap.sd.Count<-apply(count.FishTrap, 1, sd)
Fish.Trap.samplesize.Count <- apply(count.FishTrap, 1, sum)
Fish.Trap.CI.Count <- (1.96*Fish.Trap.sd.Count)/sqrt(Fish.Trap.samplesize.Count)
Fish.Trap.numbers<-cbind(rownames(count.FishTrap), as.numeric(Fish.Trap.Mean.Count), as.numeric(Fish.Trap.CI.Count))
Fish.Trap.numbers<-as.data.frame(Fish.Trap.numbers)

length.means1 <- length.FishTrap*count.FishTrap
length.means2<- apply(count.FishTrap, 1, sum, na.rm=T)
length.means3 <- apply(length.means1, 1, sum, na.rm=T)
length.means<-length.means3/length.means2

f2 = function(x1){
  x1-1  
}
### to calculate n-1


pool.var1<-apply(count.FishTrap,1,f2)
pool.var1[pool.var1<0]<-0 # n-1 values for pooled variance
pool.var2<-length.FishTrap.sd*length.FishTrap.sd # variance
pool.var2[is.na(pool.var2)]<-0
pool.var2<-t(pool.var2)
pool.var3<-pool.var2*pool.var1
pool.var4<-apply(pool.var3,2,sum)
pool.var5<-apply(pool.var1,2,sum)
pool.var6<-pool.var4/pool.var5
pool.sd <- sqrt(pool.var6)
pool.sd[is.na(pool.sd)]<-0
pool.95CI<-(1.96*pool.sd)/sqrt(Fish.Trap.samplesize.Count)

Fish.Trap.Lengths<-cbind(rownames(count.FishTrap), as.numeric(length.means), as.numeric(pool.95CI))
Fish.Trap.Lengths<-as.data.frame(Fish.Trap.Lengths)

library(ggplot2)
p1 <- ggplot(Fish.Trap.Lengths, aes(x=V1, y=length.means)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=length.means-pool.95CI, ymax=length.means+pool.95CI), width=.2,
                position=position_dodge(.9))

p1 + scale_fill_brewer(palette="Paired") + theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+
    labs(title="Net - Fish Length by Fishing Trip (mean +/- 95% CI)", x="", y = "")

p2 <- ggplot(Fish.Trap.numbers, aes(x=as.character(V1), y=Fish.Trap.Mean.Count)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Fish.Trap.Mean.Count-Fish.Trap.CI.Count, ymax=Fish.Trap.Mean.Count+Fish.Trap.CI.Count), width=.2,
                position=position_dodge(.9))

p2 + scale_fill_brewer(palette="Paired") + theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, , vjust=0.5))+
  labs(title="Net - Numbers by Fishing Trip (mean +/- 95% CI)", x="", y = "")



