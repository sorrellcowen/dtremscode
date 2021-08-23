setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Mammal_BioFrag_Data")

argentina <- read.csv("PID0040_Argentinia_Mammals/Species_Meta.csv")
bats_jari <- read.csv("PID0058_Jari_Bats/Species_Meta.csv")
mammals_jari <- read.csv("PID0069_Jari_SmallMammals/Species_Meta.csv")
bats_mexico <- read.csv("PID0093_Mexico_Bats/Species_Meta.csv")
mammals_mexico <- read.csv("PID0092_Mexico_Mammals/Species_Meta.csv")
batsperu <- read.csv("PID0143_Peru_Species_Meta.csv")
batscr <- read.csv("PID0144_CR_Species_Meta.csv")
batssaop <- read.csv("PID0222_Sao_Paulo_Bats/PID0222_Species_Meta.csv")

### BELOW IS RIGHT WAY TO FIND THE SPECIES THAT OVERLAP STUDY SITE ####

####### count total species I have

arg <- argentina$Species_IUCN
batsbrazil <- bats_jari$Species_IUCN
mambrazil <- mammals_jari$Species_IUCN
batsmex <- bats_mexico$Species_IUCN
mammex <- mammals_mexico$Species_IUCN
batsperu <- batsperu$Species_IUCN
batscr <- batscr$Species_IUCN
batssaop <- batssaop$Species

#putting all the lists together into 1 list
l <- list(mambrazil=mambrazil,batsbrazil=batsbrazil,batsperu=batsperu,batscr=batscr,batsmex=batsmex,mammex=mammex, batssaop=batssaop)


#turn the lists into data frame with NAs
dfl<-sapply(l, '[', seq(max(sapply(l, length))))

#Counting the number of occurrences of each species 
n_occur <- data.frame(table(dfl))
n_occur
nrow(n_occur)

#Cut this down to just those that occur more than once
n_occur_repeated<-subset(n_occur, Freq>1)
n_occur_repeated
nrow(n_occur_repeated)

#For more than twice
n_occur_repeated2<-subset(n_occur, Freq>2)
n_occur_repeated2
nrow(n_occur_repeated2)

#For more than 3 times
n_occur_repeated3<-subset(n_occur, Freq>3)
n_occur_repeated3
nrow(n_occur_repeated3)

#For more than 4 times
n_occur_repeated4<-subset(n_occur, Freq>4)
n_occur_repeated4
nrow(n_occur_repeated4)


write.csv(n_occur_repeated3, "bat_species_in_all_studies.csv", row.names = FALSE)


