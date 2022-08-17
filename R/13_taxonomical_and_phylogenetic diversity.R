#Back to cestes data-----------------------------------------------------
comm <- read.csv("comm.csv", header = TRUE)
traits <- read.csv("traits.csv", header = TRUE)

head(comm)[,1:6]
head(traits)[,1:6]

rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

rownames(traits)[1:6]
rownames(traits) <- paste0(traits[,1])
traits <- traits[,-1]
head(traits)[,1:6]

#Species richness--------------------------------------------------------
library(vegan)
richness <- vegan::specnumber(comm)

#Taxonomic diversity
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

library(cluster)
library(FD)
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?

class(gow) #different classes
class(gow2)

plot(gow, gow2, asp = 1) #same values

#Rao's quadratic entropic calculation in R------------------------------
library(SYNCSA)
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)

#Calculating FD indices with package PD

library(FD)
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)
#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)

#How to we summarize visually, interpret community composition and trait data?
