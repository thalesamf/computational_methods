#Packages

#Data
comm <- read.csv("data/raw/cestes/comm.csv")


#Which are the 5 most abundant species overall in the dataset?

five_most_abundant <- sort(colSums(comm), decreasing = TRUE)[2:6]

#How many species are there in each plot? (Richness)

comm_binary <- comm[,-1]
comm_binary[comm_binary > 0] <- 1
species_site <- rowSums(comm_binary)


#Which the species that is most abundant in each plot?

apply(X=comm[,-1], MARGIN=1, FUN=which.max)

#Shannon diversity pseudocode or code
relativeAbundance <- mapply(sum,comm[-1])/sum(mapply(sum,comm[-1]))
shannon <- sum(mapply(function(x) -x*log(x), relativeAbundance))
simpson <- 1 - sum(mapply(function(x) x^2, relativeAbundance))
inverseSimpson <- 1/sum(mapply(function(x) x^2, relativeAbundance))

shannon_diversity <- data.frame(shannon, simpson, inverseSimpson,
                 row.names = "Values")
shannon_diversity
