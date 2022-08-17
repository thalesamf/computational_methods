community.A <- c(10,6,4,1)

community.B <- c(17, rep(1,7))

install.packages("vegan")

library(vegan)

diversity(community.A, "shannon")
diversity(community.B, "shannon")
diversity(community.A, "invsimpson")
diversity(community.B, "invsimpson")

ren_comA <- renyi(community.A)
ren_comB <- renyi(community.B)

ren_AB <- rbind(ren_comA, ren_comB)
matplot(t(ren_AB), type = 'l', axes = F)  #The profile of diversity intersect, being not comparable
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)
legend("topright",
       legend = c("Community A", "Community B"),
       lty = c(1,2),
       col = c(1,2))
