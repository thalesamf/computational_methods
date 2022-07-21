#Building models Class

#Packages-----------------------------------------------------------------------
library(bbmle)
library(ggplot2)
#Data---------------------------------------------------------------------------

cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")

#Hypothesis models--------------------------------------------------------------
h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))


summary(h3)
summary(h2)
summary(h1)

AICtab(h0,h1,h2,h3, base = TRUE, weights = TRUE)

#Predicting values--------------------------------------------------------------

newdata <- expand.grid(Mass = seq(min(cuckoo$Mass), max(cuckoo$Mass), length.out = 200),
                       Species = unique(cuckoo$Species))
newdata$Beg <- predict(h3, newdata, type = 'response')

summary(newdata$Beg)
#Plotting-----------------------------------------------------------------------
p <- ggplot(mapping = aes(x = Mass, y = Beg, colour = Species)) +
  geom_point(data = cuckoo) +
  geom_line(data = newdata) +
  theme_bw()
p
