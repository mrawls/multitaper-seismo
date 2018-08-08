# Gwen Eadie
# lab 3

library(DAAG)

####### 2.3

# check the structure of the possum data
str(possum)

# uncomment this AND dev.off() below if you want to save the plot
# pdf(file = "q2-3_eadie.pdf" )

hist(x = possum$earconch, xlab="earconch")
box()

# boxplot of female and male earconch
plot(formula = earconch ~ sex, data = possum)

# the measurement distributions differ in their median with the female earconch having a higher median than the male earconch. However, overall the ranges overlap a lot.

# I predict that the histograms look assymetric, with the females having a long tail towards lower values, and the males having a long tail towards higher values

# plot the females' earconch distribution
plot(density(possum$earconch[possum$sex=="f"]), xlab = "earconch", main="Female Earconch Distribution")

# plot the females' earconch distribution
plot(density(possum$earconch[possum$sex=="m"]), xlab = "earconch", main="Male Earconch Distribution")

# weird, they are both bimodal... maybe because of age?
plot(density(na.omit(possum$age)), main="age distribution")

# ages could be it!

# uncomment this if you want to save the plot
# dev.off()

###### 2.5

pdf(file = "q2-5_eadie.pdf")

plot(formula = cbreadth ~ clength, data=cuckoohosts, ylim=c(12, 18), xlim=c(16, 24))
points(formula= hbreadth ~ hlength, data=cuckoohosts, col="blue", pch=19)

for(i in 1:12){
  with(cuckoohosts, expr = lines(c(clength[i], hlength[i]), c(cbreadth[i], hbreadth[i])))
  
}

legend("topleft", legend = c("cuckoo", "host"), pch=c(1,19), col=c("black", "blue"))

dev.off()


###### 2.10

library(MASS)

with( Animals, cor(brain, body) )

with( Animals, cor(log(brain), log(body)) )

# note that the spearman rank coefficient gives the same answer whether it is logged or not, as long as the data have a monotonic function? i.e. the ranks don't change under the transformation
with( Animals, cor(log(brain), log(body), method="spearman" ))
with( Animals, cor(brain, body, method="spearman" ))

with(Animals, plot(log(brain), log(body)))

# pearson correlation coefficient is the covariance divided by variance squared


###### 2.13 
str(galaxies)

plot(density(galaxies), main= "emperical density distribution of galaxies")

# or make a nicer plot from the example in ?galaxies
gal <- galaxies/1000
c(width.SJ(gal, method = "dpi"), width.SJ(gal))
plot(x = c(0, 40), y = c(0, 0.3), type = "n", bty = "l",
     xlab = "velocity of galaxy (1000km/s)", ylab = "density", main="example from galaxies help file")
rug(gal)
lines(density(gal, width = 3.25, n = 200), lty = 1)
lines(density(gal, width = 2.56, n = 200), lty = 3)

# shut off the plots
dev.off()
