download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")
View(atheism)

us12 = subset(atheism, nationality == "United States" & year == "2012")
nrow(filter(us12, us12$response == "atheist"))/nrow(us12)

inference(us12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")
qnorm(0.95)


qnorm(0.975)


load("more/atheism.RData")
names(atheism)

#Q4
us12 <- subset(atheism, nationality == "United States" & year == "2012")
us12ath <- subset(atheism, nationality == "United States" & year == "2012" & response == "atheist")
nrow(us12ath)/nrow(us12)

#Q5
inference(us12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#Q6
SE = 0.0069
Z_score = 1.96
ME = SE * Z_score
ME

#Q7
chn12 <- subset(atheism, nationality == "China" & year == "2012")

inference(chn12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")


pkt12 <- subset(atheism, nationality == "Afghanistan" & year == "2012")

inference(pkt12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")






n <- 1000
p <- seq(0, 1, 0.01)
me <- 2 * sqrt(p * (1 - p)/n)




p <- 0.1
n <- 1040
p_hats <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}

hist(p_hats, main = "p = 0.1, n = 1040", xlim = c(0, 0.18))
plot(me ~ p, ylab = "Margin of Error", xlab = "Population Proportion")




summary(p_hats)
sd(p_hats)
boxplot(p_hats,y_lab="p_hats",x_lab="proportions")

#Q10
par(mfrow = c(2, 2))

#first histogram
hist(p_hats, main = "p = 0.1, n = 1040", xlim = c(0, 0.18))

p <- 0.1
n <- 400
p_hats2 <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats2[i] <- sum(samp == "atheist")/n
}

#second histogram
hist(p_hats2, main = "p = 0.1, n = 400", xlim = c(0, 0.18))

p <- 0.02
n <- 1040
p_hats3 <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats3[i] <- sum(samp == "atheist")/n
}

#third histogram
hist(p_hats3, main = "p = 0.02, n = 1040", xlim = c(0, 0.18))

p <- 0.02
n <- 400
p_hats4 <- rep(0, 5000)

for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats4[i] <- sum(samp == "atheist")/n
}

#fourth histogram
hist(p_hats4, main = "p = 0.02, n = 400", xlim = c(0, 0.18))

#On your own
spn05 <- subset(atheism, nationality == "Spain" & year == "2005")
spn12 <- subset(atheism, nationality == "Spain" & year == "2012")

inference(spn05$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")


inference(spn12$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#standard
p_spn05 = 0.1003
n_spn05 = 1146 
p_spn12 = 0.09
n_spn12 = 1145 

PE_spn = p_spn12 - p_spn05

SE_spn = sqrt((p_spn05*(1-p_spn05)/n_spn05)+(p_spn12*(1-p_spn12)/n_spn12))
SE_spn




usa05 <- subset(atheism, nationality == "United States" & year == "2005")
usa12 <- subset(atheism, nationality == "United States" & year == "2012")

inference(usa05$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")




#2
bul05 <- subset(atheism, nationality == "Bulgaria" & year == "2005")
bul12 <- subset(atheism, nationality == "Bulgaria" & year == "2012")

inference(bul05$response, est = "proportion", type = "ci", method = "theoretical", 
          success = "atheist")

#???3
SE = 0.01/1.96
p =0.018
n = (p*(1-p))/SE^2
ceiling(n)