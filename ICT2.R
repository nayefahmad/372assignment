data <- read.csv("ictdata2csv.csv", stringsAsFactors = FALSE, header = TRUE)
bady <- data[ ,3]
badx1 <- data[ ,2]
good <- complete.cases(badx1, bady)
y <- bady[good]
x1 <- badx1[good]

par(mfrow = c(2,2))

###################### FIRST REGRESSION ###################

plot(x1, y, xlab = "mbps", ylab = "htexp", main = "Figure 1: Scatterplot of htexp vs. mbps")

cor(y, x1)

fit <- lm(y~x1)
abline(fit, col = "red")
summary(fit) # R2 = 0.4739; x1 = ***, c=*

# Let's look at residual plots: 
r1 <- resid(fit)

plot(y, r1, xlab = "fitted", ylab = "resid",main = "Figure 5: residuals vs. fitted for \n htexp ~ mbps")
abline(h=0)
# this doesn't look good; it's obvious that errors are not 
# stocastic; there is a detectable relationship with fitted y values 
# logy transformation will be attempted next 


##################### SECOND REGRESSION ###################3
logy <- log(y)

plot(x1, logy, xlab = "mbps", ylab = "log(htexp)", main = "Figure 2: Scatterplot of log(htexp) \n vs. mbps ", cex.main = 1.2)

cor(x1, logy)
# This is far worse than Figure 1


fit2 <- lm(logy~x1)
abline(fit2, col = "red")

summary(fit2) # R2 = 0.2094; x1 = ***, c=***

# Let's look at residual plots:
r2 <- resid(fit2)

plot(logy, r2, xlab = "fitted", ylab = "resid", main = "Figure 6:Residual vs. Fitted for \n log(htexp) ~ mbps", cex.main = 1.2)
abline(h = 0)
# This is truly horrific :(

plot(x1, r2, xlab = "mbps", ylab = "resid", main = "Figure 7: Residual vs. mbps for \n log(htexp) ~ mbps", cex.main = 1.2)
# The stuff of nightmares

#################### THIRD REGRESSION ######################

logx1 <- log(x1)
logy <- log(y)

plot(logx1, logy, xlab = "log(mbps)", ylab = "log(htexp)",main = "Figure 3: Scatterplot of log(htexp) \n vs. log(mbps)", cex.main = 1.2)
cor(logx1, logy)

# compare with Figure 1, starting to look better 
# Note that this indicates that dummies may be appropriate: seems 
# like you could fit more than one line with the same slope, 
# but different intercepts (see )

fit3 <- lm(logy~logx1)
summary(fit3) # R2 = 0.5643, x1 = ***, c=*** 

abline(fit3, col = "red")

# look at residuals: 
r3 <- resid(fit3)

plot(logy, r3, xlab = "fitted", ylab = "resid", main = "Figure 8: Residual vs. Fitted for \n log(htexp) ~ log(mbps)", cex.main = 1.2)
abline(h = 0)

plot(logx1, r3, xlab = "log(mbps)", ylab = "resid", main = "Figure 9: Residual vs. log(mbps) for \n log(htexp) ~ log(mbps)", cex.main = 1.2)
abline(h = 0)
# Huge improvement over Figures 4 and 5, but still a lot of
# information being retained in the residuals. The model is 
# under-specified. Obviously it would be very challenging to 
# find a complete regression model to describe the level of high-tech 
# exports of a country. 

# One possible additional variable: OECD or not. 


################### FOURTH REGRESSION: dummy ####################
logy <- log(y)
logx1 <- log(x1)
# These were actually defined above before

oecd <- as.numeric(data[ ,4])

plot(oecd, logy) # shows there might be some justification 
# for including oecd in explaining logy [see below for more]


fit4 <- lm(logy~logx1+oecd)
summary(fit4) # R2 = 0.5607; x1 = ***; oecd not sig. 
# The fit has gotten worse after adding OECD, and it's not 
# significant. 

# What about residuals? 
r4 <- resid(fit4)

plot(logy, r4, xlab = "fitted", ylab = "resid", main = "Figure 10: Residual vs. Fitted for \n log(htexp) ~ log(mbps) + oecd", cex.main = 1.2)
abline(h = 0)

plot(logx1, r4, xlab = "log(mbps)", ylab = "resid", main = "Figure 11: Residual vs. log(mbps) \n for log(htexp) ~ log(mbps) + oecd", cex.main = 1.2)
abline(h = 0)
# These two residual plots (Figures 10 and 11) are identical to 
# Figures 7 and 8. This is more evidence that oecd adds no 
# useful information (?)

plot(oecd, r4, xlab = "oecd", ylab = "resid" , main = "Figure 12: Residual vs. oecd for \n log(htexp) ~ log(mpbs) + oecd", cex.main = 1.2)
abline(h = 0)



####################################################
# Justification for trying oecd as a variable: 

fit5 <- lm(logy~oecd)
summary(fit5) # R2 = 0.2642; low but not disastrously low 

plot(oecd, logy, ylab = "log(htexp)",main = "Figure 13: Scatterplot of \n log(htexp) vs. oecd")
abline(fit5, col = "red")
# Again, this shows there might be some justification 
# for including oecd in explaining logy 

r5 <- resid(fit5)

plot(logy, r5, xlab = "fitted", ylab = "resid",main = "Figure 13: Residuals vs. fitted for \n log(htexp) ~ oecd") # this is disastrous

# Lines 80-84 show that even though it seems relatively sensible
# to regress logy against oecd, and the R2 is not incredibly bad
# (2.642), it is still definitely not a valid model. The reason 
# for this is that the residuals are definitely not stochastic



############################################################
# Final model: 
# log(hitechexp) = 10.63 + 1.13*log(mbps) 
# This means that for every 1% increase in international bandwidth, 
# there is a 1.13% increase in high-tech exports, all else being 
# equal. 

# Further research will be required to add variables to improve 
# fit and ensure that residuals are well-behaved. 


##################################  THINGS TO ADD:

hist(x1)
hist(logx1)

hist(y)
hist(logy)

hist(r4)

hist(r3, main = " Histogram of residuals \n from Model 3")

hist(r5)
# evidence for normality of errors in all three cases 
