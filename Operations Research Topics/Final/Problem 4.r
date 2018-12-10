# R code for DS 775 Final, Problem 4
# Chris Austin

set.seed(40)
# Since the percentages are round two-decimals, this way works ok. Otherwise,
# with longer-decimal probabilities, a cumulative probability function, a random
# number generator from 0 to 1, and a ceiling or floor function to get the right
# number of sales would be necessary.
saleProbSample <- c(rep(10,5), rep(11,5), rep(12,10), rep(13,15), rep(14,20), 
                    rep(15,20), rep(16,10), rep(17,10), rep(18,5))

saleSample <- sample(saleProbSample, 10000, replace = TRUE)
mean(saleSample)
# mean ticket sale count with seed = 40 and 10000 runs is 14.221

ticketCost <- 100
ticketPrice <- 130

##############################################################################
# Question 14 - If Billy buys 17 tickets, what is his mean profit?
##############################################################################
saleSample17 <- saleSample
# Can't re-sell more than you buy...
saleSample17[saleSample > 17] <- 17
mean17Profit <- ticketPrice*mean(saleSample17) - 17*ticketCost
# 142.01

##############################################################################
# Question 15 - If Billy buys 17 tickets, what is the probability that his
# profit is at least 0?
##############################################################################

profitAt17 <- ticketPrice*saleSample17 - 17*ticketCost
# has profit values for each simulated pass of ticket sales

sprintf("Zero profit occurs around 35.0 and 35.1 Percent: %f , %f", 
        quantile(profitAt17, .350), quantile(profitAt17, .351))
print("Profit at least zero is 1 minus that, or 65%.")

##############################################################################
# Question 16 - Do a parameter analysis on all nine possible quantities of
# tickets to purchase between 10 and 18.  Which purchase quantity maximizes
# Billy’s mean profit?
##############################################################################

#saleSample10 <- saleSample
#saleSample11 <- saleSample
saleSample12 <- saleSample
saleSample13 <- saleSample
saleSample14 <- saleSample
saleSample15 <- saleSample
saleSample16 <- saleSample

# Again, can't re-sell more than you buy. Set sales over that to the
# number that were bought.

saleSample10[saleSample > 10] <- 10
mean10Profit <- ticketPrice*mean(saleSample10) - 10*ticketCost

saleSample11[saleSample > 11] <- 11
mean11Profit <- ticketPrice*mean(saleSample11) - 11*ticketCost

saleSample12[saleSample > 12] <- 12
mean12Profit <- ticketPrice*mean(saleSample12) - 12*ticketCost

saleSample13[saleSample > 13] <- 13
mean13Profit <- ticketPrice*mean(saleSample13) - 13*ticketCost

saleSample14[saleSample > 14] <- 14
mean14Profit <- ticketPrice*mean(saleSample14) - 14*ticketCost

saleSample15[saleSample > 15] <- 15
mean15Profit <- ticketPrice*mean(saleSample15) - 15*ticketCost

saleSample16[saleSample > 16] <- 16
mean16Profit <- ticketPrice*mean(saleSample16) - 16*ticketCost

mean18Profit <- ticketPrice*mean(saleSample) - 18*ticketCost

rbind(mean10Profit, mean11Profit, mean12Profit, mean13Profit, mean14Profit, mean15Profit, 
      mean16Profit, mean17Profit, mean18Profit)

# Max profit occurs at 13 tickets purchased.

##############################################################################
# Question 17 - If Billy buys the number of tickets that maximizes his mean 
# profit, then what is the probability he makes at  least $200?
##############################################################################
profitAt13 <- ticketPrice*saleSample13 - 13*ticketCost

sprintf("$200 profit occurs around 9.6 and 9.7 Percent: %f , %f", 
        quantile(profitAt13, .096), quantile(profitAt13, .097))
print("Profit at least zero is 1 minus that, or 90.4%.")