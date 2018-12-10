# 16-2-5c

# Conservative investment
set1 <- c(-6,7.5)
# Speculative Investment
set2 <- c(-23,13)
# Counter-cylical investment
set3 <- c(12.5,-1)
# cumulative x values
xval <- c(.0, .9)

plot(x = xval, y=set1, type = "n", xlab = "Prior Probability (p) of Stable Economy, p: 0.0 to 0.9",
     main = "Bayes Decision Rule with Odds of Improving Economy Fixed at .1",
     ylab = "Expected Profit", xlim = c(0,1), ylim = c(-25,16))
points(x = xval, y = set1, pch = 20, type = "p", col = "red")
lines(x = xval, y = set1, pch = 20, col = "red")

points(x = xval, y = set2, pch = 20, type = "p", col = "blue")
lines(x = xval, y = set2, pch = 20, col = "blue")

points(x = xval, y = set3, pch = 20, type = "p", col = "green")
lines(x = xval, y = set3, pch = 20, col = "green")

legend("bottomright", legend = c("Conservative", "Speculative", "Counter-cyclical"), title = "Alternatives",
       pch = 20, col = c("red", "blue", "green"))

abline(v = 18.5/30, lty = 2)
abline(v = 17/25, lty = 2)
text(x = 18.5/30, y = 14, sprintf("p = %.3f", 18.5/30), adj = c(1.2, 0))
text(x = 17/25, y = 14, sprintf("p = %.3f", 17/25), adj = c(-.2, 0))
text(x = .1, y = 3, "Counter-cyclical")
text(x = .65, y = -15, "Conservative")
text(x = .85, y = -9, "Speculative")

# 12.2.6d
y1 <- c(164,209)
y2 <- c(177,195)
xval <- c(.0, .9)

plot(x = xval, y=y2, type = "n", xlab = "Prior Probability (p) of states S1 and S2, p: 0.0 to 0.9",
     main = "Bayes Decision Rule with Odds of State S3 Fixed at 0.1",
     ylab = "Expected Profit", ylim = c(160,215))
points(x = xval, y = y1, pch = 20, type = "p", col = "red")
lines(x = xval, y = y1, pch = 20, col = "red")
points(x = xval, y = y2, pch = 20, type = "p", col = "blue")
lines(x = xval, y = y2, pch = 20, col = "blue")
abline(v = 13/30, lty = 2)
text(x = 13/30, y = 192, sprintf("p = %.3f", 13/30), adj = c(1.2, 0))
legend("bottomright", legend = c("Alternative 1", "Alternative 2"), title = "Alternatives",
       pch = 20, col = c("red", "blue"))

# 12.2.6e
y1 <- c(128,205)
y2 <- c(159,194)
xval <- c(.0, .7)

plot(x = xval, y=y2, type = "n", xlab = "Prior Probability (p) of states S1 and S3, p: 0.0 to 0.7",
     main = "Bayes Decision Rule with Odds of State S2 Fixed at 0.3",
     ylab = "Expected Profit", ylim = c(120,215))
points(x = xval, y = y1, pch = 20, type = "p", col = "red")
lines(x = xval, y = y1, pch = 20, col = "red")
points(x = xval, y = y2, pch = 20, type = "p", col = "blue")
lines(x = xval, y = y2, pch = 20, col = "blue")
abline(v = 31/60, lty = 2)
text(x = 31/60, y = 210, sprintf("p = %.3f", 31/60), adj = c(1.2, 0))
legend("bottomright", legend = c("Alternative 1", "Alternative 2"), title = "Alternatives",
       pch = 20, col = c("red", "blue"))

# 12.2.6f
y1 <- c(176,200)
y2 <- c(180,192)
xval <- c(.0, .4)

plot(x = xval, y=y2, type = "n", xlab = "Prior Probability (p) of states S2 and S3, p: 0.0 to 0.4",
     main = "Bayes Decision Rule with Odds of State S1 Fixed at 0.6",
     ylab = "Expected Profit", ylim = c(170,210))
points(x = xval, y = y1, pch = 20, type = "p", col = "red")
lines(x = xval, y = y1, pch = 20, col = "red")
points(x = xval, y = y2, pch = 20, type = "p", col = "blue")
lines(x = xval, y = y2, pch = 20, col = "blue")
abline(v = 4/30, lty = 2)
text(x = 4/30, y = 205, sprintf("p = %.3f", 4/30), adj = c(1.2, 0))
legend("bottomright", legend = c("Alternative 1", "Alternative 2"), title = "Alternatives",
       pch = 20, col = c("red", "blue"))
