library(DanielBiostatistics10th)

# Example 4.2.1-4.2.7; Page 93-97 (10th ed), Page 81-85 (11th ed)
(d421 = rep(1:8, times = c(62L, 47L, 39L, 39L, 58L, 37L, 4L, 11L)))
print_freqs(d421) # Table 4.2.1, 4.2.2, Table 4.2.3

# Example 4.2.8; Page 98 (10th ed), Page 85 (11th ed)
mean(d421)
sd(d421)
var(d421)

# ?dbinom # 'd' for binomial 'density'; calculate Prob(X = x)
# ?pbinom # 'p' for binomial 'probability' 
# `lower.tail = TRUE` (default), calculate Prob(X <= x)
# `lower.tail = FALSE`, calculate Prob(X > x)

# Example 4.3.1; Page 99 (10th ed)
dbinom(x = 3L, size = 5L, prob = .858)
# Example 4.3.1; Page 87 (11th ed) 
dbinom(x = 3L, size = 5L, prob = .899)

# Example 4.3.2; Page 103 (10th ed), Page 90 (11th ed)
dbinom(x = 4L, size = 10L, prob = .14)

# Example 4.3.3; Page 103 (10th ed), Page 91 (11th ed)
(pL = pbinom(q = 5L, size = 25L, prob = .1, lower.tail = TRUE)) # (a) P(X <= 5L)
(pU = pbinom(q = 5L, size = 25L, prob = .1, lower.tail = FALSE)) # (b) P(X > 5L)
pL + pU # = 1

# Example 4.3.4; Page 105 (10th ed), Page 92 (11th ed) 
dbinom(x = 7L, size = 12L, prob = .55)
pbinom(q = 5L, size = 12L, prob = .55)
pbinom(q = 7L, size = 12L, prob = .55, lower.tail = FALSE)

# Example 4.4.1; Page 110 (10th ed), Page 97 (11th ed) 
dpois(x = 3L, lambda = 12) 

# Example 4.4.2; Page 110 (10th ed), Page 98 (11th ed) 
ppois(q = 2L, lambda = 12, lower.tail = FALSE)

# Example 4.4.3; Page 110 (10th ed), Page 98 (11th ed) 
ppois(q = 1L, lambda = 2) 

# Example 4.4.4; Page 111 (10th ed), Page 98 (11th ed) 
dpois(x = 3L, lambda = 2)

# Example 4.4.5; Page 112 (10th ed), Page 98 (11th ed) 
ppois(q = 5L, lambda = 2, lower.tail = FALSE)

# Example 4.6.1; Page 119 (10th ed), Page 106 (11th ed) 
pnorm(q = 2)

# Example 4.6.2; Page 120 (10th ed), Page 106 (11th ed) 
pnorm(2.55) - pnorm(-2.55)
1 - 2 * pnorm(-2.55) # alternative solution

# Example 4.6.3; Page 121 (10th ed), Page 107 (11th ed) 
pnorm(1.53) - pnorm(-2.74)

# Example 4.6.4; Page 121 (10th ed), Page 107 (11th ed) 
pnorm(2.71, lower.tail = FALSE)

# Example 4.6.5; Page 122 (10th ed), Page 107 (11th ed) 
pnorm(2.45) - pnorm(.84)

# Example 4.7.1; Page 122 (10th ed), Page 109 (11th ed) 
pnorm(q = 3, mean = 5.4, sd = 1.3)
pnorm(q = (3-5.4)/1.3) # manual solution

# Example 4.7.2; Page 125 (10th ed), Page 111 (11th ed) 
pnorm(q = 649, mean = 491, sd = 119) - pnorm(q = 292, mean = 491, sd = 119)

# Example 4.7.3; Page 122 (10th ed), Page 111 (11th ed) 
1e4L * pnorm(q = 8.5, mean = 5.4, sd = 1.3, lower.tail = FALSE)
