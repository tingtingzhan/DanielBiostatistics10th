library(DanielBiostatistics10th)

# Example 10.3.1; Page 493 (10th ed), Page 419 (11th ed) 
head(EXA_C10_S03_01)
pairs(EXA_C10_S03_01, main = 'Figure 10.3.1')
summary(mod_1031 <- lm(CDA ~ AGE + EDLEVEL, data = EXA_C10_S03_01))

# Example 10.4.1; Page 502 (10th ed), Page 428 (11th ed) 
# .. see 'Multiple R-squared' (not 'Adjusted R-squared')

# Example 10.4.2; Page 504 (10th ed), Page 429 (11th ed) 
# .. see 'F-statistic'

# Example 10.4.3; Page 505 (10th ed), Page 430 (11th ed) 
# .. see 'Coefficients:'

# confidence interval for beta's; Page 506 (10th ed), Page 431 (11th ed) 
confint(mod_1031)

# Example 10.5.1; Page 509 (10th ed), Page 434 (11th ed) 
(newd_1031 = data.frame(AGE = 68, EDLEVEL = 12))
predict(mod_1031, newdata = newd_1031, interval = 'prediction')
predict(mod_1031, newdata = newd_1031, interval = 'confidence')

# Example 10.6.1; Page 511 (10th ed), Page 436 (11th ed) 
head(EXA_C10_S06_01)
pairs(EXA_C10_S06_01, main = 'Scatter Plot Matrix of Example 10.6.1')
summary(mod_1061 <- lm(W ~ P + S, data = EXA_C10_S06_01))
confint(mod_1061)

# Example 10.6.2; Page 515 (10th ed), Page 440 (11th ed)  
psych::partial.r(EXA_C10_S06_01, x = 2:3, y = 1L)
