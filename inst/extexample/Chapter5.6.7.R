library(DanielBiostatistics10th)

# Example 5.3.2; Page 142 (10th ed)
aggregated_z(xbar = 190, sd = 12.7, n = 10L, null.value = 185.6, alternative = 'greater')

# Example 5.3.3; 
# Page 143 (10th ed)
pnorm(125, mean = 120, sd = 15/sqrt(50)) - pnorm(115, mean = 120, sd = 15/sqrt(50))
aggregated_z(125, sd = 15, n = 50L, null.value = 120, alternative = 'less')$p.value - 
  aggregated_z(115, sd = 15, n = 50L, null.value = 120, alternative = 'less')$p.value
# Page 127 (11th ed)
aggregated_z(110, sd = 20, n = 50L, null.value = 100, alternative = 'less')$p.value - 
  aggregated_z(95, sd = 20, n = 50L, null.value = 100, alternative = 'less')$p.value

# Example 5.4.1; Page 145 (10th ed), Page 129 (11th ed) 
aggregated_z(xbar = c(92, 105), sd = 20, n = 15L, null.value = 0, alternative = 'less') 

# Example 5.4.2; Page 148 (10th ed), 
aggregated_z(xbar = 20, sd = c(15, 20), n = c(35L, 40L), null.value = c(45, 30), 
  alternative = 'greater')

# Example 5.5.1; Page 150 (10th ed), 
prop_CLT(xbar = .4, n = 150L, null.value = .357, alternative = 'greater')

# Example 5.5.2; Page 152 (10th ed), 
prop_CLT(xbar = .45, n = 200L, null.value = .51, alternative = 'less')

# Example 5.6.1; Page 155 (10th ed), 
prop_CLT(xbar = .1, null.value = c(.28, .21), n = c(100L, 100L), alternative = 'greater')

# Example 5.6.2; Page 155 (10th ed), 
prop_CLT(xbar = .05, null.value = c(.34, .26), n = c(250L, 200L), alternative = 'less')

# Example 6.2.1; Page 166 (10th ed), Page 147 (11th ed)
aggregated_z(xbar = 22, n = 10L, sd = sqrt(45))

# Example 6.2.2; Page 168 (10th ed), Page 149 (11th ed)
aggregated_z(xbar = 84.3, n = 15L, sd = sqrt(144), conf.level = .99)

# Example 6.2.3; Page 168 (10th ed), Page 150 (11th ed)
aggregated_z(xbar = 17.2, n = 35L, sd = 8, conf.level = .9)

# Example 6.2.4; Page 169 (10th ed), Page 150 (11th ed)
head(EXA_C06_S02_04)
aggregated_z(xbar = mean(EXA_C06_S02_04$ACTIVITY), n = nrow(EXA_C06_S02_04), sd = sqrt(.36))

# Example 6.3.1; Page 173 (10th ed), 
aggregated_t(xbar = 250.8, xsd = 130.9, n = 19L)

# Example 6.4.1; Page 177 (10th ed), 
aggregated_z(xbar = c(4.5, 3.4), sd = sqrt(c(1, 1.5)), n = c(12L, 15L))

# Example 6.4.2; Page 178 (10th ed), 
aggregated_z(xbar = c(4.3, 13), sd = c(5.22, 8.97), n = c(328L, 64L), conf.level = .99)

# Example 6.4.3; Page 180 (10th ed), 
aggregated_t(xbar = c(4.7, 8.8), xsd = c(9.3, 11.5), n = c(18L, 10L), var.equal = TRUE)

# Example 6.4.4; Page 181 (10th ed), 
aggregated_t(xbar = c(4.7, 8.8), xsd = c(9.3, 11.5), n = c(18L, 10L)) 
# Welch slightly different from Cochran; textbook explained on Page 182

# Example 6.5.1; Page 185 (10th ed), 
prop_CLT(xbar = .18, n = 1220L)

# Example 6.6.1; Page 187 (10th ed), 
prop_CLT(x = c(31L, 53L), n = c(68L, 255L), conf.level = .99)

# Example 6.7.1; Page 190 (10th ed), 
(n_671 = uniroot(f = function(n, sd, level = .95) {
  qnorm(1-(1-level)/2) * sd/sqrt(n) - 5 # half-width of CI <= 5 grams
}, interval = c(0, 2e2), sd = 20)$root)
ceiling(n_671)

# Example 6.8.1; Page 192 (10th ed), 
(n_681 = uniroot(f = function(n, p, level = .95) {
  qnorm(1-(1-level)/2) * sqrt(p*(1-p)/n) - .05
}, interval = c(0, 1e3), p = .35)$root)
ceiling(n_681)

# Example 6.9.1; Page 196 (10th ed), 
d691 = c(9.7, 12.3, 11.2, 5.1, 24.8, 14.8, 17.7)
sqrt(aggregated_var(xsd = sd(d691), n = length(d691)))

# Example 6.10.1; Page 200 (10th ed), 
aggregated_var(xsd = c(8.1, 5.9), n = c(16L, 4L))

# Example 7.2.1; Page 222 (10th ed); Page 201 (11th ed)
aggregated_z(xbar = 27, sd = sqrt(20), n = 10L, null.value = 30)

# Example 7.2.2; Page 226 (10th ed); Page 204 (11th ed)
aggregated_z(xbar = 27, sd = sqrt(20), n = 10L, null.value = 30, alternative = 'less')

# Example 7.2.3; Page 228 (10th ed); Page 206 (11th ed)
head(EXA_C07_S02_03)
t.test(EXA_C07_S02_03$DAYS, mu = 15)

# Example 7.2.4; Page 231 (10th ed); Page 209 (11th ed)
aggregated_z(xbar = 146, sd = 27, n = 157L, null.value = 140, alternative = 'greater')

# Example 7.2.5; Page 232 (10th ed); Page 210 (11th ed)
d725 = c(33.38, 32.15, 34.34, 33.95, 33.46, 34.13, 33.99, 34.10, 33.85, 
  34.23, 34.45, 34.19, 33.97, 32.73, 34.05)
t.test(d725, mu = 34.5)

# Example 7.3.1; Page 237 (10th ed), Page 213 (11th ed) 
aggregated_z(xbar = c(4.5, 3.4), sd = sqrt(c(1, 1.5)), n = c(12L, 15L), null.value = 0)

# Example 7.3.2; Page 239 (10th ed), Page 215 (11th ed) 
head(EXA_C07_S03_02)
with(EXA_C07_S03_02, t.test(x = CONTROL, y = SCI, alternative = 'less', var.equal = TRUE))

# Example 7.3.3; Page 240 (10th ed), Page 217 (11th ed) 
aggregated_t(xbar = c(19.16, 9.53), xsd = c(5.29, 2.69), n = c(15L, 30L), null.value = 0)

# Example 7.3.4; Page 242 (10th ed), Page 219 (11th ed) 
aggregated_z(xbar = c(59.01, 46.61), sd = c(44.89, 34.85), n = c(53L, 54L), null.value = 0,
  alternative = 'greater')

# Example 7.4.1; Page 251 (10th ed), Page 226 (11th ed) 
head(EXA_C07_S04_01)
with(EXA_C07_S04_01, t.test(x = POSTOP, y = PREOP, alternative = 'greater', paired = TRUE))

# Example 7.5.1; Page 258 (10th ed), Page 232 (11th ed) 
prop_CLT(x = 24L, n = 301L, null.value = .063, alternative = 'greater') 

# Example 7.6.1; Page 261 (10th ed), Page 235 (11th ed) 
prop_CLT(x = c(24L, 11L), n = c(44L, 29L), null.value = 0, alternative = 'greater')

# Example 7.7.1; Page 264 (10th ed), Page 238 (11th ed) 
head(EXA_C07_S07_01)
aggregated_var(xsd = sd(EXA_C07_S07_01$mass), n = 16L, null.value = 600)

# Example 7.8.1; Page 268 (10th ed), Page 242 (11th ed) 
aggregated_var(xsd = c(30.62, 11.37), n = 6L, null.value = 1, alternative = 'greater')

# Example 7.8.2; Page 270 (10th ed), 
with(EXA_C07_S03_02, var.test(x = CONTROL, y = SCI))
