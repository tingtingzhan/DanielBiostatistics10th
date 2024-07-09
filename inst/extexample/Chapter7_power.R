library(DanielBiostatistics10th)

# Example 7.9.1; Page 272 (10th ed), Page 245 (11th ed) 
(p791 = power_z(seq.int(from = 16, to = 19, by = .5), null.value = 17.5, sd = 3.6, n = 100L))
# Table 7.9.1

# Example 7.9.2; Page 276 (10th ed), Page 248 (11th ed) 
(p792 = power_z(seq.int(from = 50, to = 70, by = 5), null.value = 65, sd = 15, n = 20L, 
                sig.level = .01, alternative = 'less'))

# Example 7.10.1; Page 278 (10th ed), 
(n_d7101 <- uniroot(f = function(x) {
  power_z(55, null.value = 65, sd = 15, n = x, sig.level = .01, alternative = 'less')$power - .95
}, interval = c(0, 50))$root)
power_z(55, null.value = 65, sd = 15, n = ceiling(n_d7101), sig.level = .01, alternative = 'less')
