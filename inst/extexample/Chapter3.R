library(DanielBiostatistics10th)

# Example 3.4.1-3.4.9; Page 69-75 (10th ed), Page 61-67 (11th ed)
(d341 = matrix(c(28L, 19L, 41L, 53L, 35L, 38L, 44L, 60L), ncol = 2L, dimnames = list(
  FamilyHx = c('none', 'Bipolar', 'Unipolar', 'UniBipolar'), Onset = c('Early', 'Late'))))
class(d341) # 'matrix', i.e., a two-dimensional 'array'
addProbs(d341)
addProbs(d341, margin = 2L)
addProbs(d341, margin = 1L)

# Example 3.5.1; Page 81 (10th ed), Page 72 (11th ed)
(d351 = matrix(c(495L, 14L, 5L, 436L), nrow = 2L, dimnames = list(
  Alzheimer = c('No', 'Yes'), Test = c('Negative', 'Positive'))))
print(binTab(d351), prevalence = .113)
