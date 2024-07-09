library(DanielBiostatistics10th)

# Example 2.2.1; Page 20 (10th ed), Page 19 (11th ed)
head(EXA_C01_S04_01)
class(age <- EXA_C01_S04_01$AGE) # 'integer'
sort(age) # Table 2.2.1

# Example 2.3.1; Page 23 (10th ed); Page 22 (11th ed)
(r231 = print_freqs(age, breaks = seq.int(from=30, to=90, by=10), right = FALSE)) # Table 2.3.2
# The open/close of interval ends is determined by textbook using 30-39, 40-49, etc.

# Example 2.4.1-2.4.6; Page 38-42 (10th ed); Page 30-34 (11th ed)
# Example 2.5.1-2.5.3; Page 44-46 (10th ed); Page 35-41 (11th ed)
print_stats(age)

# Example 2.5.4 (omitted); Page 49 (10th ed), Page 41 (11th ed)

# Example 2.5.5; Page 50 (10th ed)
head(EXA_C02_S05_05)
boxplot(EXA_C02_S05_05$GRF, main = c('Example 2.5.5 (10th ed)'))
print_stats(EXA_C02_S05_05$GRF)
print_freqs(EXA_C02_S05_05$GRF, breaks = seq.int(10, 45, by = 5))

