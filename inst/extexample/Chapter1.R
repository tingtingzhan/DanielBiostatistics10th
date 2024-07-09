library(DanielBiostatistics10th)
# To run a line of code, use shortcut
# Command + Enter: Mac and RStudio Cloud
# Control + Enter: Windows, Mac and RStudio Cloud
# To clear the console
# Control + L: Windows, Mac and RStudio Cloud

# Example 1.4.1; Page 8 (10th ed), Page 7 (11th ed)
class(EXA_C01_S04_01) # `EXA_C01_S04_01` is a 'data.frame' (a specific class defined in R)
dim(EXA_C01_S04_01) # dimension, number-row and number-column
head(EXA_C01_S04_01, n = 8L) # first `n` rows of a 'data.frame'
names(EXA_C01_S04_01) # column names of a 'data.frame'
EXA_C01_S04_01$AGE # use `$` to obtain one column from a 'data.frame' 
sampleRow(EXA_C01_S04_01, size = 10L, replace = FALSE) # to answer Example 1.4.1

# Example 1.4.2; Page 11 (10th ed), Page 10 (11th ed)
EXA_C01_S04_01[seq.int(from = 4L, to = 166L, by = 18L), ]