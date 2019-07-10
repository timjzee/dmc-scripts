library(ndl)

# Plag & Balling example
Word <- c("baptize", "chance", "extreme", "modernize", "optimal", 
          "optimize", "sand", "size")
Outcomes <- c("baptize", "chance", "extreme", "modern make", "optimal", 
              "optimal make", "sand", "size")
Frequency <- c(37, 12303, 558, 6, 15, 5, 1035, 2353)
ize <- data.frame(Word, Outcomes, Frequency)
ize$Cues <- orthoCoding(ize$Word, grams=2)
ize
ize.w = estimateWeights(ize)
round(ize.w, 3)
ize.a = estimateActivations(ize, ize.w)$activationMatrix
rownames(ize.a) <- ize[,"Word"]