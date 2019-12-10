library(ndl)
library(dplyr)

if (Sys.info()[1] == "Darwin"){
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  cgn_path = "/vol/tensusers/timzee/cgn/"
}

combined = read.csv(paste(cgn_path, "ndl_ifadv.csv", sep = ""))
combined2 = combined %>% count(Cues, Outcomes)
names(combined2) = c("Cues", "Outcomes", "Frequency")
head(combined2)
combined.w = estimateWeights(combined2)

saveRDS(combined.w, file = paste(cgn_path, "ifadv_ndl_weights.rds", sep = ""))
