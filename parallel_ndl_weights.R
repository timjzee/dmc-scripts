library(ndl)
library(dplyr)

if (Sys.info()[1] == "Darwin"){
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  cgn_path = "/vol/tensusers/timzee/cgn/"
}

combined = read.csv(paste(cgn_path, "ndl_ifadv.csv", sep = ""))
play = head(combined, 2)
play = play %>% count(Cues, Outcomes)
names(play) = c("Cues", "Outcomes", "Frequency")
play.w = estimateWeights(play, saveCounts = TRUE)
cooc_C = readRDS("play.coocCues.rds")
cooc_CO = readRDS("play.coocCuesOutcomes.rds")

play_col1 = data.frame(Cues = row.names(cooc_CO), 
                       Outcomes = rep(colnames(cooc_CO)[1], nrow(cooc_CO)),
                       Frequency = as.vector(cooc_CO[,1])
                       )

play_col1.w = estimateWeights(play_col1)

# columns not independent
# let's try rows

play_rows1 = data.frame(Cues = c(row.names(cooc_CO)[1:6], row.names(cooc_CO)[1:6]), 
                        Outcomes = c(rep(colnames(cooc_CO)[1], 6), rep(colnames(cooc_CO)[2], 6)),
                        Frequency = c(as.vector(cooc_CO[1:6,1]), as.vector(cooc_CO[1:6,2]))
)

play_rows1.w = estimateWeights(play_rows1)


ndl.w = readRDS(paste(cgn_path, "ifadv_ndl_weights.rds", sep = ""))
pyndl.w = t(as.matrix(read.csv(paste(cgn_path, "ifadv_pyndl_weights.csv", sep = ""), row.names = 1)))

# inspect ndl matrixes
library(sjPlot)
tab_df(as.data.frame(ndl.w[c(".iets.", "EUk", "ks", "d@"), c(".leuks.", ".bos.", "PART", "PL")]))

tab_df(as.data.frame(pyndl.w[c(".iets.", "EUk"), c(".leuks.", "PL")]))

