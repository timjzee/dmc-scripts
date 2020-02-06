if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/grid_search/"
} else {
  f_path = "/vol/tensusers/timzee/grid_search/"
}

gs01 = read.csv(paste(f_path, "gs01_measures.csv", sep = ""))
gs02 = read.csv(paste(f_path, "gs02_measures.csv", sep = ""))
gs03 = read.csv(paste(f_path, "gs03_measures.csv", sep = ""))
gs04 = read.csv(paste(f_path, "gs04_measures.csv", sep = ""))
gs05 = read.csv(paste(f_path, "gs05_measures.csv", sep = ""))
gs06 = read.csv(paste(f_path, "gs06_measures.csv", sep = ""))
gs07 = read.csv(paste(f_path, "gs07_measures.csv", sep = ""))
gs08 = read.csv(paste(f_path, "gs08_measures.csv", sep = ""))
gs09 = read.csv(paste(f_path, "gs09_measures.csv", sep = ""))
gs10 = read.csv(paste(f_path, "gs10_measures.csv", sep = ""))
gs11 = read.csv(paste(f_path, "gs11_measures.csv", sep = ""))
gs12 = read.csv(paste(f_path, "gs12_measures.csv", sep = ""))
gs13 = read.csv(paste(f_path, "gs13_measures.csv", sep = ""))
gs14 = read.csv(paste(f_path, "gs14_measures.csv", sep = ""))
gs15 = read.csv(paste(f_path, "gs15_measures.csv", sep = ""))
gs16 = read.csv(paste(f_path, "gs16_measures.csv", sep = ""))
gs17 = read.csv(paste(f_path, "gs17_measures.csv", sep = ""))
gs18 = read.csv(paste(f_path, "gs18_measures.csv", sep = ""))
gs19 = read.csv(paste(f_path, "gs19_measures.csv", sep = ""))
gs20 = read.csv(paste(f_path, "gs20_measures.csv", sep = ""))
gs21 = read.csv(paste(f_path, "gs21_measures.csv", sep = ""))
gs22 = read.csv(paste(f_path, "gs22_measures.csv", sep = ""))
gs23 = read.csv(paste(f_path, "gs23_measures.csv", sep = ""))
gs24 = read.csv(paste(f_path, "gs24_measures.csv", sep = ""))



mean_dist = c(gs01["Mean Distance", "combined"], gs02["Mean Distance", "combined"],
              gs03["Mean Distance", "combined"], gs04["Mean Distance", "combined"],  
              gs05["Mean Distance", "combined"], gs06["Mean Distance", "combined"], 
              gs07["Mean Distance", "combined"], gs08["Mean Distance", "combined"], 
              gs09["Mean Distance", "combined"], gs10["Mean Distance", "combined"], 
              gs11["Mean Distance", "combined"], gs12["Mean Distance", "combined"], 
              gs13["Mean Distance", "combined"], gs14["Mean Distance", "combined"], 
              gs15["Mean Distance", "combined"], gs16["Mean Distance", "combined"], 
              gs17["Mean Distance", "combined"], gs18["Mean Distance", "combined"], 
              gs19["Mean Distance", "combined"], gs20["Mean Distance", "combined"], 
              gs21["Mean Distance", "combined"], gs22["Mean Distance", "combined"], 
              gs23["Mean Distance", "combined"], gs24["Mean Distance", "combined"])
sil_F1 = c(gs01["SIL F-measure", "combined"], gs02["SIL F-measure", "combined"],
           gs03["SIL F-measure", "combined"], gs04["SIL F-measure", "combined"],  
           gs05["SIL F-measure", "combined"], gs06["SIL F-measure", "combined"], 
           gs07["SIL F-measure", "combined"], gs08["SIL F-measure", "combined"], 
           gs09["SIL F-measure", "combined"], gs10["SIL F-measure", "combined"], 
           gs11["SIL F-measure", "combined"], gs12["SIL F-measure", "combined"], 
           gs13["SIL F-measure", "combined"], gs14["SIL F-measure", "combined"], 
           gs15["SIL F-measure", "combined"], gs16["SIL F-measure", "combined"], 
           gs17["SIL F-measure", "combined"], gs18["SIL F-measure", "combined"], 
           gs19["SIL F-measure", "combined"], gs20["SIL F-measure", "combined"], 
           gs21["SIL F-measure", "combined"], gs22["SIL F-measure", "combined"], 
           gs23["SIL F-measure", "combined"], gs24["SIL F-measure", "combined"])
SIL_start_diff = c(gs01["SIL start diff", "combined"], gs02["SIL start diff", "combined"],
                   gs03["SIL start diff", "combined"], gs04["SIL start diff", "combined"],  
                   gs05["SIL start diff", "combined"], gs06["SIL start diff", "combined"], 
                   gs07["SIL start diff", "combined"], gs08["SIL start diff", "combined"], 
                   gs09["SIL start diff", "combined"], gs10["SIL start diff", "combined"], 
                   gs11["SIL start diff", "combined"], gs12["SIL start diff", "combined"], 
                   gs13["SIL start diff", "combined"], gs14["SIL start diff", "combined"], 
                   gs15["SIL start diff", "combined"], gs16["SIL start diff", "combined"], 
                   gs17["SIL start diff", "combined"], gs18["SIL start diff", "combined"], 
                   gs19["SIL start diff", "combined"], gs20["SIL start diff", "combined"], 
                   gs21["SIL start diff", "combined"], gs22["SIL start diff", "combined"], 
                   gs23["SIL start diff", "combined"], gs24["SIL start diff", "combined"])
SIL_end_diff = c(gs01["SIL end diff", "combined"], gs02["SIL end diff", "combined"],
                 gs03["SIL end diff", "combined"], gs04["SIL end diff", "combined"],  
                 gs05["SIL end diff", "combined"], gs06["SIL end diff", "combined"], 
                 gs07["SIL end diff", "combined"], gs08["SIL end diff", "combined"], 
                 gs09["SIL end diff", "combined"], gs10["SIL end diff", "combined"], 
                 gs11["SIL end diff", "combined"], gs12["SIL end diff", "combined"], 
                 gs13["SIL end diff", "combined"], gs14["SIL end diff", "combined"], 
                 gs15["SIL end diff", "combined"], gs16["SIL end diff", "combined"], 
                 gs17["SIL end diff", "combined"], gs18["SIL end diff", "combined"], 
                 gs19["SIL end diff", "combined"], gs20["SIL end diff", "combined"], 
                 gs21["SIL end diff", "combined"], gs22["SIL end diff", "combined"], 
                 gs23["SIL end diff", "combined"], gs24["SIL end diff", "combined"])
Word_start_diff = c(gs01["Word start diff", "combined"], gs02["Word start diff", "combined"],
                    gs03["Word start diff", "combined"], gs04["Word start diff", "combined"],  
                    gs05["Word start diff", "combined"], gs06["Word start diff", "combined"], 
                    gs07["Word start diff", "combined"], gs08["Word start diff", "combined"], 
                    gs09["Word start diff", "combined"], gs10["Word start diff", "combined"], 
                    gs11["Word start diff", "combined"], gs12["Word start diff", "combined"], 
                    gs13["Word start diff", "combined"], gs14["Word start diff", "combined"], 
                    gs15["Word start diff", "combined"], gs16["Word start diff", "combined"], 
                    gs17["Word start diff", "combined"], gs18["Word start diff", "combined"], 
                    gs19["Word start diff", "combined"], gs20["Word start diff", "combined"], 
                    gs21["Word start diff", "combined"], gs22["Word start diff", "combined"], 
                    gs23["Word start diff", "combined"], gs24["Word start diff", "combined"])
Word_end_diff = c(gs01["Word end diff", "combined"], gs02["Word end diff", "combined"],
                  gs03["Word end diff", "combined"], gs04["Word end diff", "combined"],  
                  gs05["Word end diff", "combined"], gs06["Word end diff", "combined"], 
                  gs07["Word end diff", "combined"], gs08["Word end diff", "combined"], 
                  gs09["Word end diff", "combined"], gs10["Word end diff", "combined"], 
                  gs11["Word end diff", "combined"], gs12["Word end diff", "combined"], 
                  gs13["Word end diff", "combined"], gs14["Word end diff", "combined"], 
                  gs15["Word end diff", "combined"], gs16["Word end diff", "combined"], 
                  gs17["Word end diff", "combined"], gs18["Word end diff", "combined"], 
                  gs19["Word end diff", "combined"], gs20["Word end diff", "combined"], 
                  gs21["Word end diff", "combined"], gs22["Word end diff", "combined"], 
                  gs23["Word end diff", "combined"], gs24["Word end diff", "combined"])
All_start_diff = c(gs01["All start diff", "combined"], gs02["All start diff", "combined"],
                    gs03["All start diff", "combined"], gs04["All start diff", "combined"],  
                    gs05["All start diff", "combined"], gs06["All start diff", "combined"], 
                    gs07["All start diff", "combined"], gs08["All start diff", "combined"], 
                    gs09["All start diff", "combined"], gs10["All start diff", "combined"], 
                    gs11["All start diff", "combined"], gs12["All start diff", "combined"], 
                    gs13["All start diff", "combined"], gs14["All start diff", "combined"], 
                    gs15["All start diff", "combined"], gs16["All start diff", "combined"], 
                    gs17["All start diff", "combined"], gs18["All start diff", "combined"], 
                    gs19["All start diff", "combined"], gs20["All start diff", "combined"], 
                   gs21["All start diff", "combined"], gs22["All start diff", "combined"], 
                   gs23["All start diff", "combined"], gs24["All start diff", "combined"])
All_end_diff = c(gs01["All end diff", "combined"], gs02["All end diff", "combined"],
                  gs03["All end diff", "combined"], gs04["All end diff", "combined"],  
                  gs05["All end diff", "combined"], gs06["All end diff", "combined"], 
                  gs07["All end diff", "combined"], gs08["All end diff", "combined"], 
                  gs09["All end diff", "combined"], gs10["All end diff", "combined"], 
                  gs11["All end diff", "combined"], gs12["All end diff", "combined"], 
                  gs13["All end diff", "combined"], gs14["All end diff", "combined"], 
                  gs15["All end diff", "combined"], gs16["All end diff", "combined"], 
                  gs17["All end diff", "combined"], gs18["All end diff", "combined"], 
                  gs19["All end diff", "combined"], gs20["All end diff", "combined"], 
                 gs21["All end diff", "combined"], gs22["All end diff", "combined"], 
                 gs23["All end diff", "combined"], gs24["All end diff", "combined"])
N_F1 = c(gs01["N F-measure", "combined"], gs02["N F-measure", "combined"],
         gs03["N F-measure", "combined"], gs04["N F-measure", "combined"],  
         gs05["N F-measure", "combined"], gs06["N F-measure", "combined"], 
         gs07["N F-measure", "combined"], gs08["N F-measure", "combined"], 
         gs09["N F-measure", "combined"], gs10["N F-measure", "combined"], 
         gs11["N F-measure", "combined"], gs12["N F-measure", "combined"], 
         gs13["N F-measure", "combined"], gs14["N F-measure", "combined"], 
         gs15["N F-measure", "combined"], gs16["N F-measure", "combined"], 
         gs17["N F-measure", "combined"], gs18["N F-measure", "combined"], 
         gs19["N F-measure", "combined"], gs20["N F-measure", "combined"], 
         gs21["N F-measure", "combined"], gs22["N F-measure", "combined"], 
         gs23["N F-measure", "combined"], gs24["N F-measure", "combined"])
Schwa_F1 = c(gs01["Schwa F-measure", "combined"], gs02["Schwa F-measure", "combined"],
         gs03["Schwa F-measure", "combined"], gs04["Schwa F-measure", "combined"],  
         gs05["Schwa F-measure", "combined"], gs06["Schwa F-measure", "combined"], 
         gs07["Schwa F-measure", "combined"], gs08["Schwa F-measure", "combined"], 
         gs09["Schwa F-measure", "combined"], gs10["Schwa F-measure", "combined"], 
         gs11["Schwa F-measure", "combined"], gs12["Schwa F-measure", "combined"], 
         gs13["Schwa F-measure", "combined"], gs14["Schwa F-measure", "combined"], 
         gs15["Schwa F-measure", "combined"], gs16["Schwa F-measure", "combined"], 
         gs17["Schwa F-measure", "combined"], gs18["Schwa F-measure", "combined"], 
         gs19["Schwa F-measure", "combined"], gs20["Schwa F-measure", "combined"], 
         gs21["Schwa F-measure", "combined"], gs22["Schwa F-measure", "combined"], 
         gs23["Schwa F-measure", "combined"], gs24["Schwa F-measure", "combined"])
SPN_F1 = c(gs01["SPN F-measure", "combined"], gs02["SPN F-measure", "combined"],
           gs03["SPN F-measure", "combined"], gs04["SPN F-measure", "combined"],  
           gs05["SPN F-measure", "combined"], gs06["SPN F-measure", "combined"], 
           gs07["SPN F-measure", "combined"], gs08["SPN F-measure", "combined"], 
           gs09["SPN F-measure", "combined"], gs10["SPN F-measure", "combined"], 
           gs11["SPN F-measure", "combined"], gs12["SPN F-measure", "combined"], 
           gs13["SPN F-measure", "combined"], gs14["SPN F-measure", "combined"], 
           gs15["SPN F-measure", "combined"], gs16["SPN F-measure", "combined"], 
           gs17["SPN F-measure", "combined"], gs18["SPN F-measure", "combined"], 
           gs19["SPN F-measure", "combined"], gs20["SPN F-measure", "combined"], 
           gs21["SPN F-measure", "combined"], gs22["SPN F-measure", "combined"], 
           gs23["SPN F-measure", "combined"], gs24["SPN F-measure", "combined"])
gs_num = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
comparison = data.frame(gs_num, mean_dist, sil_F1, SIL_start_diff, SIL_end_diff, Word_start_diff, Word_end_diff, N_F1, Schwa_F1)

par(mfrow=c(1,2))

plot(gs_num, mean_dist, type = "b", ylim = c(0,2), col = "red")
lines(gs_num, sil_F1, type = "b", col = "blue")
lines(gs_num, N_F1, type = "b", col = "orange")
lines(gs_num, Schwa_F1, type = "b", col = "purple")
lines(gs_num, SPN_F1, type = "b", col = "yellow")

plot(gs_num, SIL_start_diff, type = "b", ylim = c(0,0.12), col = "green")
lines(gs_num, SIL_end_diff, type = "b", col = "green", lty = "dashed")
lines(gs_num, Word_start_diff, type = "b", col = "pink")
lines(gs_num, Word_end_diff, type = "b", col = "pink", lty = "dashed")
lines(gs_num, All_start_diff, type = "b", col = "black")
lines(gs_num, All_end_diff, type = "b", col = "black", lty = "dashed")

## just lex exp plot
mean_dist_a = c(gs01["Mean Distance", "comp.a"], gs02["Mean Distance", "comp.a"],
              gs03["Mean Distance", "comp.a"], gs04["Mean Distance", "comp.a"],  
              gs05["Mean Distance", "comp.a"], gs06["Mean Distance", "comp.a"], 
              gs07["Mean Distance", "comp.a"], gs08["Mean Distance", "comp.a"], 
              gs09["Mean Distance", "comp.a"], gs10["Mean Distance", "comp.a"],
              gs21["Mean Distance", "comp.a"], gs22["Mean Distance", "comp.a"], 
              gs23["Mean Distance", "comp.a"], gs24["Mean Distance", "comp.a"])
mean_dist_k = c(gs01["Mean Distance", "comp.k"], gs02["Mean Distance", "comp.k"],
                gs03["Mean Distance", "comp.k"], gs04["Mean Distance", "comp.k"],  
                gs05["Mean Distance", "comp.k"], gs06["Mean Distance", "comp.k"], 
                gs07["Mean Distance", "comp.k"], gs08["Mean Distance", "comp.k"], 
                gs09["Mean Distance", "comp.k"], gs10["Mean Distance", "comp.k"],
                gs21["Mean Distance", "comp.k"], gs22["Mean Distance", "comp.k"], 
                gs23["Mean Distance", "comp.k"], gs24["Mean Distance", "comp.k"])
mean_dist_o = c(gs01["Mean Distance", "comp.o"], gs02["Mean Distance", "comp.o"],
                gs03["Mean Distance", "comp.o"], gs04["Mean Distance", "comp.o"],  
                gs05["Mean Distance", "comp.o"], gs06["Mean Distance", "comp.o"], 
                gs07["Mean Distance", "comp.o"], gs08["Mean Distance", "comp.o"], 
                gs09["Mean Distance", "comp.o"], gs10["Mean Distance", "comp.o"],
                gs21["Mean Distance", "comp.o"], gs22["Mean Distance", "comp.o"], 
                gs23["Mean Distance", "comp.o"], gs24["Mean Distance", "comp.o"])

par(mfrow=c(1,1))
generations = c(0,1,2,3,4)
plot(generations, mean_dist_a[1:5], type = "b", col = "red", ylim = c(1,2.6),
     ylab = "weighted feature edit distance (panphon library)", 
     xlab = "nGEN",
     main = "Number of lexical expansion generations")
lines(generations, mean_dist_k[1:5], type = "b", col = "blue")
lines(generations, mean_dist_o[1:5], type = "b", col = "green")
legend("right", legend = c("CGN-A", "CGN-K", "CGN-O"),
       col = c("red", "blue", "green"),
       lty = c("solid", "solid", "solid"), cex=0.7)

generations = c(0,1,2,3)
plot(generations, mean_dist_a[c(11,12,13,14)], type = "b", col = "red", ylim = c(0.8,2.6),
     ylab = "Weighted feature edit distance", 
     xlab = "Generation",
     main = "Lexical expansion evaluation")
lines(generations, mean_dist_k[c(11,12,13,14)], type = "b", col = "blue")
lines(generations, mean_dist_o[c(11,12,13,14)], type = "b", col = "green")
legend("right", legend = c("CGN-A", "CGN-K", "CGN-O"), text.width = 0.55,
       col = c("red", "blue", "green"),
       lty = c("solid", "solid", "solid"), cex=0.7)

# check why lex exp distance goes up
gs = read.csv(paste(f_path, "gs23_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs23_a = gs[gs$corpus == "a",]
gs = read.csv(paste(f_path, "gs24_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs24_a = gs[gs$corpus == "a",]
gs_diff = merge(gs23_a, gs24_a, by = c("wav", "from", "to", "tier", "word", "cgn_start", "cgn_end"))
gs_diff$dist_diff = gs_diff$tran_dist.y - gs_diff$tran_dist.x
gs_diff = gs_diff[order(-gs_diff$dist_diff),]
mean(gs_diff$dist_diff, na.rm = T)
nrow(gs_diff[gs_diff$dist_diff > 0,])
nrow(gs_diff[gs_diff$dist_diff == 0,])
nrow(gs_diff[gs_diff$dist_diff < 0,])
hist(gs_diff$dist_diff)

# inspection in Praat shows that gs23 (n=2) is actually better than gs24 (n=3)
# now let's see if gs22 (n=1) is better than gs23
gs = read.csv(paste(f_path, "gs22_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs22_a = gs[gs$corpus == "a",]
gs = read.csv(paste(f_path, "gs23_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs23_a = gs[gs$corpus == "a",]
gs_diff = merge(gs22_a, gs23_a, by = c("wav", "from", "to", "tier", "word", "cgn_start", "cgn_end"))
gs_diff$dist_diff = gs_diff$tran_dist.y - gs_diff$tran_dist.x
gs_diff = gs_diff[order(-gs_diff$dist_diff),]
mean(gs_diff$dist_diff, na.rm = T)
nrow(gs_diff[gs_diff$dist_diff > 0,])
nrow(gs_diff[gs_diff$dist_diff == 0,])
nrow(gs_diff[gs_diff$dist_diff < 0,])
hist(gs_diff$dist_diff)

## what if we compare gs21 and gs22
gs = read.csv(paste(f_path, "gs21_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs21_a = gs[gs$corpus == "a",]
gs = read.csv(paste(f_path, "gs22_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs22_a = gs[gs$corpus == "a",]
gs_diff = merge(gs21_a, gs22_a, by = c("wav", "from", "to", "tier", "word", "cgn_start", "cgn_end"))
gs_diff$dist_diff = gs_diff$tran_dist.y - gs_diff$tran_dist.x
gs_diff = gs_diff[order(-gs_diff$dist_diff),]
mean(gs_diff$dist_diff, na.rm = T)
nrow(gs_diff[gs_diff$dist_diff > 0,])
nrow(gs_diff[gs_diff$dist_diff == 0,])
nrow(gs_diff[gs_diff$dist_diff < 0,])
plot(density(gs_diff$dist_diff, na.rm = T))


## just n info
N_F1_a = c(gs04["N F-measure", "comp.a"], gs06["N F-measure", "comp.a"], 
         gs07["N F-measure", "comp.a"], gs08["N F-measure", "comp.a"], 
         gs09["N F-measure", "comp.a"])
N_F1_k = c(gs04["N F-measure", "comp.k"], gs06["N F-measure", "comp.k"], 
           gs07["N F-measure", "comp.k"], gs08["N F-measure", "comp.k"], 
           gs09["N F-measure", "comp.k"])
N_F1_o = c(gs04["N F-measure", "comp.o"], gs06["N F-measure", "comp.o"], 
           gs07["N F-measure", "comp.o"], gs08["N F-measure", "comp.o"], 
           gs09["N F-measure", "comp.o"])
Schwa_F1_a = c(gs04["Schwa F-measure", "comp.a"], gs06["Schwa F-measure", "comp.a"], 
               gs07["Schwa F-measure", "comp.a"], gs08["Schwa F-measure", "comp.a"], 
               gs09["Schwa F-measure", "comp.a"])
Schwa_F1_k = c(gs04["Schwa F-measure", "comp.k"], gs06["Schwa F-measure", "comp.k"], 
               gs07["Schwa F-measure", "comp.k"], gs08["Schwa F-measure", "comp.k"], 
               gs09["Schwa F-measure", "comp.k"])
Schwa_F1_o = c(gs04["Schwa F-measure", "comp.o"], gs06["Schwa F-measure", "comp.o"], 
               gs07["Schwa F-measure", "comp.o"], gs08["Schwa F-measure", "comp.o"], 
               gs09["Schwa F-measure", "comp.o"])

par(mfrow=c(1,1))
Ndiff = c(0,0.5,1,2,4)
plot(Ndiff, N_F1_a, type = "b", col = "red", ylim = c(0,1),
     ylab = "F1 score", 
     xlab = "dN [added to -log(pWord)]",
     main = "Final -en words")
lines(Ndiff, Schwa_F1_a, type = "b", col = "red", lty = "dashed")
lines(Ndiff, N_F1_k, type = "b", col = "blue")
lines(Ndiff, Schwa_F1_k, type = "b", col = "blue", lty = "dashed")
lines(Ndiff, N_F1_o, type = "b", col = "green")
lines(Ndiff, Schwa_F1_o, type = "b", col = "green", lty = "dashed")
legend("topleft", legend = c("CGN-A N", "CGN-A Schwa", "CGN-K N", "CGN-K Schwa",
                                 "CGN-O N", "CGN-O Schwa"),
       col = c("red", "red", "blue", "blue", "green", "green"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"), cex=0.5)

# just SIL info
sil_F1_a = c(gs12["SIL F-measure", "comp.a"], gs08["SIL F-measure", "comp.a"],
             gs10["SIL F-measure", "comp.a"], gs11["SIL F-measure", "comp.a"])
sil_F1_k = c(gs12["SIL F-measure", "comp.k"], gs08["SIL F-measure", "comp.k"],
             gs10["SIL F-measure", "comp.k"], gs11["SIL F-measure", "comp.k"])
sil_F1_o = c(gs12["SIL F-measure", "comp.o"], gs08["SIL F-measure", "comp.o"],
             gs10["SIL F-measure", "comp.o"], gs11["SIL F-measure", "comp.o"])

SIL_start_diff_a = c(gs12["SIL start diff", "comp.a"], gs08["SIL start diff", "comp.a"],
                     gs10["SIL start diff", "comp.a"], gs11["SIL start diff", "comp.a"])
SIL_start_diff_k = c(gs12["SIL start diff", "comp.k"], gs08["SIL start diff", "comp.k"],
                     gs10["SIL start diff", "comp.k"], gs11["SIL start diff", "comp.k"])
SIL_start_diff_o = c(gs12["SIL start diff", "comp.o"], gs08["SIL start diff", "comp.o"],
                     gs10["SIL start diff", "comp.o"], gs11["SIL start diff", "comp.o"])

SIL_end_diff_a = c(gs12["SIL end diff", "comp.a"], gs08["SIL end diff", "comp.a"],
                   gs10["SIL end diff", "comp.a"], gs11["SIL end diff", "comp.a"])
SIL_end_diff_k = c(gs12["SIL end diff", "comp.k"], gs08["SIL end diff", "comp.k"],
                   gs10["SIL end diff", "comp.k"], gs11["SIL end diff", "comp.k"])
SIL_end_diff_o = c(gs12["SIL end diff", "comp.o"], gs08["SIL end diff", "comp.o"],
                   gs10["SIL end diff", "comp.o"], gs11["SIL end diff", "comp.o"])

par(mfrow=c(1,2))
silprob = c(0.25,0.5,0.75,0.99)

plot(silprob, sil_F1_a, type = "b", col = "red", ylim = c(0.6,0.9),
     ylab = "F1 score", 
     xlab = "sil-prob",
     main = "Inserted silences")
lines(silprob, sil_F1_k, type = "b", col = "blue")
lines(silprob, sil_F1_o, type = "b", col = "green")
legend("topleft", legend = c("CGN-A", "CGN-K", "CGN-O"), 
       col = c("red", "blue", "green"), lty = c("solid", "solid", "solid"), cex=0.5)

plot(silprob, SIL_start_diff_a, type = "b", col = "red", ylim = c(0,0.17),
     ylab = "Time difference", 
     xlab = "sil-prob",
     main = "Inserted silences")
lines(silprob, SIL_end_diff_a, type = "b", col = "red", lty = "dashed")
lines(silprob, SIL_start_diff_k, type = "b", col = "blue")
lines(silprob, SIL_end_diff_k, type = "b", col = "blue", lty = "dashed")
lines(silprob, SIL_start_diff_o, type = "b", col = "green")
lines(silprob, SIL_end_diff_o, type = "b", col = "green", lty = "dashed")
legend("topleft", legend = c("CGN-A start", "CGN-A end", "CGN-K start", "CGN-K end", "CGN-O start", "CGN-O end"), 
       col = c("red", "red", "blue", "blue", "green", "green"), lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"), cex=0.5)

# pSPN: look at recall, precision and F1 instead of different components (only comp-a is interesting)
spn_F1 = c(gs10["SPN F-measure", "combined"], gs19["SPN F-measure", "combined"], gs17["SPN F-measure", "combined"], gs13["SPN F-measure", "combined"],
           gs14["SPN F-measure", "combined"], gs15["SPN F-measure", "combined"])
spn_recall = c(gs10["SPN recall", "combined"], gs19["SPN recall", "combined"], gs17["SPN recall", "combined"], gs13["SPN recall", "combined"],
               gs14["SPN recall", "combined"], gs15["SPN recall", "combined"])
spn_precision = c(gs10["SPN precision", "combined"], gs19["SPN precision", "combined"], gs17["SPN precision", "combined"], gs13["SPN precision", "combined"],
                  gs14["SPN precision", "combined"], gs15["SPN precision", "combined"])

par(mfrow=c(1,2))
pSPN = c(0,0.01,0.025,0.05,0.1,0.2)

plot(pSPN, spn_F1, type = "b", col = "red", ylim = c(0,0.3),
     ylab = "Performance measures", 
     xlab = "pSPN",
     main = "Parallel SPN")
lines(pSPN, spn_recall, type = "b", col = "blue")
lines(pSPN, spn_precision, type = "b", col = "green")
legend("topleft", legend = c("F-measure", "Recall", "Precision"), 
       col = c("red", "blue", "green"), lty = c("solid", "solid", "solid"), cex=0.5)

plot(pSPN, All_start_diff[c(10,19,17,13,14,15)], type = "b", ylim = c(0.02,0.045), col = "black",
     ylab = "Boundary difference", main = "Effect on boundaries")
lines(pSPN, All_end_diff[c(10,19,17,13,14,15)], type = "b", col = "black", lty = "dashed")
lines(pSPN, SIL_end_diff[c(10,19,17,13,14,15)], type = "l", col = "green", lty = "dashed")


# pSIL
parsil_F1 = c(gs10["SIL F-measure", "combined"], gs16["SIL F-measure", "combined"], gs18["SIL F-measure", "combined"], gs20["SIL F-measure", "combined"])
parsil_recall = c(gs10["SIL recall", "combined"], gs16["SIL recall", "combined"], gs18["SIL recall", "combined"], gs20["SIL recall", "combined"])
parsil_precision = c(gs10["SIL precision", "combined"], gs16["SIL precision", "combined"], gs18["SIL precision", "combined"], gs20["SIL precision", "combined"])

par(mfrow=c(1,2))
pSIL = c(0,0.05,0.1,0.2)

plot(pSIL, parsil_F1, type = "b", col = "red", ylim = c(0.74,0.78),
     ylab = "Performance measures", 
     xlab = "pSIL",
     main = "Parallel SIL")
lines(pSIL, parsil_recall, type = "b", col = "blue")
lines(pSIL, parsil_precision, type = "b", col = "green")
legend("topleft", legend = c("F-measure", "Recall", "Precision"), 
       col = c("red", "blue", "green"), lty = c("solid", "solid", "solid"), cex=0.5)

plot(pSIL, SIL_start_diff[c(10,16,18,20)], type = "b", ylim = c(0.02,0.11), col = "green",
     ylab = "Boundary difference", main = "Effect on boundaries")
lines(pSIL, SIL_end_diff[c(10,16,18,20)], type = "b", col = "black")
lines(pSIL, Word_start_diff[c(10,16,18,20)], type = "b", col = "pink")
lines(pSIL, Word_end_diff[c(10,16,18,20)], type = "b", col = "grey")


# let's investigate increase in distance between gs02 and gs03
gs = read.csv(paste(f_path, "gs02_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs02_a = gs[gs$corpus == "a",]
gs = read.csv(paste(f_path, "gs03_aligned_dist.csv", sep = ""))
gs$corpus = as.factor(substr(gs$wav, 1, 1))
gs03_a = gs[gs$corpus == "a",]
gs_diff = merge(gs02_a, gs03_a, by = c("wav", "from", "to", "tier", "word", "cgn_start", "cgn_end"))
gs_diff$dist_diff = gs_diff$tran_dist.y - gs_diff$tran_dist.x
