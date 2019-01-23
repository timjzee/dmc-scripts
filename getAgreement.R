if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/cgn/"
}

agr = read.csv(paste(f_path, "agreement_measurements_enc.csv", sep = ""))
agr$levenshtein = diag(adist(agr$cgn_tran, agr$kaldi_tran))
agr$comp = substr(agr$filepath, 1, 1)
agr_tbl = table(agr$agreement, agr$comp)
agr_tbl_prop = prop.table(agr_tbl, 2)
rownames(agr_tbl_prop) = c("Disagree", "Agree")
barplot(agr_tbl_prop, legend = rownames(agr_tbl_prop), main = "Boundary percentage agreement (20 ms)",
        args.legend = list(x = "top", horiz = TRUE, inset=c(0, -0.16), xpd = TRUE, bty = "n"))