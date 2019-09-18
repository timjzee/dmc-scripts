if (Sys.info()[1] == "Darwin"){
  tim_path = "/Volumes/timzee/Docs/"
} else {
  tim_path = "/home/timzee/Docs/"
}

lex_var = read.csv(paste(tim_path, "lex_var.csv", sep = ""))
lex_var = na.omit(lex_var)
lex_var$prop_canon = lex_var$freq_canon / lex_var$freq_total
lex_var$canon_other_prop = lex_var$freq_canon / lex_var$freq_other
lex_var$prop_variants_used = lex_var$num_used_var / lex_var$num_poss_var
hist(lex_var$prop_variants_used, main = "")

nrow(lex_var[lex_var$canon_other_prop < 1,]) / nrow(lex_var)
nrow(lex_var[lex_var$canon_other_prop > 1,]) / nrow(lex_var)
nrow(lex_var[lex_var$canon_other_prop == 1,]) / nrow(lex_var)

sum(lex_var$freq_canon) / sum(lex_var$freq_total)
sum(lex_var$freq_other) / sum(lex_var$freq_total)

mean(lex_var$num_poss_var)
min(lex_var$num_poss_var)
max(lex_var$num_poss_var)
mean(lex_var$num_used_var)

hist(lex_var$canon_other_prop, xlim = c(0, 10), breaks = 800)
