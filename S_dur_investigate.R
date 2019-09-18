library(lme4)
library(effects)
library(car)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/s_dur_models/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
  ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/Volumes/tensusers/timzee/ECSD/"
} else {
  f_path = "/vol/tensusers/timzee/s_dur_models/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
  ifadv_path = "/vol/tensusers/timzee/IFADVcorpus/"
  ecsd_path = "/vol/tensusers/timzee/ECSD/"
}

print("Loading comp-a")
s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static_final.csv", sep = ""))
s_dur_a$corpus = as.factor("cgn-a")
print("Loading comp-c")
s_dur_c = read.csv(paste(cgn_path, "comp-c_s_static_final.csv", sep = ""))
s_dur_c$corpus = as.factor("cgn-c")
s_dur_c$birth_year = as.integer(s_dur_c$birth_year)
print("Loading comp-d")
s_dur_d = read.csv(paste(cgn_path, "comp-d_s_static_final.csv", sep = ""))
s_dur_d$corpus = as.factor("cgn-d")
print("Loading comp-k")
s_dur_k = read.csv(paste(cgn_path, "comp-k_s_static_final.csv", sep = ""))
s_dur_k$corpus = as.factor("cgn-k")
s_dur_k$birth_year = as.integer(s_dur_k$birth_year)
s_dur_k$mean_hnr = as.factor(s_dur_k$mean_hnr)
print("Loading comp-o")
s_dur_o = read.csv(paste(cgn_path, "comp-o_s_static_final.csv", sep = ""))
s_dur_o$corpus = as.factor("cgn-o")
s_dur_o$birth_year = as.integer(s_dur_o$birth_year)
s_dur_o$mean_hnr = as.factor(s_dur_o$mean_hnr)
print("Loading ifadv")
s_dur_ifadv = read.csv(paste(ifadv_path, "ifadv_s_static_final.csv", sep = ""))
s_dur_ifadv$corpus = as.factor("ifadv")
s_dur_ifadv$mean_hnr = as.factor(s_dur_ifadv$mean_hnr)
levels(s_dur_ifadv$speaker_sex) = c("sex2", "sex1")
print("Loading ecsd")
s_dur_ecsd = read.csv(paste(ecsd_path, "ecsd_s_static_final.csv", sep = ""))
s_dur_ecsd$corpus = as.factor("ecsd")

print("Prepping data")

s_dur = rbind(s_dur_a, s_dur_c, s_dur_d, s_dur_k, s_dur_o, s_dur_ifadv, s_dur_ecsd)
s_dur$prev_mention = as.factor(s_dur$prev_mention)
s_dur$phrase_final = as.factor(s_dur$phrase_final)

s_dur$stressed = s_dur$num_syl == s_dur$word_stress
s_dur$stressed = as.factor(s_dur$stressed)
s_dur$stress_dist = s_dur$num_syl - s_dur$word_stress

is.na(s_dur$num_syl_pron) = !s_dur$num_syl_pron

# get rid of unnecessary columns
drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

# Get rid of GEN-TIME
s_dur = s_dur[s_dur$type_of_s != "GEN-TIME",]
s_dur$type_of_s = as.factor(as.character(s_dur$type_of_s))
s_dur$type_of_s = relevel(s_dur$type_of_s, ref="S")

# don't create unnnecessary NAs
#is.na(s_dur$bigram_f) = !s_dur$bigram_f
s_dur$log_bigf = log10(s_dur$bigram_f + 1)


vowels = c("@", "A", "AU", "E", "E2", "EI", "EU", "I", "O", "U", "UI", "a", "e", "i", "o", "u", "y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N")
fricatives = c("G", "S", "Z", "f", "h", "s", "v", "x", "z")
plosives = c("b", "d", "g", "k", "p", "t")

get_phon_class = function(x) {
  if (x %in% vowels){
    return("V")
  } else if (x %in% liquids){
    return("L")
  } else if (x %in% approximants){
    return("APP")
  } else if (x %in% nasals){
    return("N")
  } else if (x %in% fricatives){
    return("F")
  } else if (x %in% plosives){
    return("P")
  } else if (!is.na(x) & x == "SIL"){
    return("SIL")
  } else {
    return(NA)
  }
}

s_dur$next_phon_class = as.factor(sapply(s_dur$next_phon_pron, get_phon_class))
s_dur$prev_phon_class = as.factor(sapply(s_dur$prev_phon_pron, get_phon_class))

# see https://statisticalhorizons.com/multicollinearity
# V has most cases by far for the prev_phon_class
s_dur$prev_phon_class = relevel(s_dur$prev_phon_class, ref="V")
s_dur$next_phon_class = relevel(s_dur$next_phon_class, ref="V")

# remove lines for which measurements failed
s_dur = s_dur[!is.na(s_dur$s_dur), ]
# remove NA lines
s_dur = s_dur[rowSums(is.na(s_dur))<length(s_dur),]

# remove unrepresentative outliers (Baayen, 2008, p. 243)
s_dur = s_dur[s_dur$s_dur < 0.4,]

# make new predictors
n_cow = 5052213
n_subtlex = 437504
s_dur$wf = 10^s_dur$log_wf
s_dur[is.na(s_dur$wf),]$wf = 1
s_dur$log_wf = log10(s_dur$wf)
s_dur$p_next_w = (s_dur$bigram_f / n_cow) / (s_dur$wf / n_subtlex)

# don't create unnecessary NAs
s_dur[is.na(s_dur$lex_neb_freq),]$lex_neb_freq = 0
#s_dur$lex_neb_freq[s_dur$lex_neb_freq == 0] = NA
#n_lexicon = 251563
s_dur$prop_lex_neb_freq = s_dur$lex_neb_freq / s_dur$wf

s_dur$mean_syl_dur = s_dur$base_dur / s_dur$num_syl_pron

# transform dependent variable
s_dur$log_s_dur = log10(s_dur$s_dur)

drop = c("lex_neb", "prev_phon_pron", "next_phon_pron", "lex_neb_freq", "bigram_f", "num_syl", 
         "word_stress", "base_dur", "num_syl_pron", "log_bigf", "prev_phon_class",
         "wf", "stressed")
s_dur = s_dur[ , !(names(s_dur) %in% drop)]

table(s_dur$corpus, s_dur$type_of_s)

s_dur2 = na.omit(s_dur)

table(s_dur2$corpus, s_dur2$type_of_s)

##

intercepts = lmer(log_s_dur ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                   + log_wf + prop_lex_neb_freq + p_next_w
                   + stress_dist + next_phon_class + prev_mention + phrase_final
                   + type_of_s*corpus
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_dur)

plot(effect("type_of_s:corpus", intercepts, x.var = "type_of_s"), multiline = F, 
     layout = c(7,1), ylim = c(-1.4, -0.8), alternating = F, rotx = 45, ci.style = "bars")

## Select FA validation set
s_dur_ao = s_dur2[s_dur2$corpus %in% c("cgn-a", "cgn-o"),]
s_dur_ao$corpus = as.factor(as.character(s_dur_ao$corpus))
s_dur_ao$file = substr(as.character(s_dur_ao$wav), 6, nchar(as.character(s_dur_ao$wav)))
s_dur_ao$id = 1:nrow(s_dur_ao)

core_a = as.vector(read.table(paste(cgn_path, "comp-a_wrd.tmp", sep = ""))$V1)
core_o = as.vector(read.table(paste(cgn_path, "comp-o_wrd.tmp", sep = ""))$V1)
core_files = c(core_a, core_o)

s_dur_ao$in_core = s_dur_ao$file %in% core_files
table(s_dur_ao[s_dur_ao$in_core == TRUE,]$type_of_s, s_dur_ao[s_dur_ao$in_core == TRUE,]$corpus)

core = s_dur_ao[s_dur_ao$in_core == TRUE,]
notcore = s_dur_ao[s_dur_ao$in_core == FALSE,]

set.seed(41)

ss = 50
ignore_words = c("aanschijns", "inziens", "huizes", "Wells'", "Jezus'", "andermans", "Christus'", "Ovidius'", "Thomas'", "avondmaals")

fa_subset = rbind(core[core$id %in% sample(core[core$corpus == "cgn-a" & core$type_of_s == "S",]$id, ss, replace = F),], 
                  core[core$corpus == "cgn-a" & core$type_of_s == "GEN-POSS" & !(core$word_ort %in% ignore_words),],
                  notcore[notcore$id %in% sample(notcore[notcore$corpus == "cgn-a" & notcore$type_of_s == "GEN-POSS" & !(notcore$word_ort %in% ignore_words),]$id, 
                                                 ss - nrow(core[core$corpus == "cgn-a" & core$type_of_s == "GEN-POSS" & !(core$word_ort %in% ignore_words),]), replace = F),], 
                  core[core$corpus == "cgn-a" & core$type_of_s == "PART",],
                  notcore[notcore$id %in% sample(notcore[notcore$corpus == "cgn-a" & notcore$type_of_s == "PART",]$id, 
                                                 ss - nrow(core[core$corpus == "cgn-a" & core$type_of_s == "PART",]), replace = F),], 
                  core[core$id %in% sample(core[core$corpus == "cgn-a" & core$type_of_s == "PL",]$id, ss, replace = F),],
                  core[core$id %in% sample(core[core$corpus == "cgn-o" & core$type_of_s == "S",]$id, ss, replace = F),], 
                  core[core$corpus == "cgn-o" & core$type_of_s == "GEN-POSS" & !(core$word_ort %in% ignore_words),],
                  notcore[notcore$id %in% sample(notcore[notcore$corpus == "cgn-o" & notcore$type_of_s == "GEN-POSS" & !(notcore$word_ort %in% ignore_words),]$id, 
                                                 ss - nrow(core[core$corpus == "cgn-o" & core$type_of_s == "GEN-POSS" & !(core$word_ort %in% ignore_words),]), replace = F),], 
                  core[core$corpus == "cgn-o" & core$type_of_s == "PART",],
                  notcore[notcore$id %in% sample(notcore[notcore$corpus == "cgn-o" & notcore$type_of_s == "PART",]$id, 
                                                 ss - nrow(core[core$corpus == "cgn-o" & core$type_of_s == "PART",]), replace = F),], 
                  core[core$id %in% sample(core[core$corpus == "cgn-o" & core$type_of_s == "PL",]$id, ss, replace = F),])

write.table(fa_subset[ ,c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", "word_ort", "speaker")], 
            file = paste(cgn_path, "fa_eval_subset.csv", sep = ""), row.names=FALSE, col.names=TRUE, sep=",", quote = FALSE)

## check durational effects

s_eval = read.csv(paste(cgn_path, "eval_s_static_final.csv", sep = ""))
s_eval$corpus = as.factor(paste("cgn-", substr(s_eval$wav, 1, 1), sep = ""))

s_eval$prev_mention = as.factor(s_eval$prev_mention)
s_eval$phrase_final = as.factor(s_eval$phrase_final)
s_eval$stressed = s_eval$num_syl == s_eval$word_stress
s_eval$stressed = as.factor(s_eval$stressed)
s_eval$stress_dist = s_eval$num_syl - s_eval$word_stress
is.na(s_eval$num_syl_pron) = !s_eval$num_syl_pron

drop = c("ptan", "ptaf", "mean_hnr", "next_phon_dur", "prev_phon_dur", "birth_year", "speaker_sex", 
         "proportion_voiced", "proportion_voiced2", "s_cog_full", "s_cog_window", "per_mil_wf", 
         "word_class", "word_pos", "next_phon", "prev_phon", "sent_i", "word_sent_i")
s_eval = s_eval[ , !(names(s_eval) %in% drop)]

s_eval$type_of_s = relevel(s_eval$type_of_s, ref="S")
s_eval$log_bigf = log10(s_eval$bigram_f + 1)

s_eval$next_phon_class = as.factor(sapply(s_eval$next_phon_pron, get_phon_class))
s_eval$prev_phon_class = as.factor(sapply(s_eval$prev_phon_pron, get_phon_class))
s_eval$prev_phon_class = relevel(s_eval$prev_phon_class, ref="V")
s_eval$next_phon_class = relevel(s_eval$next_phon_class, ref="V")

s_eval = s_eval[!is.na(s_eval$s_dur), ]
s_eval = s_eval[rowSums(is.na(s_eval))<length(s_eval),]
s_eval = s_eval[s_eval$s_dur < 0.4,]

n_cow = 5052213
n_subtlex = 437504
s_eval$wf = 10^s_eval$log_wf
s_eval[is.na(s_eval$wf),]$wf = 1
s_eval$log_wf = log10(s_eval$wf)
s_eval$p_next_w = (s_eval$bigram_f / n_cow) / (s_eval$wf / n_subtlex)

s_eval[is.na(s_eval$lex_neb_freq),]$lex_neb_freq = 0
s_eval$prop_lex_neb_freq = s_eval$lex_neb_freq / s_eval$wf
s_eval$mean_syl_dur = s_eval$base_dur / s_eval$num_syl_pron
s_eval$log_s_dur = log10(s_eval$s_dur)

drop = c("lex_neb", "prev_phon_pron", "next_phon_pron", "lex_neb_freq", "bigram_f", "num_syl", 
         "word_stress", "base_dur", "num_syl_pron", "log_bigf", "prev_phon_class",
         "wf", "stressed")
s_eval = s_eval[ , !(names(s_eval) %in% drop)]

table(s_eval$corpus, s_eval$type_of_s)

## check difference
s_dur_a = read.csv(paste(cgn_path, "comp-a_s_static_final.csv", sep = ""))
s_dur_o = read.csv(paste(cgn_path, "comp-o_s_static_final.csv", sep = ""))
s_dur_o$birth_year = as.integer(s_dur_o$birth_year)
s_dur_o$mean_hnr = as.factor(s_dur_o$mean_hnr)
s_dur = rbind(s_dur_a, s_dur_o)
s_dur = s_dur[, names(s_dur) %in% c("wav", "chan", "chunk_start", "chunk_end", "tier", "word_chunk_i", "s_dur")]
s_dur$kaldi_dur = s_dur$s_dur
s_dur = s_dur[, names(s_dur) != "s_dur"]

s_diff = merge(s_eval, s_dur)
s_diff$dur_diff = s_diff$kaldi_dur - s_diff$s_dur
#s_diff$log_diff = log10(s_diff$dur_diff + 0.001)

#
differences = lmer(dur_diff ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                   + log_wf + prop_lex_neb_freq + p_next_w
                   + stress_dist + next_phon_class + prev_mention + phrase_final
                   + type_of_s*corpus
                   + (1 | speaker) + (1 | word_ort),
                   control = lmerControl(optCtrl = list(maxfun = 1e6)),
                   data=s_diff)

plot(effect("type_of_s:corpus", differences, x.var = "type_of_s"), multiline = F, 
     layout = c(2,1), 
     #     ylim = c(-1.4, -0.8), 
     alternating = F, rotx = 45, ci.style = "bars")

## does this difference between GEN-POSS and other categories exist when we exclude
## the 4 tokens that we used when we looked at wrd boundaries
s_diff$file = substr(as.character(s_diff$wav), 6, nchar(as.character(s_diff$wav)))
s_diff$in_core = s_diff$file %in% core_files

s_diff2 = s_diff[!(s_diff$type_of_s == "GEN-POSS" & s_diff$corpus == "cgn-a" & s_diff$in_core == TRUE),]

#
differences2 = lmer(dur_diff ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                    + log_wf + prop_lex_neb_freq + p_next_w
                    + stress_dist + next_phon_class + prev_mention + phrase_final
                    + type_of_s*corpus
                    + (1 | speaker) + (1 | word_ort),
                    control = lmerControl(optCtrl = list(maxfun = 1e6)),
                    data=s_diff2)

plot(effect("type_of_s:corpus", differences2, x.var = "type_of_s"), multiline = F, 
     layout = c(2,1), 
     #     ylim = c(-1.4, -0.8), 
     alternating = F, rotx = 45, ci.style = "bars")

## yes, a difference between GEN-POSS (and PART) and others remains
## now let's figure out what causes it.

# other predictors:

library(gplots)
par(mfrow=c(1,1))
plotmeans(speech_rate_pron ~ interaction(type_of_s, corpus), data = s_diff)
plotmeans(log_wf ~ interaction(type_of_s, corpus), data = s_diff)
plotmeans(p_next_w ~ interaction(type_of_s, corpus), data = s_diff)
plotmeans(prop_lex_neb_freq ~ interaction(type_of_s, corpus), data = s_diff)
plotmeans(mean_syl_dur ~ interaction(type_of_s, corpus), data = s_diff)
plotmeans(stress_dist ~ interaction(type_of_s, corpus), data = s_diff)
plotmeans(num_cons_pron ~ interaction(type_of_s, corpus), data = s_diff)

plotmeans(kaldi_dur ~ interaction(type_of_s, corpus), data = s_diff, ylim = c(0.06, 0.18))
plotmeans(s_dur ~ interaction(type_of_s, corpus), data = s_diff, ylim = c(0.06, 0.18))

table(s_diff$type_of_s, s_diff$next_phon_class, s_diff$corpus)

# dynamic analysis:

library(mgcv)
library(itsadug)

s_dyn = read.csv(paste(cgn_path, "eval_s_dyn2.csv", sep = ""))
s_dyn$corpus = as.factor(paste("cgn-", substr(s_dyn$wav, 1, 1), sep = ""))
# s_dyn$type_of_s = as.character(s_dyn$type_of_s)
s_dyn = s_dyn[!(s_dyn$type_of_s %in% c("OTHER", "GEN-TIME")),]
#s_dyn$type_of_s = as.factor(s_dyn$type_of_s)
s_dyn$type_of_s = relevel(s_dyn$type_of_s, ref="S")
s_dyn$freq_bin = as.numeric(s_dyn$freq_bin)
s_dyn_a = s_dyn[s_dyn$corpus == "cgn-a",]

dyn_a = bam(dB_per_Hz ~ type_of_s + te(time, freq_bin, k = c(4,4), by = type_of_s) 
            + s(speaker, bs = 're'), data = s_dyn_a, method = 'ML')

### dyn o

s_dyn_o = s_dyn[s_dyn$corpus == "cgn-o",]

dyn_o = bam(dB_per_Hz ~ type_of_s + te(time, freq_bin, k = c(4,4), by = type_of_s) 
            + s(speaker, bs = 're'), data = s_dyn_o, method = 'ML')

### get manually adjusted data

s_dyn_eval = read.csv(paste(cgn_path, "eval_s_dynamic.csv", sep = ""))
s_dyn_eval$corpus = as.factor(paste("cgn-", substr(s_dyn_eval$wav, 1, 1), sep = ""))
# s_dyn_eval$type_of_s = as.character(s_dyn_eval$type_of_s)
s_dyn_eval = s_dyn_eval[!(s_dyn_eval$type_of_s %in% c("OTHER", "GEN-TIME")),]
#s_dyn_eval$type_of_s = as.factor(s_dyn_eval$type_of_s)
s_dyn_eval$type_of_s = relevel(s_dyn_eval$type_of_s, ref="S")
s_dyn_eval$freq_bin = as.numeric(s_dyn_eval$freq_bin)
s_dyn_eval_a = s_dyn_eval[s_dyn_eval$corpus == "cgn-a",]

dyn_eval_a = bam(dB_per_Hz ~ type_of_s + te(time, freq_bin, k = c(4,4), by = type_of_s) 
                 + s(speaker, bs = 're'), data = s_dyn_eval_a, method = 'ML')

s_dyn_eval_o = s_dyn_eval[s_dyn_eval$corpus == "cgn-o",]
dyn_eval_o = bam(dB_per_Hz ~ type_of_s + te(time, freq_bin, k = c(4,4), by = type_of_s) 
                 + s(speaker, bs = 're'), data = s_dyn_eval_o, method = 'ML')


##
par(mfrow=c(2,4))
fvisgam(dyn_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'S'), 
        zlim = c(58.5, 87),
        main = 'S', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PL'), 
        zlim = c(58.5, 87),
        main = 'PL', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'GEN-POSS'), 
        zlim = c(58.5, 87),
        main = 'GEN-POSS', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PART'),
        zlim = c(58.5, 87),
        main = 'PART', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'S'), 
        zlim = c(58.5, 87),
        main = 'S', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PL'), 
        zlim = c(58.5, 87),
        main = 'PL', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'GEN-POSS'), 
        zlim = c(58.5, 87),
        main = 'GEN-POSS', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_a,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PART'),
        zlim = c(58.5, 87),
        main = 'PART', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)


## o
par(mfrow=c(2,4))
fvisgam(dyn_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'S'), 
        zlim = c(66.8, 99.3),
        main = 'S', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PL'), 
        zlim = c(66.8, 99.3),
        main = 'PL', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'GEN-POSS'), 
        zlim = c(66.8, 99.3),
        main = 'GEN-POSS', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PART'),
        zlim = c(66.8, 99.3),
        main = 'PART', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'S'), 
        zlim = c(66.8, 99.3),
        main = 'S', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PL'), 
        zlim = c(66.8, 99.3),
        main = 'PL', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'GEN-POSS'), 
        zlim = c(66.8, 99.3),
        main = 'GEN-POSS', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)
fvisgam(dyn_eval_o,  view = c('time', 'freq_bin'), cond = list(type_of_s = 'PART'),
        zlim = c(66.8, 99.3),
        main = 'PART', xlab = 'Time', ylab = 'Frequency Bin', rm.ranef=F)

### suspicion:  in comp-a segments start later and end later due to echo, 
###             but in GEN-POSS segments do not start later because it is 
###             intense enough to rise above noise/echo
### get actual boundaries

bounds = read.csv(paste(cgn_path, "eval_boundaries.csv", sep = ""))
s_diff3 = merge(s_diff, bounds)
s_diff3$start_diff = s_diff3$kal_start - s_diff3$tim_start
s_diff3$end_diff = s_diff3$kal_end - s_diff3$tim_end

start_diff_m = lmer(start_diff ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                    + log_wf + prop_lex_neb_freq + p_next_w
                    + stress_dist + next_phon_class + prev_mention + phrase_final
                    + type_of_s*corpus
                    + (1 | speaker) + (1 | word_ort),
                    control = lmerControl(optCtrl = list(maxfun = 1e6)),
                    data=s_diff3)

plot(effect("type_of_s:corpus", start_diff_m, x.var = "type_of_s"), multiline = F, 
     layout = c(2,1), 
     ylim = c(0, 0.03), 
     alternating = F, rotx = 45, ci.style = "bars")

end_diff_m = lmer(end_diff ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                  + log_wf + prop_lex_neb_freq + p_next_w
                  + stress_dist + next_phon_class + prev_mention + phrase_final
                  + type_of_s*corpus
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_diff3)

plot(effect("type_of_s:corpus", end_diff_m, x.var = "type_of_s"), multiline = F, 
     layout = c(2,1), 
     ylim = c(0, 0.03),
     alternating = F, rotx = 45, ci.style = "bars")


### our suspicion was not correct
### the effect of duration is really caused by the end boundary
### let's get influential points
### to keep things simple we'll make a model for just cgn-a
### we have get rid of one token because 1 observation for Liquids does not work with influence measurement
s_diff3_a = s_diff3[s_diff3$corpus == "cgn-a" & s_diff3$next_phon_class != "L",]
s_diff3_a$next_phon_class = as.factor(as.character(s_diff3_a$next_phon_class))

end_diff_a = lmer(end_diff ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                  + log_wf + prop_lex_neb_freq + p_next_w
                  + stress_dist + prev_mention + phrase_final + next_phon_class
                  + type_of_s
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_diff3_a)

plot(effect("type_of_s", end_diff_a, x.var = "type_of_s"), multiline = F, 
     layout = c(1,1), 
     ylim = c(0, 0.03),
     alternating = F, rotx = 45, ci.style = "bars")


infl = influence(end_diff_a)
betas = as.data.frame(dfbeta(infl))
gen_coeff = summary(end_diff_a)$coefficients["type_of_sGEN-POSS", "Estimate"]
betas$prop_coef = betas$`type_of_sGEN-POSS` / gen_coeff

plot(betas$prop_coef, type="n")
text(betas$prop_coef, labels = s_diff3_a$word_ort)

### influential points:
### increasing GEN-POSS coefficient:
### a/nl/fn000616,2,266.602,268.947,2,3,papa's
### a/nl/fn000254,2,410.148,412.957,1,12,moeders
###
### decreasing GEN-POSS coefficient:
### a/nl/fn000501,2,33.204,35.309,2,7,tussentijds  (because it is similar to GEN-POSS?)

### same points influential if we look at durational differences?

dur_diff_a = lmer(dur_diff ~ speech_rate_pron + mean_syl_dur + num_cons_pron
                  + log_wf + prop_lex_neb_freq + p_next_w
                  + stress_dist + prev_mention + phrase_final + next_phon_class
                  + type_of_s
                  + (1 | speaker) + (1 | word_ort),
                  control = lmerControl(optCtrl = list(maxfun = 1e6)),
                  data=s_diff3_a)

plot(effect("type_of_s", dur_diff_a, x.var = "type_of_s"), multiline = F, 
     layout = c(1,1), 
#     ylim = c(0, 0.03),
     alternating = F, rotx = 45, ci.style = "bars")

infl = influence(dur_diff_a)
betas = as.data.frame(dfbeta(infl))
gen_coeff = summary(dur_diff_a)$coefficients["type_of_sGEN-POSS", "Estimate"]
betas$prop_coef = betas$`type_of_sGEN-POSS` / gen_coeff

plot(betas$prop_coef, type="n")
text(betas$prop_coef, labels = s_diff3_a$word_ort)

### yes largely same influencers, a new increaser:
### a/nl/fn007960,2,105.162,106.350,1,4,ieders
### a/nl/fn000526,2,348.264,351.378,1,3,moeders
### a/nl/fn000521,2,153.949,155.772,1,3,elkaars

### most increasers are LONG and precede SIL or 'uh'
