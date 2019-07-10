library(lmerTest)
library(effects)
library(gplots)

if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/IFAcorpus/"
  wd = "/Volumes/timzee/GitHub/dmc-scripts/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/"
} else {
  f_path = "/vol/tensusers/timzee/IFAcorpus/"
  wd = "/home/timzee/GitHub/dmc-scripts/"
  cgn_path = "/vol/tensusers/timzee/cgn/"
}

source(paste(wd, "gwet/paired_t_test_for_agreement_coefficients.r", sep = ""))
source(paste(wd, "gwet/weights.gen.r", sep = ""))

vowels = c("@", "2", "9", "9+", "a", "A", "e", "E", "E+", "E~", "i", "I", "o", "O", "o+", "O+", "O~", "u", "y", "Y")
liquids = c("r", "l")
approximants = c("j", "w")
nasals = c("n", "m", "N", "J")
fricatives = c("f", "G", "h", "s", "S", "v", "x", "z", "Z")
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
  } else if (!is.na(x) & x == "*"){
    return("SIL")
  } else {
    return(NA)
  }
}

get_prev_phon_class = function(x) {
  prev_pc = NA
  xi = as.integer(x)
  if (!is.na(kki_dur$phon_class[xi]) & xi > 1) {
    while(is.na(prev_pc)) {
      xi = xi - 1
      prev_pc = as.character(kki_dur$phon_class[xi])
    }
  }
  return(prev_pc)
}

get_prev_diff1 = function(x) {
  lab1 = NA
  lab2 = NA
  prev_df = NA
  xi = as.integer(x)
  if (!is.na(i_k$diff[xi]) & xi > 1) {
    while(is.na(lab1) & is.na(lab2)) {
      xi = xi - 1
      lab1 = i_k$ifa_labels[xi]
      lab2 = i_k$annot2_labels[xi]
    }
    prev_df = i_k$diff[xi]
  }
  return(prev_df)
}

get_prev_diff2 = function(x) {
  lab1 = NA
  lab2 = NA
  prev_df = NA
  xi = as.integer(x)
  if (!is.na(i_ke$diff[xi]) & xi > 1) {
    while(is.na(lab1) & is.na(lab2)) {
      xi = xi - 1
      lab1 = i_ke$ifa_labels[xi]
      lab2 = i_ke$annot2_labels[xi]
    }
    prev_df = i_ke$diff[xi]
  }
  return(prev_df)
}

get_next_diff1 = function(x) {
  lab1 = NA
  lab2 = NA
  next_df = NA
  xi = as.integer(x)
  if (!is.na(i_k$diff[xi]) & (xi + 1) != 1) {
    while(is.na(lab1) & is.na(lab2)) {
      xi = xi + 1
      lab1 = i_k$ifa_labels[xi]
      lab2 = i_k$annot2_labels[xi]
    }
    next_df = i_k$diff[xi]
  }
  return(next_df)
}

get_next_diff2 = function(x) {
  lab1 = NA
  lab2 = NA
  next_df = NA
  xi = as.integer(x)
  if (!is.na(i_ke$diff[xi]) & (xi + 1) != 1) {
    while(is.na(lab1) & is.na(lab2)) {
      xi = xi + 1
      lab1 = i_ke$ifa_labels[xi]
      lab2 = i_ke$annot2_labels[xi]
    }
    next_df = i_ke$diff[xi]
  }
  return(next_df)
}

fix_kaldi_diff = function(x) {
  xi = as.integer(x)
  prev_df = i_k$diff[xi]
  if (!is.na(i_k$diff[xi]) & (as.integer(i_k$phon_num[xi]) > 3) & (i_k$sentence[xi] == "M15R2VS21A")) {
    prev_df = NA
    while(is.na(prev_df)) {
      xi = xi - 1
      prev_df = i_k$diff[xi]
    }
  }
  return(prev_df)
}

#### load kaldi - kaldi expanded - ifa file
kki = read.csv(paste(f_path, "validation_data_ifa-kaldi-kaldi-exp_small_pos_dur.csv", sep = ""))

# get phonetic classes
kki$ifa_class = as.factor(sapply(kki$ifa_labels, get_phon_class))
kki$kaldi_class = as.factor(sapply(kki$kaldi_labels, get_phon_class))
kki$kaldi_exp_class = as.factor(sapply(kki$kaldi_exp_labels, get_phon_class))

# save real NAs for later
kki_dur = kki

# turn NA into 'NA'
kki$ifa_labels = as.character(kki$ifa_labels)
kki$ifa_labels[is.na(kki$ifa_labels)] = "NA"
kki$ifa_labels = as.factor(kki$ifa_labels)
kki$kaldi_labels = as.character(kki$kaldi_labels)
kki$kaldi_labels[is.na(kki$kaldi_labels)] = "NA"
kki$kaldi_labels = as.factor(kki$kaldi_labels)
kki$kaldi_exp_labels = as.character(kki$kaldi_exp_labels)
kki$kaldi_exp_labels[is.na(kki$kaldi_exp_labels)] = "NA"
kki$kaldi_exp_labels = as.factor(kki$kaldi_exp_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(kki[, c("ifa_labels", "kaldi_labels")], kki[, c("ifa_labels", "kaldi_exp_labels")])

# expanded KALDI labels are significantly more similar to manual IFA labels

# we can make a confusion matrix using phonetic classes
# turn NA into 'NA'
kki$ifa_class = as.character(kki$ifa_class)
kki$ifa_class[is.na(kki$ifa_class)] = "NA"
kki$ifa_class = as.factor(kki$ifa_class)
kki$kaldi_class = as.character(kki$kaldi_class)
kki$kaldi_class[is.na(kki$kaldi_class)] = "NA"
kki$kaldi_class = as.factor(kki$kaldi_class)
kki$kaldi_exp_class = as.character(kki$kaldi_exp_class)
kki$kaldi_exp_class[is.na(kki$kaldi_exp_class)] = "NA"
kki$kaldi_exp_class = as.factor(kki$kaldi_exp_class)

confusion_i_k = table(kki$ifa_class, kki$kaldi_class)
confusion_i_ke = table(kki$ifa_class, kki$kaldi_exp_class)
confusion_i_k
confusion_i_ke

# notice the increase in NA agreement in IFA - KALDI-exp (compared to KALDI) 
# and decrease in NA-V confusion (83 --> 50) in 
# now let's see whether this affects agreement in segmentation

# first we need the real NAs back, so we use kki_dur
threshold = 0.02
kki_dur$ifa_kaldi_agr = kki$ifa_kaldi_diff < threshold
kki_dur$ifa_kaldi_exp_agr = kki$ifa_kaldi_exp_diff < threshold
kki_dur$kaldi_exp_kaldi_agr = kki$kaldi_exp_kaldi_diff < threshold
# we'll use manual phon class as the gold standard
colnames(kki_dur)[colnames(kki_dur)=="ifa_class"] = "phon_class"
# get prev phon class
kki_dur$phon_id = as.factor(1:nrow(kki_dur))
kki_dur$phon_num = unlist(sapply(unique(as.character(kki_dur$sentence)), function(x) 1:nrow(kki_dur[kki_dur$sentence == x,])))
kki_dur$prev_phon_class = as.factor(sapply(kki_dur$phon_id, get_prev_phon_class))
# first phones in a sentence don't have a preceding phone
kki_dur[kki_dur$phon_num == 1,]$prev_phon_class = NA

# create two dfs
drop = c("kaldi_exp_labels", "ifa_kaldi_exp_diff", "kaldi_exp_kaldi_diff", 
         "ifa_kaldi_exp_agr", "kaldi_exp_kaldi_agr", "kaldi_exp_dur")
i_k = kki_dur[ , !(names(kki_dur) %in% drop)]
i_k$annotator2 = as.factor("kaldi")
i_k$phon_match = as.factor(i_k$phon_class == i_k$kaldi_class)
colnames(i_k)[colnames(i_k)=="kaldi_labels"] = "annot2_labels"
colnames(i_k)[colnames(i_k)=="ifa_kaldi_diff"] = "diff"
colnames(i_k)[colnames(i_k)=="ifa_kaldi_agr"] = "agreement"
colnames(i_k)[colnames(i_k)=="kaldi_dur"] = "dur"
# get prev diff
i_k$prev_diff = sapply(i_k$phon_id, get_prev_diff1)
i_k$next_diff = sapply(i_k$phon_id, get_next_diff1)

drop = c("kaldi_labels", "ifa_kaldi_diff", "kaldi_exp_kaldi_diff", 
         "ifa_kaldi_agr", "kaldi_exp_kaldi_agr", "kaldi_dur")
i_ke = kki_dur[ , !(names(kki_dur) %in% drop)]
i_ke$annotator2 = as.factor("kaldi-exp")
i_ke$phon_match = as.factor(i_ke$phon_class == i_ke$kaldi_exp_class)
colnames(i_ke)[colnames(i_ke)=="kaldi_exp_labels"] = "annot2_labels"
colnames(i_ke)[colnames(i_ke)=="ifa_kaldi_exp_diff"] = "diff"
colnames(i_ke)[colnames(i_ke)=="ifa_kaldi_exp_agr"] = "agreement"
colnames(i_ke)[colnames(i_ke)=="kaldi_exp_dur"] = "dur"
# get prev diff
i_ke$prev_diff = sapply(i_ke$phon_id, get_prev_diff2)
i_ke$next_diff = sapply(i_ke$phon_id, get_next_diff2)
# combine in a long df
kki_long = rbind(i_k, i_ke)
colnames(kki_long)[colnames(kki_long)=="kaldi_pos"] = "position"
kki_long[kki_long$position == "SIL",]$position = NA
kki_long = kki_long[kki_long$diff < 0.1 & kki_long$phon_num != 1,]
kki_long = kki_long[!is.na(kki_long$sentence),]
kki_long$speaker = as.factor(substr(kki_long$sentence, 1, 4))
# make model
kki_long = na.omit(kki_long)

# one mystery solved, now for the main correlation
# SOLUTION: I shouldn't have included phon_id as random variable, it created the correlation
#           it linked kaldi and kaldi_exp diffs, so if you knew 1 you also knew the other


# hypothesis annotator2*position: especially E and I should be better because KALDI 
# won't try to squeeze in unpronounced phons
LMM4 = lmer(sqrt(diff) ~ annotator2*position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 + annotator2 | ifa_labels),
            data = kki_long)

par(mfrow=c(1,3))
plot(fitted(LMM4), residuals(LMM4))
cor(fitted(LMM4), residuals(LMM4))
qqnorm(residuals(LMM4))
qqline(residuals(LMM4))
plot(density(residuals(LMM4)))

par(mfrow=c(1,1))
plot(fitted(LMM4), residuals(LMM4), xlim = c(0.08, 0.13), ylim = c(-0.05, 0), type = "n")
text(fitted(LMM4), residuals(LMM4), labels = kki_long$phon_id)
# inspection shows that phon_ids starting from 350 show a very strong trend --> M15R2VS21A
rows = which(kki_long$sentence == "M15R2VS21A")
phon_ids = kki_long[kki_long$sentence == "M15R2VS21A",]$phon_id
plot(fitted(LMM4)[rows], residuals(LMM4)[rows], xlim = c(0.08, 0.13), ylim = c(-0.05, 0), type = "n")
text(fitted(LMM4)[rows], residuals(LMM4)[rows], labels = phon_ids)
# identified specific phon nums:
# for 383, 410 residuals goes down while diff goes up for kaldi
kki_dur[kki_dur$phon_id %in% c(383, 410),]
# I checked in praat and for those phones ifa_kaldi_exp_diff actually represents 
# the starting boundary of that phone, and ifa_kaldi_diff represents the ending
# boundary of that phone, i.e. shifted ifa_kaldi_diff, e.g.:
kki_dur[kki_dur$phon_id %in% c(382, 383, 384),]
# fix it:
i_k$diff = sapply(i_k$phon_id, fix_kaldi_diff)
# remake df
kki_long = rbind(i_k, i_ke)
colnames(kki_long)[colnames(kki_long)=="kaldi_pos"] = "position"
kki_long[kki_long$position == "SIL",]$position = NA
kki_long = kki_long[kki_long$diff < 0.1 & kki_long$phon_num != 1,]
kki_long = kki_long[!is.na(kki_long$sentence),]
kki_long$speaker = as.factor(substr(kki_long$sentence, 1, 4))
kki_long = na.omit(kki_long)

# refit model and inspect residuals
# removed random slopes because for annotator2 correlation was 1
LMM5 = lmer(sqrt(diff) ~ annotator2*position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kki_long)

par(mfrow=c(2,3))
plot(fitted(LMM5), residuals(LMM5))
cor(fitted(LMM5), residuals(LMM5))
qqnorm(residuals(LMM5))
qqline(residuals(LMM5))
plot(density(residuals(LMM5)))

kki_long_tr = kki_long[abs(scale(resid(LMM5))) < 2.5,]

LMM6 = lmer(sqrt(diff) ~ annotator2*position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kki_long_tr)

plot(fitted(LMM6), residuals(LMM6))
cor(fitted(LMM6), residuals(LMM6))
qqnorm(residuals(LMM6))
qqline(residuals(LMM6))
plot(density(residuals(LMM6)))

par(mfrow=c(1,1))

summary(LMM6)
anova(LMM6, type = 2)
plot(effect("annotator2:position", LMM6))

pairwise = as.data.frame(ls_means(LMM6, pairwise = TRUE, which = "annotator2:position"))
pairwise[c(1,14,23,28), names(pairwise) %in% c("t value", "Pr(>|t|)")]

LMM7 = lmer(sqrt(diff) ~ annotator2 + position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kki_long)

kki_long_tr = kki_long[abs(scale(resid(LMM7))) < 2.5,]

LMM8 = lmer(sqrt(diff) ~ annotator2 + position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kki_long_tr)

summary(LMM8)
anova(LMM8, type = 2)
plot(effect("annotator2", LMM8))

#############################
# word boundaries across components

wb_a = read.csv(paste(cgn_path, "comp-a_word_boundaries.csv", sep = ""))
wb_a$register = as.factor("face-to-face")
wb_c = read.csv(paste(cgn_path, "comp-c_word_boundaries.csv", sep = ""))
wb_c$register = as.factor("telephone")
wb_k = read.csv(paste(cgn_path, "comp-k_word_boundaries.csv", sep = ""))
wb_k$register = as.factor("news")
wb_o = read.csv(paste(cgn_path, "comp-o_word_boundaries.csv", sep = ""))
wb_o$register = as.factor("reading")

wb = rbind(wb_a, wb_c, wb_k, wb_o)
wb$word = as.factor(sub("[.,!? ]+", "", sub("\\*.*", "", as.character(wb$raw_word), perl = TRUE), perl = TRUE))
wb$register = relevel(wb$register, ref="telephone")
wb$start_phon = substr(as.character(wb$kaldi_tran), 1, 1)
wb$start_class = as.factor(sapply(wb$start_phon, get_phon_class))
wb$end_phon = substr(as.character(wb$kaldi_tran), nchar(as.character(wb$kaldi_tran)), nchar(as.character(wb$kaldi_tran)))
wb$end_class = as.factor(sapply(wb$end_phon, get_phon_class))

get_chunk_pos = function(x) {
  if (wb$word_chunk_i[x] == 1) {
    if ((x == nrow(wb)) | (wb$word_chunk_i[x + 1] == 1)) {
      chunk_position = as.factor("single-word")
    } else {
      chunk_position = as.factor("initial")
    }
  } else {
    if ((x == nrow(wb)) | (wb$word_chunk_i[x + 1] == 1)) {
      chunk_position = as.factor("final")
    } else {
      chunk_position = as.factor("medial")
    }
  }
}

wb$word_id = 1:nrow(wb)
wb$chunk_position = sapply(wb$word_id, get_chunk_pos)

wb$word_dur = wb$wrd_end - wb$wrd_start

wb$time_to_start = wb$kaldi_start - wb$wrd_start
wb$time_to_end = wb$kaldi_end - wb$wrd_end

plot(density(wb$time_to_start, bw = 0.02, na.rm = TRUE), xlim = c(-0.2,0.2), main = "Distribution of time between KALDI and manual word boundaries")
lines(density(wb$time_to_end, bw = 0.02, na.rm = TRUE), xlim = c(-0.2,0.2), lty = "dashed")
abline(v = 0)

wb1 = wb[wb$time_to_start > -0.2 & wb$time_to_start < 0.2, ]

# models

start1 = lmer(cbrt(time_to_start) ~ register + start_class + chunk_position + word_dur + hnr 
              + (1 | word) + (1 | speaker), data = wb1)

par(mfrow=c(1,3))
plot(fitted(start1), residuals(start1))
cor(fitted(start1), residuals(start1))
qqnorm(residuals(start1))
qqline(residuals(start1))
plot(density(residuals(start1)))

# bimodal --> investigate
# due to 0 differences at start of chunks
#wb_k = na.omit(wb[wb$register == "news" & !(wb$chunk_position %in% c("initial", "single-word")),])
#wb_k$chunk_position = as.factor(as.character(wb_k$chunk_position))

#start2 = lmer(time_to_start ~ start_class + chunk_position + word_dur + hnr 
#              + (1 | word) + (1 | speaker), data = wb_k)

#plot(fitted(start2), residuals(start2), ylim = c(-0.03,-0.02), xlim = c(0.02,0.03), type = "n")
#text(fitted(start2), residuals(start2), labels = wb_k$word_id)

wb2 = wb[!(wb$chunk_position %in% c("initial", "single-word")),]
wb2$chunk_position = as.factor(as.character(wb2$chunk_position))
wb2 = wb2[wb2$time_to_start > -0.2 & wb2$time_to_start < 0.2, ]

start2 = lmer(time_to_start ~ register + start_class + chunk_position + word_dur + hnr 
              + (1 | word) + (1 | speaker), data = wb2)

par(mfrow=c(1,3))
plot(fitted(start2), residuals(start2))
cor(fitted(start2), residuals(start2))
qqnorm(residuals(start2))
qqline(residuals(start2))
plot(density(residuals(start2)))

# bimodality is gone but still leptokurtic

cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}


start3 = lmer(sqrt(abs(time_to_start)) ~ register + start_class + chunk_position + word_dur + hnr 
              + (1 | word) + (1 | speaker), data = wb2)

par(mfrow=c(1,3))
plot(fitted(start3), residuals(start3))
cor(fitted(start3), residuals(start3))
qqnorm(residuals(start3))
qqline(residuals(start3))
plot(density(residuals(start3)))

wb_tr = wb2[abs(scale(resid(start3))) < 2.5,]

start4 = lmer(sqrt(abs(time_to_start)) ~ register + start_class + chunk_position + word_dur + hnr 
              + (1 | word) + (1 | speaker), data = wb_tr)

plot(fitted(start4), residuals(start4))
cor(fitted(start4), residuals(start4))
qqnorm(residuals(start4))
qqline(residuals(start4))
plot(density(residuals(start4)))
par(mfrow=c(1,1))

summary(start4)
anova(start4, type = 2)
plot(effect("register", start4))
plotmeans(time_to_start ~ register, data = wb_tr)
plot(effect("hnr", start4))

# interesting:
start5 = lmer(sqrt(abs(time_to_start)) ~ register*hnr + start_class + chunk_position + word_dur  
              + (1 | word) + (1 | speaker), data = wb_tr)
anova(start5, type = 2)
plot(effect("register:hnr", start5))
# some collinearity checks
summary(lmer(sqrt(abs(time_to_start)) ~ register + (1 | word) + (1 | speaker), data = wb_tr))$coefficients
summary(lmer(sqrt(abs(time_to_start)) ~ hnr + (1 | word) + (1 | speaker), data = wb_tr))$coefficients
summary(lmer(sqrt(abs(time_to_start)) ~ register + hnr + (1 | word) + (1 | speaker), data = wb_tr))$coefficients


# end boundaries

wb3 = wb[!(wb$chunk_position %in% c("final", "single-word")),]
wb3$chunk_position = as.factor(as.character(wb3$chunk_position))
wb3 = wb3[wb3$time_to_end > -0.1 & wb3$time_to_end < 0.1, ]

end1 = lmer(sqrt(abs(time_to_end)) ~ register + end_class + chunk_position + word_dur + hnr 
              + (1 | word) + (1 | speaker), data = wb3)

par(mfrow=c(2,3))
plot(fitted(end1), residuals(end1))
cor(fitted(end1), residuals(end1))
qqnorm(residuals(end1))
qqline(residuals(end1))
plot(density(residuals(end1)))

wb_tr = wb3[abs(scale(resid(end1))) < 2.5,]

end2 = lmer(sqrt(abs(time_to_end)) ~ register + end_class + chunk_position + word_dur + hnr 
            + (1 | word) + (1 | speaker), data = wb_tr)

plot(fitted(end2), residuals(end2))
cor(fitted(end2), residuals(end2))
qqnorm(residuals(end2))
qqline(residuals(end2))
plot(density(residuals(end2)))
par(mfrow=c(1,1))

anova(end2, type = 2)
plot(effect("register", end2))




