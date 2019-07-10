library(effects)
library(lmerTest)


if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/IFAcorpus/"
  wd = "/Volumes/timzee/GitHub/dmc-scripts/"
} else {
  f_path = "/vol/tensusers/timzee/IFAcorpus/"
  wd = "/home/timzee/GitHub/dmc-scripts/"
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
  if (!is.na(kti_dur$phon_class[xi]) & xi > 1) {
    while(is.na(prev_pc)) {
      xi = xi - 1
      prev_pc = as.character(kti_dur$phon_class[xi])
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
  if (!is.na(i_t$diff[xi]) & xi > 1) {
    while(is.na(lab1) & is.na(lab2)) {
      xi = xi - 1
      lab1 = i_t$ifa_labels[xi]
      lab2 = i_t$annot2_labels[xi]
    }
    prev_df = i_t$diff[xi]
  }
  return(prev_df)
}

get_next_diff1 = function(x) {
  lab1 = NA
  lab2 = NA
  next_df = NA
  xi = as.integer(x)
  if (!is.na(i_k$diff[xi]) & (xi + 1) != 1 & xi < nrow(i_k)) {
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
  if (!is.na(i_t$diff[xi]) & (xi + 1) != 1 & xi < nrow(i_k)) {
    while(is.na(lab1) & is.na(lab2)) {
      xi = xi + 1
      lab1 = i_t$ifa_labels[xi]
      lab2 = i_t$annot2_labels[xi]
    }
    next_df = i_t$diff[xi]
  }
  return(next_df)
}


## load IFA - KALDI - Tim 
kti = read.csv(paste(f_path, "validation_data_small_pos_dur.csv", sep = ""))

# get phonetic classes
kti$ifa_class = as.factor(sapply(kti$ifa_labels, get_phon_class))
kti$kaldi_class = as.factor(sapply(kti$kaldi_labels, get_phon_class))
kti$tim_class = as.factor(sapply(kti$tim_labels, get_phon_class))

# save real NAs for later
kti_dur = kti

# turn NA into 'NA'
kti$ifa_labels = as.character(kti$ifa_labels)
kti$ifa_labels[is.na(kti$ifa_labels)] = "NA"
kti$ifa_labels = as.factor(kti$ifa_labels)
kti$kaldi_labels = as.character(kti$kaldi_labels)
kti$kaldi_labels[is.na(kti$kaldi_labels)] = "NA"
kti$kaldi_labels = as.factor(kti$kaldi_labels)
kti$tim_labels = as.character(kti$tim_labels)
kti$tim_labels[is.na(kti$tim_labels)] = "NA"
kti$tim_labels = as.factor(kti$tim_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(kti[, c("ifa_labels", "kaldi_labels")], kti[, c("ifa_labels", "tim_labels")])

# Tim labels are significantly more similar to manual IFA labels

# we can make a confusion matrix
# turn NA into 'NA'
kti$ifa_class = as.character(kti$ifa_class)
kti$ifa_class[is.na(kti$ifa_class)] = "NA"
kti$ifa_class = as.factor(kti$ifa_class)
kti$kaldi_class = as.character(kti$kaldi_class)
kti$kaldi_class[is.na(kti$kaldi_class)] = "NA"
kti$kaldi_class = as.factor(kti$kaldi_class)
kti$tim_class = as.character(kti$tim_class)
kti$tim_class[is.na(kti$tim_class)] = "NA"
kti$tim_class = as.factor(kti$tim_class)

confusion_i_k = table(kti$ifa_class, kti$kaldi_class)
confusion_i_t = table(kti$ifa_class, kti$tim_class)
confusion_i_k
confusion_i_t

# now let's see whether this affects agreement in segmentation

# first we need the real NAs back, so we use kki_dur
threshold = 0.02
kti_dur$ifa_kaldi_agr = kti$ifa_kaldi_diff < threshold
kti_dur$ifa_tim_agr = kti$ifa_tim_diff < threshold
kti_dur$tim_kaldi_agr = kti$tim_kaldi_diff < threshold
# we'll use manual phon class as the gold standard
colnames(kti_dur)[colnames(kti_dur)=="ifa_class"] = "phon_class"
# get prev phon class
kti_dur$phon_id = as.factor(1:nrow(kti_dur))
kti_dur$phon_num = unlist(sapply(unique(as.character(kti_dur$sentence)), function(x) 1:nrow(kti_dur[kti_dur$sentence == x,])))
kti_dur$prev_phon_class = as.factor(sapply(kti_dur$phon_id, get_prev_phon_class))
# first phones in a sentence don't have a preceding phone
kti_dur[kti_dur$phon_num == 1,]$prev_phon_class = NA

# create two dfs
drop = c("tim_labels", "ifa_tim_diff", "tim_kaldi_diff", 
         "ifa_tim_agr", "tim_kaldi_agr")
i_k = kti_dur[ , !(names(kti_dur) %in% drop)]
i_k$annotator2 = as.factor("kaldi")
i_k$phon_match = as.factor(i_k$phon_class == i_k$kaldi_class)
colnames(i_k)[colnames(i_k)=="kaldi_labels"] = "annot2_labels"
colnames(i_k)[colnames(i_k)=="ifa_kaldi_diff"] = "diff"
colnames(i_k)[colnames(i_k)=="ifa_kaldi_agr"] = "agreement"
# get prev diff
i_k$prev_diff = sapply(i_k$phon_id, get_prev_diff1)
i_k$next_diff = sapply(i_k$phon_id, get_next_diff1)

drop = c("kaldi_labels", "ifa_kaldi_diff", "tim_kaldi_diff", 
         "ifa_kaldi_agr", "tim_kaldi_agr")
i_t = kti_dur[ , !(names(kti_dur) %in% drop)]
i_t$annotator2 = as.factor("tim")
i_t$phon_match = as.factor(i_t$phon_class == i_t$tim_class)
colnames(i_t)[colnames(i_t)=="tim_labels"] = "annot2_labels"
colnames(i_t)[colnames(i_t)=="ifa_tim_diff"] = "diff"
colnames(i_t)[colnames(i_t)=="ifa_tim_agr"] = "agreement"
# get prev diff
i_t$prev_diff = sapply(i_t$phon_id, get_prev_diff2)
i_t$next_diff = sapply(i_t$phon_id, get_next_diff2)
# combine in a long df
kti_long = rbind(i_k, i_t)
colnames(kti_long)[colnames(kti_long)=="kaldi_pos"] = "position"
colnames(kti_long)[colnames(kti_long)=="kaldi_dur"] = "dur"
kti_long[kti_long$position == "SIL",]$position = NA
kti_long = kti_long[kti_long$diff < 0.1 & kti_long$phon_num != 1,]
kti_long = kti_long[!is.na(kti_long$sentence),]
kti_long$speaker = as.factor(substr(kti_long$sentence, 1, 4))
# make model
kti_long = na.omit(kti_long)

# removed random slopes for annotator2 because of perfect correlation
agr1 = lmer(sqrt(diff) ~ annotator2*position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kti_long)

par(mfrow = c(2,3))
plot(fitted(agr1), residuals(agr1))
cor(fitted(agr1), residuals(agr1))
qqnorm(residuals(agr1))
qqline(residuals(agr1))
plot(density(residuals(agr1)))

kti_long_tr = kti_long[abs(scale(resid(agr1))) < 2.5,]

agr2 = lmer(sqrt(diff) ~ annotator2*position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kti_long_tr)

plot(fitted(agr2), residuals(agr2))
cor(fitted(agr2), residuals(agr2))
qqnorm(residuals(agr2))
qqline(residuals(agr2))
plot(density(residuals(agr2)))

par(mfrow = c(1,1))
summary(agr2)
anova(agr2, type = 2)
plot(effect("annotator2:position", agr2))

# interaction marignally significant, let's check pairwise comparisons before we get rid of it

pairwise = as.data.frame(ls_means(agr2, pairwise = TRUE, which = "annotator2:position"))
pairwise[c(1,14,23,28), names(pairwise) %in% c("t value", "Pr(>|t|)")]

agr3 = lmer(sqrt(diff) ~ annotator2 + position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 | ifa_labels),
            data = kti_long)

par(mfrow = c(2,3))
plot(fitted(agr3), residuals(agr3))
cor(fitted(agr3), residuals(agr3))
qqnorm(residuals(agr3))
qqline(residuals(agr3))
plot(density(residuals(agr3)))

kti_long_tr = kti_long[abs(scale(resid(agr3))) < 2.5,]

agr4 = lmer(sqrt(diff) ~ annotator2 + position + phon_class + prev_phon_class 
            + phon_match + dur + prev_diff + next_diff
            + (1 + annotator2 | ifa_labels),
            data = kti_long_tr)

plot(fitted(agr4), residuals(agr4))
cor(fitted(agr4), residuals(agr4))
qqnorm(residuals(agr4))
qqline(residuals(agr4))
plot(density(residuals(agr4)))

par(mfrow = c(1,1))
summary(agr4)
anova(agr4, type = 2)
plot(effect("position", agr4))
plot(effect("phon_class", agr4))
plot(effect("prev_phon_class", agr4))
######


agr_tbl = table(agr_man[,c("ifa_kaldi_agr")])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(agr_man[is.na(agr_man$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr_man[,c("ifa_tim_agr")])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(agr_man[is.na(agr_man$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr_man[,c("tim_kaldi_agr")])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(agr_man[is.na(agr_man$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))



agr_man$phon_class = as.factor(sapply(agr_man$ifa_labels, get_phon_class))
agr_man$phon_id = as.factor(1:nrow(agr_man))
# get indices of phones in sentence
agr_man$phon_num = unlist(sapply(unique(as.character(agr_man$sentence)), function(x) 1:nrow(agr_man[agr_man$sentence == x,])))
# get preceding ifa phon class
#agr_man$prev_phon_class = as.factor(c(NA, as.character(agr_man$phon_class[1:nrow(agr_man) - 1])))
agr_man$prev_phon_class = as.factor(sapply(agr_man$phon_id, get_prev_phon_class))
agr_man[agr_man$phon_num == 1,]$prev_phon_class = NA

agr_m_i_k = agr_man[, c(1,2,4,6,8,9,13,14,15,16)]
agr_m_i_k$annotator2 = "Kaldi"
names(agr_m_i_k) = c("sentence", "ifa_labels", "annot2_labels", "diff", "position", "agreement", "phon_class", "phon_id", "phon_num", "prev_phon_class", "annotator2")
agr_m_i_t = agr_man[, c(1,2,3,5,8,10,13,14,15,16)]
agr_m_i_t$annotator2 = "Tim"
names(agr_m_i_t) = c("sentence", "ifa_labels", "annot2_labels", "diff", "position", "agreement", "phon_class", "phon_id", "phon_num", "prev_phon_class", "annotator2")
agr_long = rbind(agr_m_i_k, agr_m_i_t)
agr_long$annotator2 = as.factor(agr_long$annotator2)
agr_long[agr_long$position == "SIL",]$position = NA
agr_long2 = agr_long[agr_long$diff < 1 & agr_long$phon_num != 1,]
agr_long2 = agr_long2[!is.na(agr_long2$sentence),]
agr_long2$speaker = as.factor(substr(agr_long2$sentence, 1, 4))

#mod1 = glm(agreement ~ annotator2*position + phon_class, data = agr_long, family = binomial)
#summary(mod1)

#par(mfrow = c(1,1))
#plot(effect("phon_class", mod1))
#plot(effect("annotator2:position", mod1))


#hist((agr_long2$diff^(1/3)))
#plot(density(agr_long2$diff^(1/3), na.rm = TRUE))
#agr_long2$diff_cube_rt = agr_long2$diff^(1/3)
#agr_long2 = agr_long2[rowSums(is.na(agr_long2)) != ncol(agr_long2),]
#mod2 = lm(diff_cube_rt ~ annotator2*position + phon_class, data = agr_long2)

agr_long2$phon_id = as.factor(agr_long2$phon_id)
LMM = lmer(diff ~ annotator2*position + phon_class + prev_phon_class + (1|phon_id),
           data = agr_long2)
plot(effect("annotator2:position", LMM))

LMM2 = lmer(diff ~ annotator2 + position + phon_class + prev_phon_class + (1|phon_id),
            data = agr_long2)
plot(effect("annotator2:position", LMM))




#do Box-Cox test instead to normalize boundary difference
#Box-Cox only works with positive response variable
agr_long2$diff = agr_long2$diff + 0.0001
mod2 = lm(diff ~ annotator2*position + phon_class + prev_phon_class, data = agr_long2)
bc = boxcox(mod2)
lambda = bc$x[which.max(bc$y)]
mod3 = lm(diff^lambda ~ annotator2*position + phon_class + prev_phon_class, data = agr_long2)
qqnorm(mod2$residuals)
qqline(mod2$residuals)
qqnorm(mod3$residuals)
qqline(mod3$residuals)

summary(mod3)
# get collinearity estimates
vif(mod3)
cramerV(as.character(agr_long2$annotator2), as.character(agr_long2$position), bias.correct=TRUE)
cramerV(as.character(agr_long2$annotator2), as.character(agr_long2$phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$annotator2), as.character(agr_long2$prev_phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$position), as.character(agr_long2$phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$position), as.character(agr_long2$prev_phon_class), bias.correct=TRUE)
cramerV(as.character(agr_long2$phon_class), as.character(agr_long2$prev_phon_class), bias.correct=TRUE)
# Collinearity due to phonotactics
# high vif of interaction term is due to its association with the main predictors that form it

par(mfrow = c(1,1))
plot(effect("phon_class", mod3))
# result shows us we should not consider sentence initial phones (mostly silences) as these have a difference of 0 by definition
plot(effect("prev_phon_class", mod3))
plot(effect("annotator2:position", mod3))

#######

i_k_ins = agr2[(is.na(agr2$ifa_labels) & !is.na(agr2$kaldi_labels)) | (!is.na(agr2$ifa_labels) & is.na(agr2$kaldi_labels)), ]
prop_k_ins = nrow(i_k_ins) / nrow(agr2[!is.na(agr2$kaldi_labels) | !is.na(agr2$ifa_labels),])
prop_k_ins
i_t_ins = agr2[(is.na(agr2$ifa_labels) & !is.na(agr2$tim_labels)) | (!is.na(agr2$ifa_labels) & is.na(agr2$tim_labels)), ]
prop_t_ins = nrow(i_t_ins) / nrow(agr2[!is.na(agr2$tim_labels) | !is.na(agr2$ifa_labels),])
prop_t_ins

par(mfrow = c(1,3))
agr_tbl = table(agr2[,agr2$ifa_kaldi_agr])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(agr2[is.na(agr2$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N = ", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr2[,agr2$ifa_tim_agr])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(agr2[is.na(agr2$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N = ", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(agr2[,agr2$tim_kaldi_agr])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(agr2[is.na(agr2$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N = ", as.character(num_t_k), sep = ""), ylim = c(0, 1))

s_agr2 = agr2[agr2$kaldi_labels == "s" & agr2$kaldi_pos == "E",]
s_agr2 = s_agr2[rowSums(is.na(s_agr2)) != ncol(s_agr2),]

par(mfrow = c(1,3))
agr_tbl = table(s_agr2[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(s_agr2[is.na(s_agr2$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(s_agr2[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(s_agr2[is.na(s_agr2$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(s_agr2[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(s_agr2[is.na(s_agr2$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))

sub_agr = agr2[!is.na(agr2$ifa_labels) & !is.na(agr2$kaldi_labels) & !is.na(agr2$tim_labels),]

par(mfrow = c(1,3))
agr_tbl = table(sub_agr[,c(9)])
prop_tbl = prop.table(agr_tbl)
num_i_k = nrow(sub_agr[is.na(sub_agr$ifa_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - kaldi N=", as.character(num_i_k), sep = ""), ylim = c(0, 1))
agr_tbl = table(sub_agr[,c(10)])
prop_tbl = prop.table(agr_tbl)
num_i_t = nrow(sub_agr[is.na(sub_agr$ifa_tim_diff) == FALSE,])
barplot(prop_tbl, main = paste("ifa - tim N=", as.character(num_i_t), sep = ""), ylim = c(0, 1))
agr_tbl = table(sub_agr[,c(11)])
prop_tbl = prop.table(agr_tbl)
num_t_k = nrow(sub_agr[is.na(sub_agr$tim_kaldi_diff) == FALSE,])
barplot(prop_tbl, main = paste("tim - kaldi N=", as.character(num_t_k), sep = ""), ylim = c(0, 1))
