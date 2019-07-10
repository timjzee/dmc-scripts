
if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/IFAcorpus/"
  cgn_path = "/Volumes/tensusers/timzee/cgn/n_tests/"
  wd = "/Volumes/timzee/GitHub/dmc-scripts/"
} else {
  f_path = "/vol/tensusers/timzee/IFAcorpus/"
  cgn_path = "/vol/tensusers/timzee/cgn/n_tests/"
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

get_word_i = function(x) {
  xi = as.integer(x)
  cur_sen = kti$sentence[xi]
  sen_start_i = kti[kti$sentence == cur_sen & kti$phon_num == 1,]$phon_id
  sen_start_i = sen_start_i - 1
  word_i = 0
  while (sen_start_i != xi) {
    sen_start_i = sen_start_i + 1
    position = kti$kaldi_pos[sen_start_i]
    if (position %in% c("S", "B") & !is.na(kti$kaldi_labels[sen_start_i])) {
      word_i = word_i + 1
    }
  }
  return(word_i)
}

get_word = function(x) {
  xi = as.integer(x)
  sentence = kti2$word_l[[xi]]
  word_n = kti2$word_num[xi]
  word = sentence[word_n]
  return(word)
}

get_word_i2 = function(x) {
  xi = as.integer(x)
  cur_sen = kni$sentence[xi]
  sen_start_i = kni[kni$sentence == cur_sen & kni$phon_num == 1,]$phon_id
  sen_start_i = sen_start_i - 1
  word_i = 0
  while (sen_start_i != xi) {
    sen_start_i = sen_start_i + 1
    position = kni$kaldi_pos[sen_start_i]
    if (position %in% c("S", "B") & !is.na(kni$kaldi_n_labels[sen_start_i])) {
      word_i = word_i + 1
    }
  }
  return(word_i)
}

get_word2 = function(x) {
  xi = as.integer(x)
  sentence = kni2$word_l[[xi]]
  word_n = kni2$word_num[xi]
  word = sentence[word_n]
  return(word)
}

get_word_i3 = function(x) {
  xi = as.integer(x)
  cur_sen = knc$sentence[xi]
  sen_start_i = knc[knc$sentence == cur_sen & knc$phon_num == 1,]$phon_id
  sen_start_i = sen_start_i - 1
  word_i = 0
  while (sen_start_i != xi) {
    sen_start_i = sen_start_i + 1
    position = knc$kaldi_pos[sen_start_i]
    if (position %in% c("S", "B") & !is.na(knc$kaldi_n_labels[sen_start_i])) {
      word_i = word_i + 1
    }
  }
  print(xi)
  return(word_i)
}

get_word3 = function(x) {
  xi = as.integer(x)
  sentence = knc2$word_l[[xi]]
  word_n = knc2$word_num[xi]
  word = sentence[word_n]
  return(word)
}


## load IFA - KALDI - Tim 
kti = read.csv(paste(f_path, "validation_data_small_pos_dur.csv", sep = ""))

# get phonetic classes
kti$ifa_class = as.factor(sapply(kti$ifa_labels, get_phon_class))
kti$kaldi_class = as.factor(sapply(kti$kaldi_labels, get_phon_class))
kti$tim_class = as.factor(sapply(kti$tim_labels, get_phon_class))

# get word number per sentence
kti$phon_id = 1:nrow(kti)
kti$phon_num = unlist(sapply(unique(as.character(kti$sentence)), function(x) 1:nrow(kti[kti$sentence == x,])))
kti$word_num = sapply(kti$phon_id, get_word_i)
kti[kti$kaldi_pos == "SIL",]$word_num = NA

# get actual words
ort = read.csv(paste(f_path, "IFA_sentences_ort.csv", sep = ""))
kti2 = merge(kti, ort)
kti2$sen_num = sapply(kti2$sentence, match, unique(as.character(kti2$sentence)))

kti2$word_l = as.vector(sapply(as.character(kti2$ort), strsplit, " "))
kti2$phon_id = 1:nrow(kti2)
kti2$word = sapply(kti2$phon_id, get_word)
kti2 = kti2[, names(kti2) != "word_l"]
kti_schwa = kti2[substr(kti2$word, nchar(kti2$word) - 1, nchar(kti2$word)) == "en" & nchar(kti2$word) > 2,]
kti_schwa = kti_schwa[!(substr(kti_schwa$word, nchar(kti_schwa$word) - 2, nchar(kti_schwa$word) - 2) 
                          %in% c("o", "e")),]
kti_schwa = kti_schwa[kti_schwa$kaldi_pos == "E",]
# get rid of endings that have no schwa or n
kti_schwa = kti_schwa[(kti_schwa$kaldi_labels %in% c("@", "n")) | is.na(kti_schwa$kaldi_labels),]
kti_schwa = kti_schwa[rowSums(is.na(kti_schwa))<length(kti_schwa),]

# turn NA into 'NA'
kti_schwa$ifa_labels = as.character(kti_schwa$ifa_labels)
kti_schwa$ifa_labels[is.na(kti_schwa$ifa_labels)] = "NA"
kti_schwa$ifa_labels = as.factor(kti_schwa$ifa_labels)
kti_schwa$kaldi_labels = as.character(kti_schwa$kaldi_labels)
kti_schwa$kaldi_labels[is.na(kti_schwa$kaldi_labels)] = "NA"
kti_schwa$kaldi_labels = as.factor(kti_schwa$kaldi_labels)
kti_schwa$tim_labels = as.character(kti_schwa$tim_labels)
kti_schwa$tim_labels[is.na(kti_schwa$tim_labels)] = "NA"
kti_schwa$tim_labels = as.factor(kti_schwa$tim_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(kti_schwa[, c("ifa_labels", "kaldi_labels")], kti_schwa[, c("ifa_labels", "tim_labels")])

# we can make a confusion matrix
# turn NA into 'NA'
kti_schwa$ifa_class = as.character(kti_schwa$ifa_class)
kti_schwa$ifa_class[is.na(kti_schwa$ifa_class)] = "NA"
kti_schwa$ifa_class = as.factor(kti_schwa$ifa_class)
kti_schwa$kaldi_class = as.character(kti_schwa$kaldi_class)
kti_schwa$kaldi_class[is.na(kti_schwa$kaldi_class)] = "NA"
kti_schwa$kaldi_class = as.factor(kti_schwa$kaldi_class)
kti_schwa$tim_class = as.character(kti_schwa$tim_class)
kti_schwa$tim_class[is.na(kti_schwa$tim_class)] = "NA"
kti_schwa$tim_class = as.factor(kti_schwa$tim_class)

confusion_i_k = table(kti_schwa$ifa_class, kti_schwa$kaldi_class)
confusion_i_t = table(kti_schwa$ifa_class, kti_schwa$tim_class)
confusion_i_k
confusion_i_t

# Kaldi has unwarranted n in 35%, matching schwa in 42%, matching n in 12%
# Only 25% of Kaldi /n/s were actually there

## Now let's do the same for kaldi segmentation with penalty for final [n]

kni = read.csv(paste(f_path, "validation_data_ifa-kaldi-kaldi-n_small_pos_dur.csv", sep = ""))

# get phonetic classes
kni$ifa_class = as.factor(sapply(kni$ifa_labels, get_phon_class))
kni$kaldi_class = as.factor(sapply(kni$kaldi_labels, get_phon_class))
kni$kaldi_n_class = as.factor(sapply(kni$kaldi_n_labels, get_phon_class))

# get word number per sentence
kni$phon_id = 1:nrow(kni)
kni$phon_num = unlist(sapply(unique(as.character(kni$sentence)), function(x) 1:nrow(kni[kni$sentence == x,])))
kni$word_num = sapply(kni$phon_id, get_word_i2)
kni[kni$kaldi_pos == "SIL",]$word_num = NA

# get actual words
ort = read.csv(paste(f_path, "IFA_sentences_ort.csv", sep = ""))
kni2 = merge(kni, ort)
kni2$sen_num = sapply(kni2$sentence, match, unique(as.character(kni2$sentence)))

kni2$word_l = as.vector(sapply(as.character(kni2$ort), strsplit, " "))
kni2$phon_id = 1:nrow(kni2)
kni2$word = sapply(kni2$phon_id, get_word2)
kni2 = kni2[, names(kni2) != "word_l"]
kni_schwa = kni2[substr(kni2$word, nchar(kni2$word) - 1, nchar(kni2$word)) == "en" & nchar(kni2$word) > 2,]
kni_schwa = kni_schwa[!(substr(kni_schwa$word, nchar(kni_schwa$word) - 2, nchar(kni_schwa$word) - 2) 
                        %in% c("o", "e")),]
kni_schwa = kni_schwa[kni_schwa$kaldi_pos == "E",]
# get rid of endings that have no schwa or n
kni_schwa = kni_schwa[(kni_schwa$kaldi_n_labels %in% c("@", "n")) | is.na(kni_schwa$kaldi_n_labels),]
kni_schwa = kni_schwa[rowSums(is.na(kni_schwa))<length(kni_schwa),]

# turn NA into 'NA'
kni_schwa$ifa_labels = as.character(kni_schwa$ifa_labels)
kni_schwa$ifa_labels[is.na(kni_schwa$ifa_labels)] = "NA"
kni_schwa$ifa_labels = as.factor(kni_schwa$ifa_labels)
kni_schwa$kaldi_labels = as.character(kni_schwa$kaldi_labels)
kni_schwa$kaldi_labels[is.na(kni_schwa$kaldi_labels)] = "NA"
kni_schwa$kaldi_labels = as.factor(kni_schwa$kaldi_labels)
kni_schwa$kaldi_n_labels = as.character(kni_schwa$kaldi_n_labels)
kni_schwa$kaldi_n_labels[is.na(kni_schwa$kaldi_n_labels)] = "NA"
kni_schwa$kaldi_n_labels = as.factor(kni_schwa$kaldi_n_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(kni_schwa[, c("ifa_labels", "kaldi_n_labels")], kni_schwa[, c("ifa_labels", "kaldi_labels")])

# we can make a confusion matrix
# turn NA into 'NA'
kni_schwa$ifa_class = as.character(kni_schwa$ifa_class)
kni_schwa$ifa_class[is.na(kni_schwa$ifa_class)] = "NA"
kni_schwa$ifa_class = as.factor(kni_schwa$ifa_class)
kni_schwa$kaldi_class = as.character(kni_schwa$kaldi_class)
kni_schwa$kaldi_class[is.na(kni_schwa$kaldi_class)] = "NA"
kni_schwa$kaldi_class = as.factor(kni_schwa$kaldi_class)
kni_schwa$kaldi_n_class = as.character(kni_schwa$kaldi_n_class)
kni_schwa$kaldi_n_class[is.na(kni_schwa$kaldi_n_class)] = "NA"
kni_schwa$kaldi_n_class = as.factor(kni_schwa$kaldi_n_class)

confusion_i_k = table(kni_schwa$ifa_class, kni_schwa$kaldi_class)
confusion_i_n = table(kni_schwa$ifa_class, kni_schwa$kaldi_n_class)
confusion_i_k
confusion_i_n


##### do kni again but for all final -n words to assess whether pN = 0.05 is not too low

kni = read.csv(paste(f_path, "validation_data_ifa-kaldi-kaldi-n_small2_pos_dur.csv", sep = ""))

# get phonetic classes
kni$ifa_class = as.factor(sapply(kni$ifa_labels, get_phon_class))
kni$kaldi_class = as.factor(sapply(kni$kaldi_labels, get_phon_class))
kni$kaldi_n_class = as.factor(sapply(kni$kaldi_n_labels, get_phon_class))

# get word number per sentence
kni$phon_id = 1:nrow(kni)
kni$phon_num = unlist(sapply(unique(as.character(kni$sentence)), function(x) 1:nrow(kni[kni$sentence == x,])))
kni$word_num = sapply(kni$phon_id, get_word_i2)
kni[kni$kaldi_pos == "SIL",]$word_num = NA

# get actual words
ort = read.csv(paste(f_path, "IFA_sentences_ort.csv", sep = ""))
kni2 = merge(kni, ort)
kni2$sen_num = sapply(kni2$sentence, match, unique(as.character(kni2$sentence)))

kni2$word_l = as.vector(sapply(as.character(kni2$ort), strsplit, " "))
kni2$phon_id = 1:nrow(kni2)
kni2$word = sapply(kni2$phon_id, get_word2)
kni2 = kni2[, names(kni2) != "word_l"]
kni_schwa = kni2[substr(kni2$word, nchar(kni2$word), nchar(kni2$word)) == "n",]
kni_schwa = kni_schwa[kni_schwa$kaldi_pos == "E",]
# get rid of endings that have no schwa or n
#kni_schwa = kni_schwa[(kni_schwa$kaldi_n_labels %in% c("@", "n")) | is.na(kni_schwa$kaldi_n_labels),]
kni_schwa = kni_schwa[rowSums(is.na(kni_schwa))<length(kni_schwa),]

# turn NA into 'NA'
kni_schwa$ifa_labels = as.character(kni_schwa$ifa_labels)
kni_schwa$ifa_labels[is.na(kni_schwa$ifa_labels)] = "NA"
kni_schwa$ifa_labels = as.factor(kni_schwa$ifa_labels)
kni_schwa$kaldi_labels = as.character(kni_schwa$kaldi_labels)
kni_schwa$kaldi_labels[is.na(kni_schwa$kaldi_labels)] = "NA"
kni_schwa$kaldi_labels = as.factor(kni_schwa$kaldi_labels)
kni_schwa$kaldi_n_labels = as.character(kni_schwa$kaldi_n_labels)
kni_schwa$kaldi_n_labels[is.na(kni_schwa$kaldi_n_labels)] = "NA"
kni_schwa$kaldi_n_labels = as.factor(kni_schwa$kaldi_n_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(kni_schwa[, c("ifa_labels", "kaldi_n_labels")], kni_schwa[, c("ifa_labels", "kaldi_labels")])

# we can make a confusion matrix
# turn NA into 'NA'
kni_schwa$ifa_class = as.character(kni_schwa$ifa_class)
kni_schwa$ifa_class[is.na(kni_schwa$ifa_class)] = "NA"
kni_schwa$ifa_class = as.factor(kni_schwa$ifa_class)
kni_schwa$kaldi_class = as.character(kni_schwa$kaldi_class)
kni_schwa$kaldi_class[is.na(kni_schwa$kaldi_class)] = "NA"
kni_schwa$kaldi_class = as.factor(kni_schwa$kaldi_class)
kni_schwa$kaldi_n_class = as.character(kni_schwa$kaldi_n_class)
kni_schwa$kaldi_n_class[is.na(kni_schwa$kaldi_n_class)] = "NA"
kni_schwa$kaldi_n_class = as.factor(kni_schwa$kaldi_n_class)

confusion_i_k = table(kni_schwa$ifa_class, kni_schwa$kaldi_class)
confusion_i_n = table(kni_schwa$ifa_class, kni_schwa$kaldi_n_class)
confusion_i_k
confusion_i_n


### Get CGN data

knc = read.csv(paste(cgn_path, "validation_data_cgn-kaldi-kaldi-n_k_pos.csv", sep = ""))

#knc = knc[1:1000,]

# get phonetic classes
knc$cgn_class = as.factor(sapply(knc$cgn_labels, get_phon_class))
knc$kaldi_class = as.factor(sapply(knc$kaldi_labels, get_phon_class))
knc$kaldi_n_class = as.factor(sapply(knc$kaldi_n_labels, get_phon_class))

# get word number per sentence
knc$phon_id = 1:nrow(knc)
knc$phon_num = unlist(sapply(unique(as.character(knc$sentence)), function(x) 1:nrow(knc[knc$sentence == x,])))
knc$word_num = sapply(knc$phon_id, get_word_i3)
knc[knc$kaldi_pos == "SIL",]$word_num = NA

# get actual words
ort = read.csv(paste(cgn_path, "k_core_sentences.csv", sep = ""))
knc2 = merge(knc, ort)
knc2$sen_num = sapply(knc2$sentence, match, unique(as.character(knc2$sentence)))

knc2$word_l = as.vector(sapply(as.character(knc2$ort), strsplit, " "))
knc2$phon_id = 1:nrow(knc2)
knc2$word = sapply(knc2$phon_id, get_word3)
knc2 = knc2[, names(knc2) != "word_l"]
knc_schwa = knc2[substr(knc2$word, nchar(knc2$word), nchar(knc2$word)) == "n",]
knc_schwa = knc_schwa[knc_schwa$kaldi_pos == "E",]

knc_schwa = knc_schwa[rowSums(is.na(knc_schwa))<length(knc_schwa),]

# turn NA into 'NA'
knc_schwa$cgn_labels = as.character(knc_schwa$cgn_labels)
knc_schwa$cgn_labels[is.na(knc_schwa$cgn_labels)] = "NA"
knc_schwa$cgn_labels = as.factor(knc_schwa$cgn_labels)
knc_schwa$kaldi_labels = as.character(knc_schwa$kaldi_labels)
knc_schwa$kaldi_labels[is.na(knc_schwa$kaldi_labels)] = "NA"
knc_schwa$kaldi_labels = as.factor(knc_schwa$kaldi_labels)
knc_schwa$kaldi_n_labels = as.character(knc_schwa$kaldi_n_labels)
knc_schwa$kaldi_n_labels[is.na(knc_schwa$kaldi_n_labels)] = "NA"
knc_schwa$kaldi_n_labels = as.factor(knc_schwa$kaldi_n_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(knc_schwa[, c("cgn_labels", "kaldi_n_labels")], knc_schwa[, c("cgn_labels", "kaldi_labels")])

# we can make a confusion matrix
# turn NA into 'NA'
knc_schwa$cgn_class = as.character(knc_schwa$cgn_class)
knc_schwa$cgn_class[is.na(knc_schwa$cgn_class)] = "NA"
knc_schwa$cgn_class = as.factor(knc_schwa$cgn_class)
knc_schwa$kaldi_class = as.character(knc_schwa$kaldi_class)
knc_schwa$kaldi_class[is.na(knc_schwa$kaldi_class)] = "NA"
knc_schwa$kaldi_class = as.factor(knc_schwa$kaldi_class)
knc_schwa$kaldi_n_class = as.character(knc_schwa$kaldi_n_class)
knc_schwa$kaldi_n_class[is.na(knc_schwa$kaldi_n_class)] = "NA"
knc_schwa$kaldi_n_class = as.factor(knc_schwa$kaldi_n_class)

confusion_c_k = table(knc_schwa$cgn_class, knc_schwa$kaldi_class)
confusion_c_n = table(knc_schwa$cgn_class, knc_schwa$kaldi_n_class)
confusion_c_k
confusion_c_n


#### comp-o

knc = read.csv(paste(cgn_path, "validation_data_cgn-kaldi-kaldi-n_o_pos.csv", sep = ""))

#knc = knc[1:1000,]

# get phonetic classes
knc$cgn_class = as.factor(sapply(knc$cgn_labels, get_phon_class))
knc$kaldi_class = as.factor(sapply(knc$kaldi_labels, get_phon_class))
knc$kaldi_n_class = as.factor(sapply(knc$kaldi_n_labels, get_phon_class))

# get word number per sentence
knc$phon_id = 1:nrow(knc)
knc$phon_num = unlist(sapply(unique(as.character(knc$sentence)), function(x) 1:nrow(knc[knc$sentence == x,])))
knc$word_num = sapply(knc$phon_id, get_word_i3)
knc[knc$kaldi_pos == "SIL",]$word_num = NA

# get actual words
ort = read.csv(paste(cgn_path, "o_core_sentences.csv", sep = ""))
knc2 = merge(knc, ort)
knc2$sen_num = sapply(knc2$sentence, match, unique(as.character(knc2$sentence)))

knc2$word_l = as.vector(sapply(as.character(knc2$ort), strsplit, " "))
knc2$phon_id = 1:nrow(knc2)
knc2$word = sapply(knc2$phon_id, get_word3)
knc2 = knc2[, names(knc2) != "word_l"]
knc_schwa = knc2[substr(knc2$word, nchar(knc2$word), nchar(knc2$word)) == "n",]
knc_schwa = knc_schwa[knc_schwa$kaldi_pos == "E",]

knc_schwa = knc_schwa[rowSums(is.na(knc_schwa))<length(knc_schwa),]

# turn NA into 'NA'
knc_schwa$cgn_labels = as.character(knc_schwa$cgn_labels)
knc_schwa$cgn_labels[is.na(knc_schwa$cgn_labels)] = "NA"
knc_schwa$cgn_labels = as.factor(knc_schwa$cgn_labels)
knc_schwa$kaldi_labels = as.character(knc_schwa$kaldi_labels)
knc_schwa$kaldi_labels[is.na(knc_schwa$kaldi_labels)] = "NA"
knc_schwa$kaldi_labels = as.factor(knc_schwa$kaldi_labels)
knc_schwa$kaldi_n_labels = as.character(knc_schwa$kaldi_n_labels)
knc_schwa$kaldi_n_labels[is.na(knc_schwa$kaldi_n_labels)] = "NA"
knc_schwa$kaldi_n_labels = as.factor(knc_schwa$kaldi_n_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(knc_schwa[, c("cgn_labels", "kaldi_n_labels")], knc_schwa[, c("cgn_labels", "kaldi_labels")])

# we can make a confusion matrix
# turn NA into 'NA'
knc_schwa$cgn_class = as.character(knc_schwa$cgn_class)
knc_schwa$cgn_class[is.na(knc_schwa$cgn_class)] = "NA"
knc_schwa$cgn_class = as.factor(knc_schwa$cgn_class)
knc_schwa$kaldi_class = as.character(knc_schwa$kaldi_class)
knc_schwa$kaldi_class[is.na(knc_schwa$kaldi_class)] = "NA"
knc_schwa$kaldi_class = as.factor(knc_schwa$kaldi_class)
knc_schwa$kaldi_n_class = as.character(knc_schwa$kaldi_n_class)
knc_schwa$kaldi_n_class[is.na(knc_schwa$kaldi_n_class)] = "NA"
knc_schwa$kaldi_n_class = as.factor(knc_schwa$kaldi_n_class)

confusion_c_k = table(knc_schwa$cgn_class, knc_schwa$kaldi_class)
confusion_c_n = table(knc_schwa$cgn_class, knc_schwa$kaldi_n_class)
confusion_c_k
confusion_c_n

#### comp-a

knc = read.csv(paste(cgn_path, "validation_data_cgn-kaldi-kaldi-n_a_pos.csv", sep = ""))

#knc = knc[1:1000,]

# get phonetic classes
knc$cgn_class = as.factor(sapply(knc$cgn_labels, get_phon_class))
knc$kaldi_class = as.factor(sapply(knc$kaldi_labels, get_phon_class))
knc$kaldi_n_class = as.factor(sapply(knc$kaldi_n_labels, get_phon_class))

# get word number per sentence
knc$phon_id = 1:nrow(knc)
knc$phon_num = unlist(sapply(unique(as.character(knc$sentence)), function(x) 1:nrow(knc[knc$sentence == x,])))
knc$word_num = sapply(knc$phon_id, get_word_i3)
knc[knc$kaldi_pos == "SIL",]$word_num = NA

# get actual words
ort = read.csv(paste(cgn_path, "a_core_sentences.csv", sep = ""))
knc2 = merge(knc, ort)
knc2$sen_num = sapply(knc2$sentence, match, unique(as.character(knc2$sentence)))

knc2$word_l = as.vector(sapply(as.character(knc2$ort), strsplit, " "))
knc2$phon_id = 1:nrow(knc2)
knc2$word = sapply(knc2$phon_id, get_word3)
knc2 = knc2[, names(knc2) != "word_l"]
knc_schwa = knc2[substr(knc2$word, nchar(knc2$word), nchar(knc2$word)) == "n",]
knc_schwa = knc_schwa[knc_schwa$kaldi_pos == "E",]

knc_schwa = knc_schwa[rowSums(is.na(knc_schwa))<length(knc_schwa),]

# turn NA into 'NA'
knc_schwa$cgn_labels = as.character(knc_schwa$cgn_labels)
knc_schwa$cgn_labels[is.na(knc_schwa$cgn_labels)] = "NA"
knc_schwa$cgn_labels = as.factor(knc_schwa$cgn_labels)
knc_schwa$kaldi_labels = as.character(knc_schwa$kaldi_labels)
knc_schwa$kaldi_labels[is.na(knc_schwa$kaldi_labels)] = "NA"
knc_schwa$kaldi_labels = as.factor(knc_schwa$kaldi_labels)
knc_schwa$kaldi_n_labels = as.character(knc_schwa$kaldi_n_labels)
knc_schwa$kaldi_n_labels[is.na(knc_schwa$kaldi_n_labels)] = "NA"
knc_schwa$kaldi_n_labels = as.factor(knc_schwa$kaldi_n_labels)

# let's get inter-annotator agreements and test the difference (including insertions)
ttest.fleiss(knc_schwa[, c("cgn_labels", "kaldi_n_labels")], knc_schwa[, c("cgn_labels", "kaldi_labels")])

# we can make a confusion matrix
# turn NA into 'NA'
knc_schwa$cgn_class = as.character(knc_schwa$cgn_class)
knc_schwa$cgn_class[is.na(knc_schwa$cgn_class)] = "NA"
knc_schwa$cgn_class = as.factor(knc_schwa$cgn_class)
knc_schwa$kaldi_class = as.character(knc_schwa$kaldi_class)
knc_schwa$kaldi_class[is.na(knc_schwa$kaldi_class)] = "NA"
knc_schwa$kaldi_class = as.factor(knc_schwa$kaldi_class)
knc_schwa$kaldi_n_class = as.character(knc_schwa$kaldi_n_class)
knc_schwa$kaldi_n_class[is.na(knc_schwa$kaldi_n_class)] = "NA"
knc_schwa$kaldi_n_class = as.factor(knc_schwa$kaldi_n_class)

confusion_c_k = table(knc_schwa$cgn_class, knc_schwa$kaldi_class)
confusion_c_n = table(knc_schwa$cgn_class, knc_schwa$kaldi_n_class)
confusion_c_k
confusion_c_n


