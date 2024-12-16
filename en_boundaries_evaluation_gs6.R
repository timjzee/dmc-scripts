if (Sys.info()[1] == "Darwin"){
  f_path = "/Volumes/tensusers/timzee/classifier_evaluation/en/"
} else {
  f_path = "/vol/tensusers/timzee/classifier_evaluation/en/"
}

# indices for val/test split

vali_indices <- 1:71
test_indices <- 72:142

par_list = list(
  schwa_network_type = c("FFNN", "BLSTM"),
  schwa_context_frames = c("5", "15"),
  n_network_type = c("FFNN", "BLSTM"),
  n_context_frames = c("5", "15"),
  N_network_type = c("FFNN", "BLSTM"),
  N_context_frames = c("5", "15"),
  kal_diff_weight = c("0.5", "1"), # c("0", "0.5", "1")
  apply_penalty = c("1"), # c("0", "1")
  n_prec_values = c("0", "10"),
  n_subs_values = c("0", "10"),
  n_smooths = c("1", "2", "3"), # c("1", "2", "3", "4")
  take_sqrt = c("0"),
  threshold_schwa = c("0.2", "0.5", "0.8"), # c("0.2", "0.8")
  threshold_n = c("0.2", "0.5", "0.8"),     # c("0.2", "0.8")
  threshold_N = c("0.2", "0.5", "0.8"),     # c("0.2", "0.8")
  subtract_n = c("0", "1")
)

gs_df <- expand.grid(par_list)

num_par <- length(par_list)
num_gs <- nrow(gs_df)
gs_df$gs_name <- rep("", num_gs)
gs_df$v_schwa_perc20_TZ_MW_startb <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_NN_startb <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_MW_NN_startb <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_MW_starte <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_NN_starte <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_MW_NN_starte <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_MW_endb <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_NN_endb <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_MW_NN_endb <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_MW_ende <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_NN_ende <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_MW_NN_ende <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$v_schwa_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_MW_startb <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_NN_startb <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_MW_NN_startb <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_MW_starte <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_NN_starte <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_MW_NN_starte <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_MW_endb <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_NN_endb <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_MW_NN_endb <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_MW_ende <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_NN_ende <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_MW_NN_ende <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$v_nasal_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_TZ_MW_b <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_TZ_NN_b <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_MW_NN_b <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_TZ_MW_e <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_TZ_NN_e <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_MW_NN_e <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$v_nasalization_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$v_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$v_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$v_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$v_variant_accuracy_TZ_MW <- rep(0.1, num_gs)
gs_df$v_variant_macro_Fscore_TZ_MW <- rep(0.1, num_gs)
gs_df$v_variant_accuracy_TZ_NN <- rep(0.1, num_gs)
gs_df$v_variant_macro_Fscore_TZ_NN <- rep(0.1, num_gs)
gs_df$v_variant_accuracy_MW_NN <- rep(0.1, num_gs)
gs_df$v_variant_macro_Fscore_MW_NN <- rep(0.1, num_gs)

n_col_vali <- ncol(gs_df)

gs_df$t_schwa_perc20_TZ_MW_startb <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_NN_startb <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_MW_NN_startb <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_MW_starte <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_NN_starte <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_MW_NN_starte <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_MW_endb <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_NN_endb <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_MW_NN_endb <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_MW_ende <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_NN_ende <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_MW_NN_ende <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$t_schwa_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_MW_startb <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_NN_startb <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_MW_NN_startb <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_MW_starte <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_NN_starte <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_MW_NN_starte <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_MW_endb <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_NN_endb <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_MW_NN_endb <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_MW_ende <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_NN_ende <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_MW_NN_ende <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$t_nasal_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_TZ_MW_b <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_TZ_NN_b <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_MW_NN_b <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_TZ_MW_e <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_TZ_NN_e <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_MW_NN_e <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$t_nasalization_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$t_perc20_TZ_MW <- rep(0.1, num_gs)
gs_df$t_perc20_TZ_NN <- rep(0.1, num_gs)
gs_df$t_perc20_MW_NN <- rep(0.1, num_gs)
gs_df$t_variant_accuracy_TZ_MW <- rep(0.1, num_gs)
gs_df$t_variant_macro_Fscore_TZ_MW <- rep(0.1, num_gs)
gs_df$t_variant_accuracy_TZ_NN <- rep(0.1, num_gs)
gs_df$t_variant_macro_Fscore_TZ_NN <- rep(0.1, num_gs)
gs_df$t_variant_accuracy_MW_NN <- rep(0.1, num_gs)
gs_df$t_variant_macro_Fscore_MW_NN <- rep(0.1, num_gs)

n_col_test <- ncol(gs_df)

# gs_df <- head(gs_df, n=50)
gs_df <- as.data.frame(lapply(gs_df, as.character))

getGsData <- function(gs_index){
  print(gs_index)
  name_list = c()
  for (i in 1:num_par) name_list <- c(name_list, as.character(gs_df[gs_index,i]))
  gs_name <- paste(name_list, collapse = "_")
#  gs_name <- paste(gs_df[gs_index,1:num_par], collapse = "_")
#  print(gs_df[gs_index,1:num_par])
  gs_name <- sub("^", "e-", gs_name, perl = T)
  gs_name <- sub("(?<=5)_(?=(FFNN|BLSTM))", "_n-", gs_name, perl = T)
  gs_name <- sub("(?<=5)_(?=(FFNN|BLSTM))", "_N-", gs_name, perl = T)
  gs_name <- gsub("(?<=(TM|NN))_(?=(1|5))", "-", gs_name, perl = T)
#  print(gs_name)
  bounds <- read.csv(paste(f_path, "grid_search_output3/", gs_name, ".csv", sep = ""))
  bounds <- bounds[indices,]
  ### schwa
  ## Human-Human
  bounds$TZ_MW_e_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_b, bounds$MW_e_start_b)
  schwa_perc20_TZ_MW_startb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_start_b) < 0.02,c("TZ_MW_e_start_b")])) / NROW(bounds[,c("TZ_MW_e_start_b")])
  bounds$TZ_MW_e_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_e, bounds$MW_e_start_e)
  schwa_perc20_TZ_MW_starte <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_start_e) < 0.02,c("TZ_MW_e_start_e")])) / NROW(bounds[,c("TZ_MW_e_start_e")])
  bounds$TZ_MW_e_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_b, bounds$MW_e_end_b)
  schwa_perc20_TZ_MW_endb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_end_b) < 0.02,c("TZ_MW_e_end_b")])) / NROW(bounds[,c("TZ_MW_e_end_b")])
  bounds$TZ_MW_e_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_e, bounds$MW_e_end_e)
  schwa_perc20_TZ_MW_ende <- NROW(na.omit(bounds[abs(bounds$TZ_MW_e_end_e) < 0.02,c("TZ_MW_e_end_e")])) / NROW(bounds[,c("TZ_MW_e_end_e")])
  bounds_TZ_MW_e_long <- c(bounds$TZ_MW_e_start_b, bounds$TZ_MW_e_start_e, bounds$TZ_MW_e_end_b, bounds$TZ_MW_e_end_e)
  schwa_perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_e_long[abs(bounds_TZ_MW_e_long) < 0.02])) / NROW(bounds_TZ_MW_e_long)
  ## Human-Computer
  # check for incomplete and early schwas
  bounds$contains_schwa <- !is.na(bounds$nn_e_start_b + bounds$nn_e_start_e + bounds$nn_e_end_b + bounds$nn_e_end_e)
  bounds$frag_dur <- bounds$kal_end - bounds$kal_start + .5
  bounds$end_first_quart <- bounds$kal_start - .3 + .33*bounds$frag_dur
  bounds$early_schwa <- bounds$nn_e_start_b < bounds$end_first_quart
  if (length(bounds[bounds$early_schwa & !is.na(bounds$early_schwa),]$contains_schwa) > 0){
    bounds[bounds$early_schwa & !is.na(bounds$early_schwa),]$contains_schwa <- FALSE
  }
  # remove boundaries of incomplete schwas and early schwas
  if(sum(!bounds$contains_schwa) > 0){
    bounds[!bounds$contains_schwa,]$nn_e_start_b <- NA
    bounds[!bounds$contains_schwa,]$nn_e_start_e <- NA
    bounds[!bounds$contains_schwa,]$nn_e_end_b <- NA
    bounds[!bounds$contains_schwa,]$nn_e_end_e <- NA
  }
  # compute differences and percentages < 20ms
  bounds$TZ_nn_e_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_b, bounds$nn_e_start_b)
  schwa_perc20_TZ_NN_startb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_start_b) < 0.02,c("TZ_nn_e_start_b")])) / NROW(bounds[,c("TZ_nn_e_start_b")])
  bounds$TZ_nn_e_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_start_e, bounds$nn_e_start_e)
  schwa_perc20_TZ_NN_starte <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_start_e) < 0.02,c("TZ_nn_e_start_e")])) / NROW(bounds[,c("TZ_nn_e_start_e")])
  bounds$TZ_nn_e_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_b, bounds$nn_e_end_b)
  schwa_perc20_TZ_NN_endb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_end_b) < 0.02,c("TZ_nn_e_end_b")])) / NROW(bounds[,c("TZ_nn_e_end_b")])
  bounds$TZ_nn_e_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_e_end_e, bounds$nn_e_end_e)
  schwa_perc20_TZ_NN_ende <- NROW(na.omit(bounds[abs(bounds$TZ_nn_e_end_e) < 0.02,c("TZ_nn_e_end_e")])) / NROW(bounds[,c("TZ_nn_e_end_e")])
  bounds_TZ_NN_e_long <- c(bounds$TZ_nn_e_start_b, bounds$TZ_nn_e_start_e, bounds$TZ_nn_e_end_b, bounds$TZ_nn_e_end_e)
  schwa_perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_e_long[abs(bounds_TZ_NN_e_long) < 0.02])) / NROW(bounds_TZ_NN_e_long)
  #
  bounds$MW_nn_e_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_start_b, bounds$nn_e_start_b)
  schwa_perc20_MW_NN_startb <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_start_b) < 0.02,c("MW_nn_e_start_b")])) / NROW(bounds[,c("MW_nn_e_start_b")])
  bounds$MW_nn_e_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_start_e, bounds$nn_e_start_e)
  schwa_perc20_MW_NN_starte <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_start_e) < 0.02,c("MW_nn_e_start_e")])) / NROW(bounds[,c("MW_nn_e_start_e")])
  bounds$MW_nn_e_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_end_b, bounds$nn_e_end_b)
  schwa_perc20_MW_NN_endb <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_end_b) < 0.02,c("MW_nn_e_end_b")])) / NROW(bounds[,c("MW_nn_e_end_b")])
  bounds$MW_nn_e_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_e_end_e, bounds$nn_e_end_e)
  schwa_perc20_MW_NN_ende <- NROW(na.omit(bounds[abs(bounds$MW_nn_e_end_e) < 0.02,c("MW_nn_e_end_e")])) / NROW(bounds[,c("MW_nn_e_end_e")])
  bounds_MW_NN_e_long <- c(bounds$MW_nn_e_start_b, bounds$MW_nn_e_start_e, bounds$MW_nn_e_end_b, bounds$MW_nn_e_end_e)
  schwa_perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_e_long[abs(bounds_MW_NN_e_long) < 0.02])) / NROW(bounds_MW_NN_e_long)
  ### nasal
  ## Human-Human
  bounds$TZ_MW_n_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_b, bounds$MW_n_start_b)
  nasal_perc20_TZ_MW_startb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_start_b) < 0.02,c("TZ_MW_n_start_b")])) / NROW(bounds[,c("TZ_MW_n_start_b")])
  bounds$TZ_MW_n_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_e, bounds$MW_n_start_e)
  nasal_perc20_TZ_MW_starte <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_start_e) < 0.02,c("TZ_MW_n_start_e")])) / NROW(bounds[,c("TZ_MW_n_start_e")])
  bounds$TZ_MW_n_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_b, bounds$MW_n_end_b)
  nasal_perc20_TZ_MW_endb <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_end_b) < 0.02,c("TZ_MW_n_end_b")])) / NROW(bounds[,c("TZ_MW_n_end_b")])
  bounds$TZ_MW_n_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_e, bounds$MW_n_end_e)
  nasal_perc20_TZ_MW_ende <- NROW(na.omit(bounds[abs(bounds$TZ_MW_n_end_e) < 0.02,c("TZ_MW_n_end_e")])) / NROW(bounds[,c("TZ_MW_n_end_e")])
  bounds_TZ_MW_n_long <- c(bounds$TZ_MW_n_start_b, bounds$TZ_MW_n_start_e, bounds$TZ_MW_n_end_b, bounds$TZ_MW_n_end_e)
  nasal_perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_n_long[abs(bounds_TZ_MW_n_long) < 0.02])) / NROW(bounds_TZ_MW_n_long)
  ## Human-Computer
  # check for incomplete and early nasals
  bounds$contains_nasal <- !is.na(bounds$nn_n_start_b + bounds$nn_n_start_e + bounds$nn_n_end_b + bounds$nn_n_end_e)
  bounds$early_nasal <- bounds$nn_n_start_b < bounds$end_first_quart
  if (length(bounds[bounds$early_nasal & !is.na(bounds$early_nasal),]$contains_nasal) > 0){
    bounds[bounds$early_nasal & !is.na(bounds$early_nasal),]$contains_nasal <- FALSE
  }
  # remove boundaries of incomplete nasals and early nasals
  if(sum(!bounds$contains_nasal) > 0){
    bounds[!bounds$contains_nasal,]$nn_n_start_b <- NA
    bounds[!bounds$contains_nasal,]$nn_n_start_e <- NA
    bounds[!bounds$contains_nasal,]$nn_n_end_b <- NA
    bounds[!bounds$contains_nasal,]$nn_n_end_e <- NA
  }
  # compute differences and percentages < 20ms
  bounds$TZ_nn_n_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_b, bounds$nn_n_start_b)
  nasal_perc20_TZ_NN_startb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_start_b) < 0.02,c("TZ_nn_n_start_b")])) / NROW(bounds[,c("TZ_nn_n_start_b")])
  bounds$TZ_nn_n_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_start_e, bounds$nn_n_start_e)
  nasal_perc20_TZ_NN_starte <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_start_e) < 0.02,c("TZ_nn_n_start_e")])) / NROW(bounds[,c("TZ_nn_n_start_e")])
  bounds$TZ_nn_n_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_b, bounds$nn_n_end_b)
  nasal_perc20_TZ_NN_endb <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_end_b) < 0.02,c("TZ_nn_n_end_b")])) / NROW(bounds[,c("TZ_nn_n_end_b")])
  bounds$TZ_nn_n_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_n_end_e, bounds$nn_n_end_e)
  nasal_perc20_TZ_NN_ende <- NROW(na.omit(bounds[abs(bounds$TZ_nn_n_end_e) < 0.02,c("TZ_nn_n_end_e")])) / NROW(bounds[,c("TZ_nn_n_end_e")])
  bounds_TZ_NN_n_long <- c(bounds$TZ_nn_n_start_b, bounds$TZ_nn_n_start_e, bounds$TZ_nn_n_end_b, bounds$TZ_nn_n_end_e)
  nasal_perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_n_long[abs(bounds_TZ_NN_n_long) < 0.02])) / NROW(bounds_TZ_NN_n_long)
  #
  bounds$MW_nn_n_start_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_start_b, bounds$nn_n_start_b)
  nasal_perc20_MW_NN_startb <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_start_b) < 0.02,c("MW_nn_n_start_b")])) / NROW(bounds[,c("MW_nn_n_start_b")])
  bounds$MW_nn_n_start_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_start_e, bounds$nn_n_start_e)
  nasal_perc20_MW_NN_starte <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_start_e) < 0.02,c("MW_nn_n_start_e")])) / NROW(bounds[,c("MW_nn_n_start_e")])
  bounds$MW_nn_n_end_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_end_b, bounds$nn_n_end_b)
  nasal_perc20_MW_NN_endb <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_end_b) < 0.02,c("MW_nn_n_end_b")])) / NROW(bounds[,c("MW_nn_n_end_b")])
  bounds$MW_nn_n_end_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_n_end_e, bounds$nn_n_end_e)
  nasal_perc20_MW_NN_ende <- NROW(na.omit(bounds[abs(bounds$MW_nn_n_end_e) < 0.02,c("MW_nn_n_end_e")])) / NROW(bounds[,c("MW_nn_n_end_e")])
  bounds_MW_NN_n_long <- c(bounds$MW_nn_n_start_b, bounds$MW_nn_n_start_e, bounds$MW_nn_n_end_b, bounds$MW_nn_n_end_e)
  nasal_perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_n_long[abs(bounds_MW_NN_n_long) < 0.02])) / NROW(bounds_MW_NN_n_long)
  ### nasalization
  ## only for humans, classifier is handled in praat script (or schwa end if no schwa present)
  bounds$TZ_contains_schwa <- !is.na(bounds$TZ_e_start_b + bounds$TZ_e_start_e + bounds$TZ_e_end_b + bounds$TZ_e_end_e)
  bounds$TZ_contains_nasal <- !is.na(bounds$TZ_n_start_b + bounds$TZ_n_start_e + bounds$TZ_n_end_b + bounds$TZ_n_end_e)
  bounds$TZ_contains_nasalization <- !is.na(bounds$TZ_N_b + bounds$TZ_N_e)
  bounds$MW_contains_schwa <- !is.na(bounds$MW_e_start_b + bounds$MW_e_start_e + bounds$MW_e_end_b + bounds$MW_e_end_e)
  bounds$MW_contains_nasal <- !is.na(bounds$MW_n_start_b + bounds$MW_n_start_e + bounds$MW_n_end_b + bounds$MW_n_end_e)
  bounds$MW_contains_nasalization <- !is.na(bounds$MW_N_b + bounds$MW_N_e)
  ## Change N start boundary to schwa start if N start < schwa start (or nasal start if no schwa present)
  bounds$TZ_N_b <- ifelse(bounds$TZ_contains_schwa, 
                          ifelse(bounds$TZ_N_b < bounds$TZ_e_start_b,
                                 bounds$TZ_e_start_b, 
                                 bounds$TZ_N_b
                          ),
                          ifelse(bounds$TZ_contains_nasal,
                                 ifelse(bounds$TZ_N_b < bounds$TZ_n_start_b, 
                                        bounds$TZ_n_start_b, 
                                        bounds$TZ_N_b
                                 ),
                                 NA
                          ))
  bounds$MW_N_b <- ifelse(bounds$MW_contains_schwa, 
                          ifelse(bounds$MW_N_b < bounds$MW_e_start_b,
                                 bounds$MW_e_start_b, 
                                 bounds$MW_N_b
                          ),
                          ifelse(bounds$MW_contains_nasal,
                                 ifelse(bounds$MW_N_b < bounds$MW_n_start_b, 
                                        bounds$MW_n_start_b, 
                                        bounds$MW_N_b
                                 ),
                                 NA
                          ))
  ## Change N end boundary to nasal end if N end > nasal end (or schwa end if no nasal present)
  bounds$TZ_N_e <- ifelse(bounds$TZ_contains_nasal, 
                          ifelse(bounds$TZ_N_e > bounds$TZ_n_end_e,
                                 bounds$TZ_n_end_e, 
                                 bounds$TZ_N_e
                          ),
                          ifelse(bounds$TZ_contains_schwa,
                                 ifelse(bounds$TZ_N_e > bounds$TZ_e_end_e, 
                                        bounds$TZ_e_end_e, 
                                        bounds$TZ_N_e
                                 ),
                                 NA
                          ))
  bounds$MW_N_e <- ifelse(bounds$MW_contains_nasal, 
                          ifelse(bounds$MW_N_e > bounds$MW_n_end_e,
                                 bounds$MW_n_end_e, 
                                 bounds$MW_N_e
                          ),
                          ifelse(bounds$MW_contains_schwa,
                                 ifelse(bounds$MW_N_e > bounds$MW_e_end_e, 
                                        bounds$MW_e_end_e, 
                                        bounds$MW_N_e
                                 ),
                                 NA
                          ))
  ## Human-Human
  bounds$TZ_MW_N_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_b, bounds$MW_N_b)
  nasalization_perc20_TZ_MW_b <- NROW(na.omit(bounds[abs(bounds$TZ_MW_N_b) < 0.02,c("TZ_MW_N_b")])) / NROW(bounds[,c("TZ_MW_N_b")])
  bounds$TZ_MW_N_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_e, bounds$MW_N_e)
  nasalization_perc20_TZ_MW_e <- NROW(na.omit(bounds[abs(bounds$TZ_MW_N_e) < 0.02,c("TZ_MW_N_e")])) / NROW(bounds[,c("TZ_MW_N_e")])
  bounds_TZ_MW_N_long <- c(bounds$TZ_MW_N_b, bounds$TZ_MW_N_e)
  nasalization_perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_N_long[abs(bounds_TZ_MW_N_long) < 0.02])) / NROW(bounds_TZ_MW_N_long)
  ## Human-Computer
  # perhaps change N boundaries to n boundaries if the end n boundary is later or the start n boundary is earlier
  # but see notes 7oct.txt
  # for now just remove incomplete nasalizations
  bounds$contains_nasalization <- !is.na(bounds$nn_N_start_b + bounds$nn_N_start_e + bounds$nn_N_end_b + bounds$nn_N_end_e)
  # remove boundaries of incomplete nasalizations
  if(sum(!bounds$contains_nasalization) > 0){
    bounds[!bounds$contains_nasalization,]$nn_N_start_b <- NA
    bounds[!bounds$contains_nasalization,]$nn_N_start_e <- NA
    bounds[!bounds$contains_nasalization,]$nn_N_end_b <- NA
    bounds[!bounds$contains_nasalization,]$nn_N_end_e <- NA
  }
  # compute differences and percentages < 20ms
  bounds$TZ_nn_N_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_b, bounds$nn_N_start_b)
  nasalization_perc20_TZ_NN_b <- NROW(na.omit(bounds[abs(bounds$TZ_nn_N_b) < 0.02,c("TZ_nn_N_b")])) / NROW(bounds[,c("TZ_nn_N_b")])
  bounds$TZ_nn_N_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$TZ_N_e, bounds$nn_N_end_e)
  nasalization_perc20_TZ_NN_e <- NROW(na.omit(bounds[abs(bounds$TZ_nn_N_e) < 0.02,c("TZ_nn_N_e")])) / NROW(bounds[,c("TZ_nn_N_e")])
  bounds_TZ_NN_N_long <- c(bounds$TZ_nn_N_b, bounds$TZ_nn_N_e)
  nasalization_perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_N_long[abs(bounds_TZ_NN_N_long) < 0.02])) / NROW(bounds_TZ_NN_N_long)
  #
  bounds$MW_nn_N_b <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_N_b, bounds$nn_N_start_b)
  nasalization_perc20_MW_NN_b <- NROW(na.omit(bounds[abs(bounds$MW_nn_N_b) < 0.02,c("MW_nn_N_b")])) / NROW(bounds[,c("MW_nn_N_b")])
  bounds$MW_nn_N_e <- mapply(function(x, y) ifelse(is.na(x) && is.na(y), 0, x - y), bounds$MW_N_e, bounds$nn_N_end_e)
  nasalization_perc20_MW_NN_e <- NROW(na.omit(bounds[abs(bounds$MW_nn_N_e) < 0.02,c("MW_nn_N_e")])) / NROW(bounds[,c("MW_nn_N_e")])
  bounds_MW_NN_N_long <- c(bounds$MW_nn_N_b, bounds$MW_nn_N_e)
  nasalization_perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_N_long[abs(bounds_MW_NN_N_long) < 0.02])) / NROW(bounds_MW_NN_N_long)
  
  bounds_TZ_MW_long <- c(bounds_TZ_MW_e_long, bounds_TZ_MW_n_long, bounds_TZ_MW_N_long)
  perc20_TZ_MW <- NROW(na.omit(bounds_TZ_MW_long[abs(bounds_TZ_MW_long) < 0.02])) / NROW(bounds_TZ_MW_long)
  bounds_TZ_NN_long <- c(bounds_TZ_NN_e_long, bounds_TZ_NN_n_long, bounds_TZ_NN_N_long)
  perc20_TZ_NN <- NROW(na.omit(bounds_TZ_NN_long[abs(bounds_TZ_NN_long) < 0.02])) / NROW(bounds_TZ_NN_long)
  bounds_MW_NN_long <- c(bounds_MW_NN_e_long, bounds_MW_NN_n_long, bounds_MW_NN_N_long)
  perc20_MW_NN <- NROW(na.omit(bounds_MW_NN_long[abs(bounds_MW_NN_long) < 0.02])) / NROW(bounds_MW_NN_long)
  
  # get F-score
  bounds$classifier_variant <- mapply(function(x,y,z) ifelse(x, ifelse(y, "@n", ifelse(z, "~", "@")), ifelse(y, "n", "0")),
                                    bounds$contains_schwa,
                                    bounds$contains_nasal,
                                    bounds$contains_nasalization)
  bounds$classifier_variant <- factor(x = bounds$classifier_variant, levels = c("@", "@n", "~", "0", "n"))
  bounds$TZ_variant <- mapply(function(x,y,z) ifelse(x, ifelse(y, "@n", ifelse(z, "~", "@")), ifelse(y, "n", "0")),
                              bounds$TZ_contains_schwa,
                              bounds$TZ_contains_nasal,
                              bounds$TZ_contains_nasalization)
  bounds$TZ_variant <- factor(x = bounds$TZ_variant, levels = c("@", "@n", "~", "0", "n"))
  #
  bounds$MW_variant <- mapply(function(x,y,z) ifelse(x, ifelse(y, "@n", ifelse(z, "~", "@")), ifelse(y, "n", "0")),
                              bounds$MW_contains_schwa,
                              bounds$MW_contains_nasal,
                              bounds$MW_contains_nasalization)
  bounds$MW_variant <- factor(x = bounds$MW_variant, levels = c("@", "@n", "~", "0", "n"))
  
  cm <- table(bounds$TZ_variant, bounds$MW_variant, 
              dnn = c("TZ", "MW"))

  recall <- diag(cm) / colSums(cm)
  macro.recall <- mean(na.omit(recall))
  precision <- diag(cm) / rowSums(cm)
  macro.precision <- mean(na.omit(precision))
  Accuracy <- sum(diag(cm)) / sum(cm)
  macro.Fscore <- 2*((macro.precision * macro.recall) / (macro.precision + macro.recall))
  
  cm2 <- table(bounds$classifier_variant, bounds$TZ_variant, 
              dnn = c("NN", "TZ"))
  
  recall2 <- diag(cm2) / colSums(cm2)
  macro.recall2 <- mean(na.omit(recall2))
  precision2 <- diag(cm2) / rowSums(cm2)
  macro.precision2 <- mean(na.omit(precision2))
  Accuracy2 <- sum(diag(cm2)) / sum(cm2)
  macro.Fscore2 <- 2*((macro.precision2 * macro.recall2) / (macro.precision2 + macro.recall2))
  
  cm3 <- table(bounds$classifier_variant, bounds$MW_variant, 
               dnn = c("NN", "MW"))
  
  recall3 <- diag(cm3) / colSums(cm3)
  macro.recall3 <- mean(na.omit(recall3))
  precision3 <- diag(cm3) / rowSums(cm3)
  macro.precision3 <- mean(na.omit(precision3))
  Accuracy3 <- sum(diag(cm3)) / sum(cm3)
  macro.Fscore3 <- 2*((macro.precision3 * macro.recall3) / (macro.precision3 + macro.recall3))
  
  if(length(indices) == 142){
    return(bounds)
  } else {
    return(c(gs_name, 
             schwa_perc20_TZ_MW_startb, 
             schwa_perc20_TZ_NN_startb, 
             schwa_perc20_MW_NN_startb,
             schwa_perc20_TZ_MW_starte, 
             schwa_perc20_TZ_NN_starte,
             schwa_perc20_MW_NN_starte,
             schwa_perc20_TZ_MW_endb, 
             schwa_perc20_TZ_NN_endb,
             schwa_perc20_MW_NN_endb,
             schwa_perc20_TZ_MW_ende, 
             schwa_perc20_TZ_NN_ende,
             schwa_perc20_MW_NN_ende,
             schwa_perc20_TZ_MW, 
             schwa_perc20_TZ_NN,
             schwa_perc20_MW_NN,
             nasal_perc20_TZ_MW_startb, 
             nasal_perc20_TZ_NN_startb, 
             nasal_perc20_MW_NN_startb,
             nasal_perc20_TZ_MW_starte, 
             nasal_perc20_TZ_NN_starte,
             nasal_perc20_MW_NN_starte,
             nasal_perc20_TZ_MW_endb, 
             nasal_perc20_TZ_NN_endb,
             nasal_perc20_MW_NN_endb,
             nasal_perc20_TZ_MW_ende, 
             nasal_perc20_TZ_NN_ende,
             nasal_perc20_MW_NN_ende,
             nasal_perc20_TZ_MW, 
             nasal_perc20_TZ_NN,
             nasal_perc20_MW_NN,
             nasalization_perc20_TZ_MW_b,
             nasalization_perc20_TZ_NN_b,
             nasalization_perc20_MW_NN_b,
             nasalization_perc20_TZ_MW_e,
             nasalization_perc20_TZ_NN_e,
             nasalization_perc20_MW_NN_e,
             nasalization_perc20_TZ_MW,
             nasalization_perc20_TZ_NN,
             nasalization_perc20_MW_NN,
             perc20_TZ_MW,
             perc20_TZ_NN,
             perc20_MW_NN,
             Accuracy,
             macro.Fscore,
             Accuracy2,
             macro.Fscore2,
             Accuracy3,
             macro.Fscore3))
  }
}

indices <- vali_indices
gs_df[,(num_par+1):n_col_vali] <- t(sapply(1:nrow(gs_df), getGsData))

indices <- test_indices
gs_df[,(n_col_vali+1):n_col_test] <- t(sapply(1:nrow(gs_df), getGsData))[,2:(n_col_test - n_col_vali+1)]


gs_df <- gs_df[order(gs_df$v_perc20_TZ_NN, decreasing = T),]

write.csv(gs_df, file = paste(f_path, "gs_summary3.csv", sep = ""),quote = F, row.names = F)

indices <- 1:142
bla <- getGsData(1)
write.table(bla, file = paste(f_path, "gs_summary3_example.csv", sep = ""),quote = F, row.names = F, sep = "\t")

# Get best parameter combo
gs_df$v_classifier_average_score_TZ <- (gs_df$v_perc20_TZ_NN + gs_df$v_variant_accuracy_TZ_NN) / 2
gs_df[order(gs_df$v_classifier_average_score_TZ, decreasing = T),]$gs_name[1]


