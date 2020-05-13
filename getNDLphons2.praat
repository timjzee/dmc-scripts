form Command line parameters
    word f_path 1
    real c_start 1
    real c_end 1
    word speaker 1
    integer word_num 1
    integer window_start 1
    integer window_end 1
    word corpus 1
    integer n_phon 2
endform

prop$ = Report system properties
os$ = extractWord$(prop$, newline$)
if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/" + corpus$
else
    tens_path$ = "/vol/tensusers/timzee/" + corpus$
endif

Read from file: tens_path$ + f_path$ + ".awd"
tier_name$ = ""
tier = 0
while tier_name$ != speaker$
    tier += 1
    tier_name$ = Get tier name: tier
endwhile

interval_i = Get high interval at time: tier, c_start
interval_i_end = Get low interval at time: tier, c_end
num_interval_i = interval_i_end - interval_i + 1
# if words are not aligned
if word_num > num_interval_i
    pre_boundary$ = ""
    post_boundary$ = ""
    boundary$ = ""
    goto SKIP
endif

interval_i -= 1
word_counter = 0
while word_counter != word_num
    interval_i += 1
    int_lab$ = Get label of interval: tier, interval_i
    if int_lab$ != ""
        word_counter += 1
    endif
endwhile


oov_meta$ = Get label of interval: tier + 1, interval_i
# if words not aligned and chunk consists of a single word
if oov_meta$ = "unintelligible"
    pre_boundary$ = ""
    post_boundary$ = ""
    boundary$ = ""
    goto SKIP
endif

# get boundary & pre-boundary phones
word_counter = 1
cur_i = interval_i + 1
pre_boundary$ = ""
while word_counter > window_start
    cur_i = cur_i - 1
    i_lab$ = Get label of interval: tier, cur_i
    if i_lab$ != ""
        word_counter = word_counter - 1
        word_end = Get end time of interval: tier, cur_i
        phon_end_i = Get low interval at time: tier + 3, word_end
        word_start = Get start time of interval: tier, cur_i
        phon_start_i = Get high interval at time: tier + 3, word_start
        meta_lab$ = Get label of interval: tier + 1, cur_i
        if meta_lab$ == "unintelligible"
            tran$ = "[UNINT]"
        else
            tran$ = ""
            for phon from phon_start_i to phon_end_i
                phon_lab$ = Get label of interval: tier + 3, phon
                if phon == phon_start_i
                    tran$ = tran$ + phon_lab$
                else
                    tran$ = tran$ + "_" + phon_lab$
                endif
            endfor
        endif
        if word_counter == 0
            pre_boundary$ = tran$ + pre_boundary$
        else
            pre_boundary$ = tran$ + "_" + pre_boundary$
        endif
        if meta_lab$ == "unintelligible"
            word_counter = window_start
        endif
    endif
    # handle intervals at start of file
    if cur_i <= 1
        word_counter = window_start
    endif
endwhile

if index(pre_boundary$, "_") != 0
    for i from 1 to n_phon
        us_i = rindex(pre_boundary$, "_")
        if i == 1
            boundary$ = right$(pre_boundary$, length(pre_boundary$) - us_i)
        else
            boundary$ = right$(pre_boundary$, length(pre_boundary$) - us_i) + "_" + boundary$
        endif
        pre_boundary$ = left$(pre_boundary$, us_i - 1)
    endfor
else
    boundary$ = pre_boundary$
    pre_boundary$ = ""
endif

# get post-boundary phones
word_counter = 0
cur_i = interval_i
max_i = Get number of intervals: tier
post_boundary$ = ""
while (word_counter < window_end) and (cur_i < max_i)
    cur_i = cur_i + 1
    i_lab$ = Get label of interval: tier, cur_i
    if i_lab$ != ""
        word_counter = word_counter + 1
        word_end = Get end time of interval: tier, cur_i
        phon_end_i = Get low interval at time: tier + 3, word_end
        word_start = Get start time of interval: tier, cur_i
        phon_start_i = Get high interval at time: tier + 3, word_start
        meta_lab$ = Get label of interval: tier + 1, cur_i
        if meta_lab$ == "unintelligible"
            tran$ = "[UNINT]"
        else
            tran$ = ""
            for phon from phon_start_i to phon_end_i
                phon_lab$ = Get label of interval: tier + 3, phon
                if phon == phon_start_i
                    tran$ = tran$ + phon_lab$
                else
                    tran$ = tran$ + "_" + phon_lab$
                endif
            endfor
        endif
        if word_counter == 1
            post_boundary$ = post_boundary$ + tran$
        else
            post_boundary$ = post_boundary$ + "_" + tran$
        endif
        if meta_lab$ == "unintelligible"
            word_counter = window_end
        endif
    endif
endwhile

label SKIP

writeInfoLine: pre_boundary$ + " " + boundary$ + " " + post_boundary$
