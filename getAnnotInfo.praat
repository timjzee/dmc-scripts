form Command line parameters
    word f_path 1
    real c_start 1
    real c_end 1
    word speaker 1
    integer word_num 1
    word corpus 1
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
    phon_pron$ = "NA"
    next_phon_pron$ = "NA"
    prev_phon_pron$ = "NA"
    overlap$ = "NA"
    oov_meta$ = "unintelligible"
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
    phon_pron$ = "NA"
    next_phon_pron$ = "NA"
    prev_phon_pron$ = "NA"
    overlap$ = "NA"
    goto SKIP
endif

phon_end = Get end time of interval: tier, interval_i
phon_i = Get low interval at time: tier + 3, phon_end
phon_start = Get start time of interval: tier + 3, phon_i

# get label of /s/, prev_phon_pron, next_phon_pron, oov_meta
phon_pron$ = Get label of interval: tier + 3, phon_i

if phon_i == 1
    prev_phon_pron$ = "NA"
else
    prev_phon_pron$ = Get label of interval: tier + 3, phon_i - 1
    if prev_phon_pron$ == ""
        prev_phon_pron$ = "SIL"
    endif
endif
total_ints = Get number of intervals: tier + 3
if phon_i == total_ints
    next_phon_pron$ = "NA"
else
    next_phon_pron$ = Get label of interval: tier + 3, phon_i + 1
    if next_phon_pron$ == ""
        next_phon_pron$ = "SIL"
    endif
endif

# get overlap
overlap_buffer = 0.05
if phon_start < overlap_buffer
    overlap_start = 0
else
    overlap_start = phon_start - overlap_buffer
endif
total_dur = Get total duration
if (total_dur - phon_end) < overlap_buffer
    overlap_end = total_dur
else
    overlap_end = phon_end + overlap_buffer
endif

num_tiers = Get number of tiers
num_speakers = num_tiers / 4
overlap$ = "FALSE"
for spkr from 1 to num_speakers
    # skip the current speaker
    if spkr != ((tier + 3) / 4)
        i_start = Get interval at time: spkr * 4, overlap_start
        i_end = Get interval at time: spkr * 4, overlap_end
        for i from i_start to i_end
            i_lab$ = Get label of interval: spkr * 4, i
            if i_lab$ != "" and i_lab$ != "SIL"
                overlap$ = "TRUE"
                goto SKIP
            endif
        endfor
    endif
endfor
label SKIP

writeInfoLine: phon_pron$ + " " + next_phon_pron$ + " " + prev_phon_pron$ + " " + overlap$ + " " + oov_meta$
