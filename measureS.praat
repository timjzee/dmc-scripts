prop$ = Report system properties
os$ = extractWord$(prop$, newline$)
if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/cgn/"
    cgn_path$ = "/Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-"
else
    tens_path$ = "/vol/tensusers/timzee/cgn/"
    cgn_path$ = "/vol/bigdata/corpora2/CGN2/data/audio/wav/comp-"
endif


procedure makeKaldiTG
    selectObject: "Table " + ali_name$
    word_end = -1
    n_ali_lines = Get number of rows
    for ali_line from 1 to n_ali_lines
        selectObject: "Table " + ali_name$
        ali_lab$ = Get value: ali_line, "phone"
        underscore_index = index(ali_lab$, "_")
        if underscore_index == 0
            phon$ = ali_lab$
            phon_tag$ = "S"
        else
            phon$ = left$(ali_lab$, underscore_index - 1)
            phon_tag$ = right$(ali_lab$, length(ali_lab$) - underscore_index)
        endif
        start_time$ = Get value: ali_line, "start"
        start_time = number(start_time$)
        if ali_line == 1
            selectObject: "TextGrid " + wav_name$
            Insert boundary: 1, cur_start + start_time
            selectObject: "Table " + ali_name$
        endif
        phon_dur$ = Get value: ali_line, "dur"
        phon_dur = number(phon_dur$)
        end_time = cur_start + start_time + phon_dur
        selectObject: "TextGrid " + wav_name$
        Insert boundary: 1, end_time
        phon_interval = Get low interval at time: 1, end_time
        Set interval text: 1, phon_interval, phon$
        if (phon_tag$ == "B") or (phon_tag$ == "S")
            word_start = cur_start + start_time
            word_tran$ = ""
            if word_start > word_end
                Insert boundary: 2, word_start
            endif
        endif
        word_tran$ = word_tran$ + phon$
        if (phon_tag$ == "E") or (phon_tag$ == "S")
            word_end = end_time
            Insert boundary: 2, word_end
            word_interval = Get low interval at time: 2, word_end
            Set interval text: 2, word_interval, word_tran$
        endif
    endfor
endproc


Read Table from comma-separated file: tens_path$ + "praat_test.csv"
table_name$ = selected$("Table")
wav_name$ = ""
ali_name$ = ""
n_inputlines = Get number of rows
for s_line from 1 to n_inputlines
    selectObject: "Table " + table_name$
    cur_path$ = Get value: s_line, "wav"
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    cur_start$ = Get value: s_line, "chunk_start"
    cur_start = number(cur_start$)
    cur_end$ = Get value: s_line, "chunk_end"
    cur_end = number(cur_end$)
    ali_name1$ = replace$(cur_path$, "/", "_", 2)
    c_channel$ = Get value: s_line, "chan"
    c_channel = number(c_channel$)
    # Louis turned all 0 channels to 1; all 1 channels to 2; and >1 channels to 1
    if (c_channel == 1) or (c_channel == 2)
        ali_name2$ = "_" + string$(c_channel) + "_"
    else
        ali_name2$ = "_1_"
    endif
    ali_file$ = ali_name1$ + ali_name2$ + cur_start$ + "_" + cur_end$
    # We only want to load a .wav file once
    if cur_name$ != wav_name$
        if s_line > 1
            selectObject: "LongSound " + wav_name$
            Remove
        endif
        Open long sound file: cgn_path$ + cur_path$ + ".wav"
        wav_name$ = selected$("LongSound")
    endif
    # We only want to load & convert the .ali file to a tg once
    if ali_name$ != replace$(ali_file$, ".", "_", 0)
        if s_line > 1
            selectObject: "Table " + ali_name$
            Remove
            selectObject: "TextGrid " + wav_name$
            Remove
        endif
        Read Table from tab-separated file: tens_path$ + "KALDI_output/CGN_beam_5_100_v3/" + ali_file$ + ".ali"
        ali_name$ = replace$(ali_file$, ".", "_", 0)
        selectObject: "LongSound " + wav_name$
        To TextGrid: "phones words", ""
        @makeKaldiTG
    endif
    selectObject: "Table " + table_name$
    chunk_i$ = Get value: s_line, "word_chunk_i"
    chunk_i = number(chunk_i$)
    # now let's get the corresponding index in the tg
    selectObject: "TextGrid " + wav_name$
    num_ints = Get number of intervals: 2
    word_counter = 0
    word_int = 0
    while word_counter < chunk_i
        word_int += 1
        int_lab$ = Get label of interval: 2, word_int
        if int_lab$ != "" and int_lab$ != "SIL"
            word_counter += 1
        endif
    endwhile
    # word_int now contains the interval number for the word containing the /s/
    word_end = Get end time of interval: 2, word_int
    s_int = Get low interval at time: 1, word_end
    s_start = Get start time of interval: 1, s_int
    s_end = Get end time of interval: 1, s_int
    s_duration = s_end - s_start
    appendInfoLine: s_duration
endfor
