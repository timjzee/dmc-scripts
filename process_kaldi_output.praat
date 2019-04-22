prop$ = Report system properties
os$ = extractWord$(prop$, newline$)
if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/"
    cgn_path$ = "/Volumes/bigdata2/corpora2/CGN2/data/annot/text/ort/comp-"
    home_path$ = "/Volumes/timzee/"
else
    tens_path$ = "/vol/tensusers/timzee/"
    cgn_path$ = "/vol/bigdata/corpora2/CGN2/data/annot/text/ort/comp-"
    home_path$ = "/home/timzee/"
endif

procedure processWords
    # adjust boundaries
    chunk_id$ = cur_path$ + "," + ort_tier$ + "," + cur_start$ + "," + cur_end$
    selectObject: "Table conv"
    row_num = Search column: "chunk_id", chunk_id$
    if row_num != 0 and ali_lab$ != cur_ort$
        input_i$ = Get value: row_num, "input_i"
        if input_i$ != ""
            # we'll delete the left boundaries of the subwords from right to left so we won't change the interval numbers
            # we do have to account for SIL intervals though
            comma_index = -1
            while comma_index != 0
                comma_index = rindex(input_i$, ",")
                input$ = right$(input_i$, length(input_i$) - comma_index)
                input_i$ = left$(input_i$, comma_index - 1)
                space_index = -1
                while space_index != 0
                    space_index = rindex(input$, " ")
                    if space_index == 0
                        # we don't delete the left boundary in the left-most subword
                        goto BREAK
                    endif
                    sw_i$ = right$(input$, length(input$) - space_index)
                    sw_i = number(sw_i$)
                    input$ = left$(input$, space_index - 1)
                    # Now lets find the actual interval number of the subword
                    selectObject: "TextGrid " + wav_name$
                    sw_count = 0
                    actual_i = c_start_i - 1
                    while sw_count != sw_i
                        actual_i += 1
                        i_tran$ = Get label of interval: spkr_tier + 2, actual_i
                        if i_tran$ != "SIL"
                            sw_count += 1
                        endif
                        # handle silences that were inserted between subwords
                        if (sw_count == sw_i - 1) and i_tran$ == "SIL"
                            Set interval text: spkr_tier + 2, actual_i, ""
                            Remove left boundary: spkr_tier + 2, actual_i
                            Remove left boundary: spkr_tier + 1, actual_i
                            Remove left boundary: spkr_tier, actual_i
                            actual_i -= 1
                        endif
                    endwhile
                    Remove left boundary: spkr_tier + 2, actual_i
                    Remove left boundary: spkr_tier + 1, actual_i
                    Remove left boundary: spkr_tier, actual_i
                endwhile
                label BREAK
            endwhile
        endif
    endif
    # now let's fill in the ort and meta tiers
    # first handle exception when no .ali file exists
    if ali_lab$ == cur_ort$
        selectObject: "TextGrid " + wav_name$
        Set interval text: spkr_tier + 1, c_start_i, "unintelligible"
        Set interval text: spkr_tier, c_start_i, cur_ort$
    else
        actual_i = c_start_i
        words$ = cur_ort$ + " "
        space_index = -1
        word_counter = 0
        if row_num != 0
            selectObject: "Table conv"
            meta_i$ = Get value: row_num, "meta_indices"
            meta_i$ = "," + meta_i$ + ","
            meta_s$ = Get value: row_num, "meta_info"
            meta_s$ = meta_s$ + ","
            meta_ss$ = meta_s$
        endif
        while space_index != 0
            selectObject: "TextGrid " + wav_name$
            tran_lab$ = Get label of interval: spkr_tier + 2, actual_i
            if tran_lab$ != "SIL"
                word_counter += 1
                space_index = index(words$, " ")
                word$ = left$(words$, space_index - 1)
                words$ = right$(words$, length(words$) - space_index)
                Set interval text: spkr_tier, actual_i, word$
                if row_num != 0
                    if index(meta_i$, "," + string$(word_counter) + ",") != 0
                        meta_c_i = index(meta_s$, ",")
                        meta$ = left$(meta_s$, meta_c_i - 1)
                        meta_s$ = right$(meta_s$, length(meta_s$) - meta_c_i)
                        Set interval text: spkr_tier + 1, actual_i, meta$
                    endif
                    if index(meta_ss$, "unintelligible") != 0 and word$ != ""
                        meta_lab$ = Get label of interval: spkr_tier + 1, actual_i
                        if meta_lab$ == ""
                            Set interval text: spkr_tier + 1, actual_i, "(unint.)"
                        else
                            if meta_lab$ != "unintelligible"
                                Set interval text: spkr_tier + 1, actual_i, meta_lab$ + ";(unint.)"
                            endif
                        endif
                    endif
                endif
            endif
            actual_i += 1
        endwhile
    endif
endproc

procedure makeKaldiTG
    selectObject: "Table ali"
    word_end = -1
    w_count = 0
    n_ali_lines = Get number of rows
    for ali_line from 1 to n_ali_lines
        selectObject: "Table ali"
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
        if ali_line == 1 and (cur_start + start_time) != 0
            # let's check for existing boundary
            selectObject: "TextGrid " + wav_name$
            num_phons = Get number of intervals: spkr_tier + 3
            prev_boundary_time = Get start time of interval: spkr_tier + 3, num_phons
            if cur_start + start_time - prev_boundary_time > 0.001
                Insert boundary: spkr_tier + 3, cur_start + start_time
            endif
            selectObject: "Table ali"
        endif
        phon_dur$ = Get value: ali_line, "dur"
        phon_dur = number(phon_dur$)
        end_time = cur_start + start_time + phon_dur
        selectObject: "TextGrid " + wav_name$
        total_dur = Get total duration
        if total_dur - end_time > 0.001
            Insert boundary: spkr_tier + 3, end_time
            phon_interval = Get low interval at time: spkr_tier + 3, end_time
        else
            phon_interval = Get number of intervals: spkr_tier + 3
        endif
        Set interval text: spkr_tier + 3, phon_interval, phon$
        if (phon_tag$ == "B") or (phon_tag$ == "S")
            word_start = cur_start + start_time
            word_tran$ = ""
            if word_start != 0
                if ali_line == 1
                    if word_start - prev_boundary_time > 0.001
                        Insert boundary: spkr_tier + 2, word_start
                        Insert boundary: spkr_tier + 1, word_start
                        Insert boundary: spkr_tier, word_start
                    endif
                elsif word_start - word_end > 0.001
                    Insert boundary: spkr_tier + 2, word_start
                    Insert boundary: spkr_tier + 1, word_start
                    Insert boundary: spkr_tier, word_start
                endif
            endif
        endif
        word_tran$ = word_tran$ + phon$
        if (phon_tag$ == "E") or (phon_tag$ == "S")
            w_count += 1
            word_end = end_time
            if total_dur - word_end > 0.001
                Insert boundary: spkr_tier + 2, word_end
                Insert boundary: spkr_tier + 1, word_end
                Insert boundary: spkr_tier, word_end
                word_interval = Get low interval at time: spkr_tier + 2, word_end
            else
                word_interval = Get number of intervals: spkr_tier + 2
            endif
            if w_count == 1
                c_start_i = word_interval
            endif
            Set interval text: spkr_tier + 2, word_interval, word_tran$
        endif
    endfor
    @processWords
endproc

Read Table from tab-separated file: tens_path$ + "cgn/oov_conv_table_comp-d.txt"
Rename: "conv"
Read Table from comma-separated file: tens_path$ + "cgn/cgn_index_d_mono2.txt"
Rename: "index"

wav_name$ = ""
prev_speaker$ = ""
n_inputlines = Get number of rows
for line from 1 to n_inputlines
    if line != 1
        removeObject: "Table ali"
    endif
    selectObject: "Table index"
    cur_ort$ = Get value: line, "ort"
    cur_path$ = Get value: line, "wav"
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    ort_tier$ = Get value: line, "tier"
    ort_tier = number(ort_tier$)
    cur_start$ = Get value: line, "from"
    cur_start = number(cur_start$)
    cur_end$ = Get value: line, "to"
    cur_end = number(cur_end$)
    appendInfoLine: cur_path$, ",", cur_start$, ",", cur_end$, ",", cur_ort$, ",", ort_tier$
    if cur_name$ != wav_name$
        if line != 1
            selectObject: "TextGrid " + wav_name$
            Save as text file: tens_path$ + "cgn/kaldi_annot/comp-" + pre_name$ + wav_name$ + ".awd"
            Remove
            removeObject: "TextGrid " + wav_name$ + "_ort"
        endif
        runSystem_nocheck: "cp " + cgn_path$ + cur_path$ + ".ort.gz " + tens_path$ + "cgn/"
        runSystem_nocheck: "gunzip " + tens_path$ + "cgn/" + cur_name$ + ".ort.gz"
        Read from file: tens_path$ + "cgn/" + cur_name$ + ".ort"
        runSystem_nocheck: "rm -f " + tens_path$ + "cgn/" + cur_name$ + ".ort"
        Rename: cur_name$ + "_ort"
        speaker$ = Get tier name: ort_tier
        tg_dur = Get total duration
        Create TextGrid: 0, tg_dur, speaker$ + " " + speaker$ + "_META " + speaker$ + "_FON " + speaker$ + "_SEG", ""
        Rename: cur_name$
        spkr_tier = 1
        wav_name$ = cur_name$
        prev_speaker$ = speaker$
        pre_name$ = left$(cur_path$, pre_name)
    else
        selectObject: "TextGrid " + cur_name$ + "_ort"
        speaker$ = Get tier name: ort_tier
        # assuming the index first lists all chunks from tier 1, followed by all chunks from tier 2 etc.
        if speaker$ != prev_speaker$
            selectObject: "TextGrid " + cur_name$
            num_tiers = Get number of tiers
            Insert interval tier: num_tiers + 1, speaker$
            Insert interval tier: num_tiers + 2, speaker$ + "_META"
            Insert interval tier: num_tiers + 3, speaker$ + "_FON"
            Insert interval tier: num_tiers + 4, speaker$ + "_SEG"
            spkr_tier = num_tiers + 1
            prev_speaker$ = speaker$
        endif
    endif
    ali_file$ = replace$(cur_path$, "/", "_", 2) + "_" + ort_tier$ + "_" + cur_start$ + "_" + cur_end$ + ".ali"
    if fileReadable(tens_path$ + "KALDI_FA_out/comp-d_mono_all/" + ali_file$)
        Read Table from tab-separated file: tens_path$ + "KALDI_FA_out/comp-d_mono_all/" + ali_file$
        Rename: "ali"
    else
        Create Table with column names: "table", 1, "start dur phone"
        Rename: "ali"
        Set string value: 1, "start", "0.0"
        ali_end$ = fixed$(cur_end - cur_start, 3)
        Set string value: 1, "dur", ali_end$
        Set string value: 1, "phone", cur_ort$
    endif
    @makeKaldiTG
endfor

selectObject: "TextGrid " + wav_name$
Save as text file: tens_path$ + "cgn/kaldi_annot/comp-" + pre_name$ + wav_name$ + ".awd"
