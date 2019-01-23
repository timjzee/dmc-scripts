form Give chunks
    word chunk_path /Volumes/tensusers/timzee/cgn/chunks_PRAAT.txt
    word wav_path /Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-
    word annot_path /Volumes/bigdata2/corpora2/CGN2/data/annot/text/awd/comp-
    word tens_path /Volumes/tensusers/timzee/cgn/
    word phone_path /Volumes/timzee/Docs/KALDI-CGN_phones2.txt
    real agreement_threshold 0.02
endform

procedure getKwordInt: getKwordInt.index
    getKwordInt.num_ints = Get number of intervals: 5
    getKwordInt.real_k_words = 0
    for getKwordInt.int from 1 to getKwordInt.num_ints
        getKwordInt.label$ = Get label of interval: 5, getKwordInt.int
        if getKwordInt.label$ != ""
            getKwordInt.real_k_words += 1
            if getKwordInt.real_k_words == getKwordInt.index
#                appendInfoLine: "word match"
                goto FINISHED
            endif
        endif
    endfor
    label FINISHED
endproc

procedure measure: measure.int_label$, measure.s_word$, measure.boundary$, measure.cgn_time$, measure.kaldi_time$, measure.cgn_min_kaldi$, measure.agreement$, measure.cgn_tran$, measure.kaldi_tran$
    selectObject: "Table measurements"
    Append row
    num_rows = Get number of rows
    Set string value: num_rows, "filepath", f_path$
    Set string value: num_rows, "channel", c_channel$
    Set string value: num_rows, "chunk_start", c_start$
    Set string value: num_rows, "chunk_end", c_end$
    Set string value: num_rows, "word", measure.int_label$
    Set string value: num_rows, "s_word", measure.s_word$
    Set string value: num_rows, "boundary", measure.boundary$
    Set string value: num_rows, "cgn_time", measure.cgn_time$
    Set string value: num_rows, "kaldi_time", measure.kaldi_time$
    Set string value: num_rows, "cgn-kaldi", measure.cgn_min_kaldi$
    Set string value: num_rows, "agreement", measure.agreement$
    Set string value: num_rows, "cgn_tran", measure.cgn_tran$
    Set string value: num_rows, "kaldi_tran", measure.kaldi_tran$
    selectObject: "TextGrid " + s_name$ + "_all"
endproc

Read Table from comma-separated file: phone_path$
Rename: "phones"
Create Table with column names: "measurements", 0, "filepath channel chunk_start chunk_end word s_word boundary cgn_time kaldi_time cgn-kaldi agreement cgn_tran kaldi_tran"
Read Table from comma-separated file: chunk_path$
Rename: "chunks"

num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    f_path$ = Get value: id, "filepath"
    c_channel$ = Get value: id, "channel"
    c_channel = number(c_channel$)
    c_start$ = Get value: id, "chunk_start"
    c_start = number(c_start$)
    c_end$ = Get value: id, "chunk_end"
    c_end = number(c_end$)
    c_ort$ = Get value: id, "chunk_ort"
    w_ort$ = Get value: id, "word_ort"

    Open long sound file: wav_path$ + f_path$ + ".wav"
    s_name$ = selected$("LongSound")
    To TextGrid: "phon_CGN word_CGN tran_CGN phon_KALDI word_KALDI tran_KALDI phon_man word_man tran_man orth", ""
    Rename: s_name$ + "_all"

    ## Load CGN TextGrids
    # First unzip the .gz files
    runSystem_nocheck: "cp " + annot_path$ + f_path$ + ".awd.gz " + tens_path$
    runSystem_nocheck: "gunzip " + tens_path$ + s_name$ + ".awd.gz"
    # Load the textgrids
    Read from file: tens_path$ + s_name$ + ".awd"
    cgn_channel = (c_channel + 1) * 3
    start_phon_interval = Get high interval at time: cgn_channel, c_start
    end_phon_interval = Get high interval at time: cgn_channel, c_end
    end_phon_interval = end_phon_interval - 1
    if start_phon_interval == end_phon_interval
        @measure: "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"
        goto SKIPCHUNK
    endif
    # copy and convert phones to new textgrid
    for interval from start_phon_interval to end_phon_interval
        selectObject: "TextGrid " + s_name$
        if interval == start_phon_interval
            start_time = Get start time of interval: cgn_channel, interval
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 1, start_time
            selectObject: "TextGrid " + s_name$
        endif
        end_time = Get end time of interval: cgn_channel, interval
        cgn_phon$ = Get label of interval: cgn_channel, interval
        if cgn_phon$ == "J"
            phon_dur = end_time - start_time
            n_end_time = start_time + phon_dur / 2
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 1, n_end_time
            new_interval = Get low interval at time: 1, n_end_time
            Set interval text: 1, new_interval, "n"
            Insert boundary: 1, end_time
            Set interval text: 1, new_interval + 1, "j"
        else
            selectObject: "Table phones"
            phon_index = Search column: "CGN", cgn_phon$
            if phon_index = 0
                kaldi_phon$ = "[SPN]"
            else
                kaldi_phon$ = Get value: phon_index, "KALDI"
            endif
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 1, end_time
            new_interval = Get low interval at time: 1, end_time
            Set interval text: 1, new_interval, kaldi_phon$
        endif
    endfor
    # copy and convert words and transcriptions to new textgrid
    selectObject: "TextGrid " + s_name$
    start_word_interval = Get high interval at time: cgn_channel - 2, c_start
    end_word_interval = Get high interval at time: cgn_channel - 2, c_end
    end_word_interval = end_word_interval - 1
    for interval from start_word_interval to end_word_interval
        selectObject: "TextGrid " + s_name$
        start_time = Get start time of interval: cgn_channel - 2, interval
        if interval == start_word_interval
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 2, start_time
            Insert boundary: 3, start_time
            selectObject: "TextGrid " + s_name$
        endif
        end_time = Get end time of interval: cgn_channel - 2, interval
        cgn_word$ = Get label of interval: cgn_channel - 2, interval
        selectObject: "TextGrid " + s_name$ + "_all"
        Insert boundary: 2, end_time
        Insert boundary: 3, end_time
        new_interval = Get low interval at time: 2, end_time
        Set interval text: 2, new_interval, cgn_word$
        cgn_tran$ = ""
        first_fon = Get high interval at time: 1, start_time
        last_fon = Get low interval at time: 1, end_time
        for f from first_fon to last_fon
            f_lab$ = Get label of interval: 1, f
            cgn_tran$ = cgn_tran$ + f_lab$
        endfor
        Set interval text: 3, new_interval, cgn_tran$
    endfor
    runSystem_nocheck: "rm " + tens_path$ + s_name$ + ".awd"
    selectObject: "TextGrid " + s_name$
    Remove

    ## Load KALDI TextGrid
    ali_name1$ = replace$(f_path$, "/", "_", 2)
    # Louis turned all 0 channels to 1; all 1 channels to 2; and >1 channels to 1
    if (c_channel == 0) or (c_channel == 1)
        ali_name2$ = "_" + string$(c_channel + 1) + "_"
    else
        ali_name2$ = "_1_"
    endif
    ali_name$ = ali_name1$ + ali_name2$ + c_start$ + "_" + c_end$ + ".ali"
    Read Table from tab-separated file: tens_path$ + "KALDI_output/CGN_beam_5_100/" + ali_name$
    Rename: "alignment"
    word_end = -1
    num_phons = Get number of rows
    for phon_index from 1 to num_phons
        selectObject: "Table alignment"
        start_time$ = Get value: phon_index, "start"
        start_time = number(start_time$)
        if phon_index == 1
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 4, c_start + start_time
            selectObject: "Table alignment"
        endif
        phon_dur$ = Get value: phon_index, "dur"
        phon_dur = number(phon_dur$)
        phon_text$ = Get value: phon_index, "phone"
        selectObject: "TextGrid " + s_name$ + "_all"
        Insert boundary: 4, c_start + start_time + phon_dur
        underscore_index = index(phon_text$, "_")
        if underscore_index == 0
            phon$ = phon_text$
            phon_tag$ = "I"
        else
            phon$ = left$(phon_text$, underscore_index - 1)
            phon_tag$ = right$(phon_text$, length(phon_text$) - underscore_index)
        endif
        phon_interval = Get low interval at time: 4, c_start + start_time + phon_dur
        Set interval text: 4, phon_interval, phon$
        if (phon_tag$ == "B") or (phon_tag$ == "S")
            word_start = c_start + start_time
            word_tran$ = ""
            if word_start > word_end
                Insert boundary: 5, word_start
                Insert boundary: 6, word_start
            endif
        endif
        word_tran$ = word_tran$ + phon$
        if (phon_tag$ == "E") or (phon_tag$ == "S")
            word_end = c_start + start_time + phon_dur
            Insert boundary: 5, word_end
            Insert boundary: 6, word_end
            word_interval = Get low interval at time: 5, word_end
            Set interval text: 5, word_interval, "word"
            Set interval text: 6, word_interval, word_tran$
        endif
    endfor
    ## Manual textgrid processing
    ## Evaluation measurements
    # Check if number of words are equal
    selectObject: "TextGrid " + s_name$ + "_all"
    num_cgn_words = Count intervals where: 2, "is not equal to", ""
    num_kaldi_words = Count intervals where: 5, "is not equal to", ""
    # For now let's compare all words
    if num_cgn_words == num_kaldi_words
        actual_word_i = 0
        num_cgn_ints = Get number of intervals: 2
        for int from 1 to num_cgn_ints
            int_label$ = Get label of interval: 2, int
            if int_label$ != ""
                actual_word_i += 1
                # check if cgn and kaldi word have equal amount of boundaries
                cgn_word_start = Get start time of interval: 2, int
                cgn_word_end = Get end time of interval: 2, int
                cgn_tran$ = Get label of interval: 3, int
                first_cgn_fon = Get high interval at time: 1, cgn_word_start
                last_cgn_fon = Get low interval at time: 1, cgn_word_end
                num_cgn_fon = last_cgn_fon - first_cgn_fon + 1
                # split cgn phons if necessary
                last_cgn_fon_end = Get end time of interval: 1, last_cgn_fon
                if last_cgn_fon_end != cgn_word_end
                    Insert boundary: 1, cgn_word_end
                    last_cgn_fon_lab$ = Get label of interval: 1, last_cgn_fon
                    Set interval text: 1, last_cgn_fon + 1, last_cgn_fon_lab$
                endif
                @getKwordInt: actual_word_i
#                kaldi_int = getKwordInt.int
#                appendInfoLine: getKwordInt.int
                kaldi_word_start = Get start time of interval: 5, getKwordInt.int
                kaldi_word_end = Get end time of interval: 5, getKwordInt.int
                kaldi_tran$ = Get label of interval: 6, getKwordInt.int
                first_kaldi_fon = Get high interval at time: 4, kaldi_word_start
                last_kaldi_fon = Get low interval at time: 4, kaldi_word_end
                num_kaldi_fon = last_kaldi_fon - first_kaldi_fon + 1
                if num_cgn_fon == num_kaldi_fon
                    # for now we'll measure starting and ending boundaries for each word
                    # even though this results in duplicate measurements
                    boundary = 0
                    for phon from 1 to num_cgn_fon
                        if phon == 1
                            boundary += 1
                            cgn_time = Get start time of interval: 1, first_cgn_fon + phon - 1
                            kaldi_time = Get start time of interval: 4, first_kaldi_fon + phon - 1
                            cgn_min_kaldi = cgn_time - kaldi_time
                            if abs(cgn_min_kaldi) > agreement_threshold
                                agreement = 0
                            else
                                agreement = 1
                            endif
                            if index_regex(int_label$, w_ort$ + "[.!?]?$") != 1
                                s_word$ = "0"
                            else
                                s_word$ = "1"
                            endif
                            @measure: int_label$, s_word$, string$(boundary), string$(cgn_time), string$(kaldi_time), string$(cgn_min_kaldi), string$(agreement), cgn_tran$, kaldi_tran$
                        endif
                        boundary += 1
                        cgn_time = Get end time of interval: 1, first_cgn_fon + phon - 1
                        kaldi_time = Get end time of interval: 4, first_kaldi_fon + phon - 1
                        cgn_min_kaldi = cgn_time - kaldi_time
                        if abs(cgn_min_kaldi) > agreement_threshold
                            agreement = 0
                        else
                            agreement = 1
                        endif
                        if index_regex(int_label$, w_ort$ + "[.!?]?$") != 1
                            s_word$ = "0"
                        else
                            s_word$ = "1"
                        endif
                        @measure: int_label$, s_word$, string$(boundary), string$(cgn_time), string$(kaldi_time), string$(cgn_min_kaldi), string$(agreement), cgn_tran$, kaldi_tran$
                    endfor
                else
                    if index_regex(int_label$, w_ort$ + "[.!?]?$") != 1
                        s_word$ = "0"
                    else
                        s_word$ = "1"
                    endif
                    @measure: int_label$, s_word$, "NA", "NA", "NA", "NA", "NA", cgn_tran$, kaldi_tran$
                endif
            endif
        endfor
    else
        @measure: "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"
    endif
    ## Go to next chunk
#    plusObject: "LongSound " + s_name$
#    View & Edit
#    editor: "TextGrid " + s_name$ + "_all"
#        Select: c_start, c_end
#        Zoom to selection
#    endeditor
#    beginPause: "Continue"
#        comment: "Annotate " + w_ort$
#        comment: "Click continue."
#    endPause: "Continue", 1
    selectObject: "Table alignment"
    Remove
    label SKIPCHUNK
    selectObject: "LongSound " + s_name$
    Remove
    selectObject: "TextGrid " + s_name$ + "_all"
    Remove
endfor

selectObject: "Table measurements"
Save as comma-separated file: tens_path$ + "agreement_measurements.csv"
total_rows = Get number of rows
Extract rows where column (text): "agreement", "is equal to", "NA"
na_rows = Get number of rows
selectObject: "Table measurements"
Extract rows where column (text): "agreement", "is equal to", "1"
agreement_rows = Get number of rows
Remove
writeInfoLine: "Mean percent agreement: ", 100 * (agreement_rows / (total_rows - na_rows)), "%"

selectObject: "Table measurements"
Extract rows where column (text): "s_word", "is equal to", "1"
total_rows = Get number of rows
Extract rows where column (text): "agreement", "is equal to", "NA"
na_rows = Get number of rows
selectObject: "Table measurements_1"
Extract rows where column (text): "agreement", "is equal to", "1"
agreement_rows = Get number of rows
appendInfoLine: "Mean percent agreement in S words: ", 100 * (agreement_rows / (total_rows - na_rows)), "%"
