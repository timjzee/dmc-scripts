form Give chunks
    word chunk_path /Volumes/tensusers/timzee/cgn/chunks_PRAAT.txt
    word wav_path /Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-
    word annot_path /Volumes/bigdata2/corpora2/CGN2/data/annot/text/awd/comp-
    word tens_path /Volumes/tensusers/timzee/cgn/
    word phone_path /Volumes/timzee/Docs/KALDI-CGN_phones2.txt
endform

Read Table from comma-separated file: phone_path$
Rename: "phones"
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
    To TextGrid: "phon_CGN word_CGN phon_KALDI word_KALDI phon_man word_man orth", ""
    Rename: s_name$ + "_all"

    ## Load CGN TextGrids
    # First unzip the .gz files
    runSystem: "cp " + annot_path$ + f_path$ + ".awd.gz " + tens_path$
    runSystem: "gunzip " + tens_path$ + s_name$ + ".awd.gz"
    # Load the textgrids
    Read from file: tens_path$ + s_name$ + ".awd"
    cgn_channel = (c_channel + 1) * 3
    start_phon_interval = Get high interval at time: cgn_channel, c_start
    end_phon_interval = Get high interval at time: cgn_channel, c_end
    end_phon_interval = end_phon_interval - 1
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
            kaldi_phon$ = Get value: phon_index, "KALDI"
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 1, end_time
            new_interval = Get low interval at time: 1, end_time
            Set interval text: 1, new_interval, kaldi_phon$
        endif
    endfor
    # copy and convert words to new textgrid
    selectObject: "TextGrid " + s_name$
    start_word_interval = Get high interval at time: cgn_channel - 2, c_start
    end_word_interval = Get high interval at time: cgn_channel - 2, c_end
    end_word_interval = end_word_interval - 1
    for interval from start_word_interval to end_word_interval
        selectObject: "TextGrid " + s_name$
        if interval == start_word_interval
            start_time = Get start time of interval: cgn_channel - 2, interval
            selectObject: "TextGrid " + s_name$ + "_all"
            Insert boundary: 2, start_time
            selectObject: "TextGrid " + s_name$
        endif
        end_time = Get end time of interval: cgn_channel - 2, interval
        cgn_word$ = Get label of interval: cgn_channel - 2, interval
        selectObject: "TextGrid " + s_name$ + "_all"
        Insert boundary: 2, end_time
        new_interval = Get low interval at time: 2, end_time
        Set interval text: 2, new_interval, cgn_word$
    endfor
    runSystem: "rm " + tens_path$ + s_name$ + ".awd"
    selectObject: "TextGrid " + s_name$
    Remove

    ## Load KALDI TextGrid
    ali_name1$ = replace$(f_path$, "/", "_", 2)
    # Louis turned all 0 channels to 1; all 1 channels to 2; and >1 channels to 1
    if c_channel == 0 or c_channel == 1
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
            Insert boundary: 3, c_start + start_time
            selectObject: "Table alignment"
        endif
        phon_dur$ = Get value: phon_index, "dur"
        phon_dur = number(phon_dur$)
        phon_text$ = Get value: phon_index, "phone"
        selectObject: "TextGrid " + s_name$ + "_all"
        Insert boundary: 3, c_start + start_time + phon_dur
        underscore_index = index(phon_text$, "_")
        if underscore_index == 0
            phon$ = phon_text$
            phon_tag$ = "I"
        else
            phon$ = left$(phon_text$, underscore_index - 1)
            phon_tag$ = right$(phon_text$, length(phon_text$) - underscore_index)
        endif
        phon_interval = Get low interval at time: 3, c_start + start_time + phon_dur
        Set interval text: 3, phon_interval, phon$
        if phon_tag$ == "B" or phon_tag$ == "S"
            word_start = c_start + start_time
            if word_start > word_end
                Insert boundary: 4, word_start
            endif
        elsif phon_tag$ == "E" or phon_tag$ == "S"
            word_end = c_start + start_time + phon_dur
            Insert boundary: 4, word_end
            word_interval = Get low interval at time: 4, word_end
            Set interval text: 4, word_interval, "word"
        endif
    endfor
    beginPause: "Continue"
        comment: "Annotate " + w_ort$
        comment: "Click continue."
    endPause: "Continue", 1
    selectObject: "LongSound " + s_name$
    Remove
    selectObject: "TextGrid " + s_name$ + "_all"
    Remove
    selectObject: "Table alignment"
    Remove
endfor
