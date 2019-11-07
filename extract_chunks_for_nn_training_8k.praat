corpus$ = "cgn"
test_overlap = 0
if corpus$ == "IFADVcorpus"
    component$ = "ifadv"
    a_path$ = "/tensusers/timzee/IFADVcorpus/Speech/"
    o_path$ = "/tensusers/timzee/IFADVcorpus/Annotations/ort/"
elif corpus$ == "cgn"
    component$ = "d"
    o_path$ = "/bigdata2/corpora2/CGN2/data/annot/text/ort/comp-"
    if component$ == "c" or component$ == "d"
        a_path$ = "/tensusers/timzee/cgn/mono_comp-"
    else
        a_path$ = "/bigdata2/corpora2/CGN2/data/audio/wav/comp-"
    endif
    if component$ == "a"
        test_overlap = 1
    endif
else
    component$ = "ecsd"
    a_path$ = "/tensusers/timzee/ECSD/Speech/"
    o_path$ = "/tensusers/timzee/ECSD/Annotations/ort/"
endif


if macintosh
    tens_path$ = "/Volumes/tensusers/timzee/" + corpus$ + "/"
    audio_path$ = "/Volumes" + a_path$
    ort_path$ = "/Volumes" + o_path$
    frag_path$ = "/Volumes/tensusers/timzee/af_classification/training_chunks/" + component$ + "/"
else
    tens_path$ = "/vol/tensusers/timzee/" + corpus$ + "/"
    audio_path$ = "/vol" + a_path$
    ort_path$ = "/vol" + o_path$
    frag_path$ = "/vol/tensusers/timzee/af_classification/training_chunks/" + component$ + "/"
endif

if corpus$ == "IFADVcorpus"
    Read Table from comma-separated file: tens_path$ + "/speakers.csv"
endif

Read Table from comma-separated file: tens_path$ + "cgn_index_d_mono2.txt"
table_name$ = selected$("Table")

wav_name$ = ""
n_inputlines = Get number of rows
for s_line from 1 to n_inputlines
    selectObject: "Table " + table_name$
    cur_path$ = Get value: s_line, "wav"
    # check language if cgn
    if corpus$ == "cgn"
        if index(cur_path$, "/vl/")
            goto NEXT
        endif
    endif
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    cur_start$ = Get value: s_line, "from"
    cur_start = number(cur_start$)
    cur_end$ = Get value: s_line, "to"
    cur_end = number(cur_end$)
    appendInfoLine: "Working on ", cur_name$, " from ", cur_start$, " to ", cur_end$
    c_channel$ = Get value: s_line, "chan"
    c_channel = number(c_channel$)
    c_tier$ = Get value: s_line, "tier"
    c_tier = number(c_tier$)

    # We only want to load the .wav and .awd file once
    if cur_name$ != wav_name$
        if s_line > 1
            removeObject: "LongSound " + wav_name$
            removeObject: "TextGrid " + wav_name$
            removeObject: "TextGrid " + wav_name$ + "_ort"
        endif
        if corpus$ == "cgn"
            Open long sound file: audio_path$ + cur_path$ + ".wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + "kaldi_annot/v2/comp-" + cur_path$ + ".awd"
            runSystem_nocheck: "cp " + ort_path$ + cur_path$ + ".ort.gz " + tens_path$
            runSystem_nocheck: "gunzip " + tens_path$ + cur_name$ + ".ort.gz"
            Read from file: tens_path$ + cur_name$ + ".ort"
            runSystem_nocheck: "rm -f " + tens_path$ + cur_name$ + ".ort"
            Rename: wav_name$ + "_ort"
        elif corpus$ == "IFADVcorpus"
            Open long sound file: audio_path$ + cur_path$ + ".wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + "kaldi_annot/v2/" + cur_path$ + ".awd"
            Create Strings as file list: "fileList", ort_path$ + cur_name$ + "*.ort"
            ifadv_path$ = Get string: 1
            Read from file: tens_path$ + "/Annotations/ort/" + ifadv_path$
            Rename: wav_name$ + "_ort"
        else
            pair_length = name_length - 10
            pair_folder$ = "PP" + mid$(cur_path$, 3, pair_length)
            Open long sound file: audio_path$ + pair_folder$ + "/" + cur_path$ + "_S.wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + "kaldi_annot/v2/" + pair_folder$ + "/" + cur_path$ + ".awd"
            Rename: wav_name$
            Read from file: ort_path$ + pair_folder$ + "/" + cur_path$ + ".TextGrid"
            Rename: wav_name$ + "_ort"
        endif
    endif
    selectObject: "TextGrid " + wav_name$ + "_ort"
    speaker$ = Get tier name: c_tier
    if corpus$ == "IFADVcorpus"
        selectObject: "Table speakers"
        file_row = Search column: "file", cur_name$
        speaker_temp$ = speaker$
        speaker$ = Get value: file_row, speaker_temp$
        selectObject: "TextGrid " + cur_name$ + "_ort"
    endif
    # check if segmented
    selectObject: "TextGrid " + wav_name$
    tier_name$ = ""
    tier = 0
    while tier_name$ != speaker$
        tier += 1
        tier_name$ = Get tier name: tier
    endwhile
    start_i = Get high interval at time: tier + 1, cur_start
    end_i = Get low interval at time: tier + 1, cur_end
    for i from start_i to end_i
        i_lab$ = Get label of interval: tier + 1, i
        if i_lab$ == "unintelligible"
            goto NEXT
        endif
    endfor
    # check overlap
    if test_overlap
        selectObject: "TextGrid " + wav_name$ + "_ort"
        num_tiers = Get number of tiers
        for t from 1 to num_tiers
            if t != c_tier
                start_i = Get high interval at time: t, cur_start
                end_i = Get low interval at time: t, cur_end
                if start_i != end_i
                    goto NEXT
                else
                    i_lab$ = Get label of interval: t, start_i
                    if i_lab$ != ""
                        goto NEXT
                    endif
                endif
            endif
        endfor
    endif
    # extract chunk
    selectObject: "LongSound " + wav_name$
    Extract part: cur_start, cur_end, "yes"
    Extract one channel: c_channel
    sample_f = Get sampling frequency
    if sample_f != 8000
        Resample: 8000, 50
    endif
    Save as WAV file: frag_path$ + replace$(cur_path$, "/", "_", 0) + "_" + cur_start$ + "_" + cur_end$ + ".wav"
    if sample_f != 8000
        removeObject: "Sound " + wav_name$ + "_ch" + c_channel$ + "_8000"
    endif
    removeObject: "Sound " + wav_name$ + "_ch" + c_channel$
    removeObject: "Sound " + wav_name$
    selectObject: "TextGrid " + wav_name$
    Extract part: cur_start, cur_end, "yes"
    Extract one tier: tier + 3
    Save as text file: frag_path$ + replace$(cur_path$, "/", "_", 0) + "_" + cur_start$ + "_" + cur_end$ + ".TextGrid"
    Remove
    removeObject: "TextGrid " + wav_name$ + "_part"
    label NEXT
endfor
