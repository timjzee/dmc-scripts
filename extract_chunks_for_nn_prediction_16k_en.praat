corpus$ = "cgn"
new_sample_f = 16000
if corpus$ == "IFADVcorpus"
    component$ = "ifadv"
    o_path$ = "/tensusers/timzee/IFADVcorpus/Speech/"
elif corpus$ == "cgn"
    component$ = "o"
    if component$ == "c" or component$ == "d"
        o_path$ = "/tensusers/timzee/cgn/mono_comp-"
        new_sample_f = 8000
    else
        o_path$ = "/bigdata2/corpora2/CGN2/data/audio/wav/comp-"
    endif
else
    component$ = "ecsd"
    o_path$ = "/tensusers/timzee/ECSD/Speech/"
endif

# output_path$ = "eval3/"
output_path$ = "o/"

if macintosh
    tens_path$ = "/Volumes/tensusers/timzee/"
    audio_path$ = "/Volumes" + o_path$
    frag_path$ = "/Volumes/tensusers/timzee/af_classification/pred_fragments_en/" + output_path$
else
    tens_path$ = "/vol/tensusers/timzee/"
    audio_path$ = "/vol" + o_path$
    frag_path$ = "/vol/tensusers/timzee/af_classification/pred_fragments_en/" + output_path$
endif

frag_buffer = 0.2

Read Table from tab-separated file: tens_path$ + corpus$ + "/speakers.txt"

# input_file$ = "classifier_evaluation/en/nn_eval_en_o3.csv"
input_file$ = "en_morph_project/nn_all_en_o.csv"

Read Table from comma-separated file: tens_path$ + input_file$
table_name$ = selected$("Table")

wav_name$ = ""
n_inputlines = Get number of rows
for s_line from 1 to n_inputlines
    selectObject: "Table " + table_name$
    speaker$ = Get value: s_line, "speaker"
    cur_path$ = Get value: s_line, "wav"
    pre_name = rindex(cur_path$, "/")
    name_length = length(cur_path$)
    cur_name$ = right$(cur_path$, name_length - pre_name)
    cur_start$ = Get value: s_line, "chunk_start"
    cur_start = number(cur_start$)
    cur_end$ = Get value: s_line, "chunk_end"
    cur_end = number(cur_end$)
    appendInfoLine: "Working on line ", s_line , " file ", cur_name$, " from ", cur_start$, " to ", cur_end$
    c_channel$ = Get value: s_line, "chan"
    c_channel = number(c_channel$)

    # We only want to load the .wav and .awd file once
    if cur_name$ != wav_name$
        if s_line > 1
            removeObject: "LongSound " + wav_name$
            removeObject: "TextGrid " + wav_name$
        endif
        if corpus$ == "cgn"
            Open long sound file: audio_path$ + cur_path$ + ".wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + corpus$ + "/kaldi_annot/v3/comp-" + cur_path$ + ".awd"
        elif corpus$ == "IFADVcorpus"
            Open long sound file: audio_path$ + cur_path$ + ".wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + corpus$ + "/kaldi_annot/v3/" + cur_path$ + ".awd"
        else
            pair_length = name_length - 10
            pair_folder$ = "PP" + mid$(cur_path$, 3, pair_length)
            Open long sound file: audio_path$ + pair_folder$ + "/" + cur_path$ + "_S.wav"
            wav_name$ = selected$("LongSound")
            Read from file: tens_path$ + corpus$ + "/kaldi_annot/v3/" + pair_folder$ + "/" + cur_path$ + ".awd"
            Rename: wav_name$
        endif
    endif

    selectObject: "Table " + table_name$
    chunk_i$ = Get value: s_line, "word_chunk_i"
    chunk_i = number(chunk_i$)
    # now let's get the corresponding index in the tg
    selectObject: "TextGrid " + wav_name$

    tier_name$ = ""
    tier = 0
    while tier_name$ != speaker$
        tier += 1
        tier_name$ = Get tier name: tier
    endwhile

    word_int = Get high interval at time: tier, cur_start
    interval_i_end = Get low interval at time: tier, cur_end
    num_interval_i = interval_i_end - word_int + 1
    # if words are not aligned
    if chunk_i > num_interval_i
        goto END
    endif
    word_int -= 1
    word_counter = 0
    while word_counter != chunk_i
        word_int += 1
        int_lab$ = Get label of interval: tier, word_int
        if int_lab$ != ""
            word_counter += 1
        endif
    endwhile
    # word_int now contains the interval number for the word containing the -en
    oov_meta$ = Get label of interval: tier + 1, word_int
    # if words not aligned and chunk consists of a single word
    if oov_meta$ = "unintelligible"
        goto END
    endif
    # get index of final segment
    word_end = Get end time of interval: tier, word_int
    s_int = Get low interval at time: tier + 3, word_end

    # get s label
    s_lab$ = Get label of interval: tier + 3, s_int
    # voorlopig zit er nog geen reductie regel in het lexicon die ervoor zorgt dat s een z wordt

    s_start = Get start time of interval: tier + 3, s_int
    s_end = Get end time of interval: tier + 3, s_int
    # extract fragment for af classification
    selectObject: "LongSound " + wav_name$
    sound_end = Get end time
    frag_start = s_start - frag_buffer * 1.5
    frag_start$ = fixed$(frag_start, 3)
    frag_end = s_end + frag_buffer
    if frag_start < 0
        frag_start = 0.000
        frag_start$ = "0.000"
    endif
    if frag_end > sound_end
        frag_end = sound_end
    endif
    Extract part: frag_start, frag_end, "yes"
    Extract one channel: c_channel
    samp_freq = Get sampling frequency
    if samp_freq != new_sample_f
        Resample: new_sample_f, 50
    endif
    Save as WAV file: frag_path$ + wav_name$ + "_" + c_channel$ + "_" + frag_start$ + "_" + fixed$(frag_end, 3) + ".wav"
    if samp_freq != new_sample_f
        removeObject: "Sound " + wav_name$ + "_ch" + c_channel$ + "_" + string$(new_sample_f)
    endif
    removeObject: "Sound " + wav_name$ + "_ch" + c_channel$
    removeObject: "Sound " + wav_name$

    label END
endfor
