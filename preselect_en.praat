Text writing preferences: "UTF-8"
tensusers$ = "/Volumes/tensusers"
bigdata2$ = "/Volumes/bigdata2"

form Choose component
    choice Component: 1
        button a
        button c
        button d
        button k
        button o
endform

chunk_path$ = tensusers$ + "/timzee/classifier_evaluation/en/nn_eval_" + component$ + "_en_preselect3.csv"
output_path$ = tensusers$ + "/timzee/classifier_evaluation/en/"


# Make sure input file has a header
Read Table from comma-separated file: chunk_path$
Rename: "chunks"
num_columns = Get number of columns
final_col_name$ = Get column label: num_columns
if final_col_name$ != "notes"
    Append column: "suffix_variant"
    Append column: "notes"
endif


beginPause: "Options"
    optionMenu: "Annotation mode", 1
        option: "One chunk"
        option: "All chunks"
        option: "From last chunk"
    comment: "If one chunk:"
    natural: "Chunk index", 1
endPause: "Continue", 1

procedure annotateChunk: .id
    selectObject: "Table chunks"
    filepath$ = Get value: .id, "wav"
    w_ort$ = Get value: .id, "word_ort"
    ort$ = Get value: .id, "ort"
    chan = Get value: .id, "chan"
    chan$ = Get value: .id, "chan"
    c_start = Get value: .id, "chunk_start"
    c_start$ = Get value: .id, "chunk_start"
    c_end = Get value: .id, "chunk_end"
    c_end$ = Get value: .id, "chunk_end"
    c_tier = Get value: .id, "tier"
    c_tier$ = Get value: .id, "tier"
    c_speaker$ = Get value: .id, "speaker"
    word_chunk_i = Get value: .id, "word_chunk_i"
    word_chunk_i$ = Get value: .id, "word_chunk_i"
    cgn_tran$ = Get value: .id, "cgn_tran"
    cgn_start = Get value: .id, "cgn_start"
    cgn_end = Get value: .id, "cgn_end"
    preselect_type$ = Get value: .id, "preselect_type"
    name_length = length(filepath$)
    if left$(filepath$, 1) == "p"
        pair_length = name_length - 10
        pair_folder$ = "PP" + mid$(filepath$, 3, pair_length)
        Open long sound file: tensusers$ + "/timzee/ECSD/Speech/" + pair_folder$ + "/" + filepath$ + "_S.wav"
        Rename: filepath$
        corpus$ = "ecsd"
    elsif left$(filepath$, 1) == "D"
        Open long sound file: tensusers$ + "/timzee/IFADVcorpus/Speech/" + filepath$ + ".wav"
        corpus$ = "ifadv"
    else
        Open long sound file: bigdata2$ + "/corpora2/CGN2/data/audio/wav/comp-" + filepath$ + ".wav"
        corpus$ = left$(filepath$, 1)
    endif
    s_name$ = selected$("LongSound")
    sound_dur = Get total duration
    buffer = 0.3
    if c_start - buffer < 0
        c_start_buf = 0
    else
        c_start_buf = c_start - buffer
    endif
    if c_end + buffer > sound_dur
        c_end_buf = sound_dur
    else
        c_end_buf = c_end + buffer
    endif
    Extract part: c_start_buf, c_end_buf, "yes"
    if corpus$ != "c" and corpus$ != "d"
        Extract one channel: chan
        removeObject: "Sound " + s_name$
        selectObject: "Sound " + s_name$ + "_ch" + chan$
        Rename: s_name$
    endif
    removeObject: "LongSound " + s_name$
    To TextGrid: "orthography cgn-transcription", ""
    Set interval text: 1, 1, ort$
    Insert boundary: 2, cgn_start + c_start
    Insert boundary: 2, cgn_end + c_start
    Set interval text: 2, 2, cgn_tran$

    plusObject: "Sound " + s_name$
    View & Edit
    editor: "TextGrid " + s_name$
        # 0.01 for narrowband
        Spectrogram settings: 0, 5000, 0.005, 70
        editor_info$ = Editor info
        pitch_enabled = extractNumber(editor_info$, "Pitch show:")
        spectrogram_enabled = extractNumber(editor_info$, "Spectrogram show:")
        intensity_enabled = extractNumber(editor_info$, "Intensity show:")
        formants_enabled = extractNumber(editor_info$, "Formant show:")
        pulses_enabled = extractNumber(editor_info$, "Pulses show:")
        if pitch_enabled == 1
            Show pitch
        endif
        if spectrogram_enabled == 0
            Show spectrogram
        endif
        if intensity_enabled == 1
            Show intensity
        endif
        if formants_enabled == 1
            Show formants
        endif
        if pulses_enabled == 1
            Show pulses
        endif
    endeditor

    preselect_array["@n"] = 1
    preselect_array["@"] = 2
    preselect_array["n"] = 3
    preselect_array["0"] = 4
    preselect_array["~"] = 5
    preselect_array[""] = 6

    variant_array$[1] = "@n"
    variant_array$[2] = "@"
    variant_array$[3] = "n"
    variant_array$[4] = "0"
    variant_array$[5] = "~"
    variant_array$[6] = ""
    beginPause: "Save and continue"
        comment: "Currently on fragment number " + string$(.id)
        comment: "Annotate word number " + string$(word_chunk_i) + ": " + w_ort$
        choice: "Suffix variant", preselect_array[preselect_type$]
            option: "@n"
            option: "@"
            option: "n"
            option: "0"
            option: "~"
            option: "exclude"
        comment: "Enter any notes below:"
        text: "Notes", ""
        comment: "Click continue to save the annotation."
    endPause: "Continue", 1
    suffix_variant$ = variant_array$[suffix_variant]

    selectObject: "Table chunks"
    Set string value: .id, "suffix_variant", suffix_variant$
    Set string value: .id, "notes", notes$

    removeObject: "TextGrid " + s_name$
    removeObject: "Sound " + s_name$
endproc

selectObject: "Table chunks"

if annotation_mode == 1
    @annotateChunk: chunk_index
    selectObject: "Table chunks"
    Save as comma-separated file: chunk_path$
elsif annotation_mode == 2
    num_chunks = Get number of rows
    Create Table with column names: "last_chunk_" + component$, 1, "chunk_id"
    for id from 1 to num_chunks
        selectObject: "Table chunks"
        @annotateChunk: id
        selectObject: "Table chunks"
        Save as comma-separated file: chunk_path$
        selectObject: "Table last_chunk_" + component$
        Set numeric value: 1, "chunk_id", id
        Save as tab-separated file: output_path$ + "last_chunk_" + component$ + ".log"
    endfor
else
    Read Table from tab-separated file: output_path$ + "last_chunk_" + component$ + ".log"
    last_chunk = Get number of rows
    last_chunk_id = Get value: last_chunk, "chunk_id"
    last_chunk_id = last_chunk_id + 1
    Append row
    selectObject: "Table chunks"
    num_chunks = Get number of rows
    for id from last_chunk_id to num_chunks
        selectObject: "Table chunks"
        @annotateChunk: id
        selectObject: "Table chunks"
        Save as comma-separated file: chunk_path$
        selectObject: "Table last_chunk_" + component$
        Set numeric value: last_chunk + 1, "chunk_id", id
        Save as tab-separated file: output_path$ + "last_chunk_" + component$ + ".log"
    endfor
endif
