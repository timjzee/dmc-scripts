form Give chunks
    word user_name TZ
    word tensusers /Volumes/tensusers
    word bigdata2 /Volumes/bigdata2
endform

chunk_path$ = tensusers$ + "/timzee/classifier_evaluation/s/nn_eval20b.csv"
output_path$ = tensusers$ + "/timzee/classifier_evaluation/s/man_annot/"


# Make sure input file has a header
Read Table from comma-separated file: chunk_path$
Rename: "chunks"

# Make sure output folder exists
Create Strings as directory list: "directoryList", output_path$ + "*"
To WordList
user_exists = Has word: user_name$
Remove
removeObject: "Strings directoryList"
if user_exists == 0
    createDirectory: output_path$ + user_name$
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
    c_dur = Get total duration
    equal_interval_dur = c_dur / 5
    To TextGrid: "orthography s-boundaries reduction", ""
    Set interval text: 1, 1, ort$
    for i from 1 to 4
        Insert boundary: 2, c_start_buf + equal_interval_dur * i
    endfor
    Set interval text: 2, 2, "start"
    Set interval text: 2, 3, "max"
    Set interval text: 2, 4, "end"
    plusObject: "Sound " + s_name$
    View & Edit
    editor: "TextGrid " + s_name$
        Spectrogram settings: 0, 8000, 0.005, 70
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
    annotation_incomplete = 1
    while annotation_incomplete == 1
        phone_boundaries_complete = 0
        reduction_complete = 0
        beginPause: "Save and continue"
            comment: "Annotate word number " + string$(word_chunk_i) + ": " + w_ort$
            comment: "Click continue to save the annotation."
        endPause: "Continue", 1
        selectObject: "TextGrid " + s_name$
        num_phon_intervals = Get number of intervals: 2
        if num_phon_intervals == 5 or num_phon_intervals == 4
            labels$ = ""
            for i from 1 to num_phon_intervals
                i_lab$ = Get label of interval: 2, i
                labels$ = labels$ + " " + i_lab$
            endfor
            if num_phon_intervals == 5
                if labels$ == "  start max end "
                    phone_boundaries_complete = 1
                else
                    appendInfoLine: "WARNING: Check labelling"
                endif
            elsif num_phon_intervals == 4
                if labels$ == "  start end "
                    phone_boundaries_complete = 1
                else
                    appendInfoLine: "WARNING: Check labelling"
                endif
            endif
        else
            appendInfoLine: "WARNING: Wrong amount of phone boundaries"
        endif
        num_reduction_intervals = Get number of intervals: 3
        if num_reduction_intervals == 1
            red_lab$ = Get label of interval: 3, 1
            if red_lab$ == "1" or red_lab$ == "2" or red_lab$ == "3" or red_lab$ == "4" or red_lab$ == "D"
                reduction_complete = 1
            else
                appendInfoLine: "WARNING: Wrong Reduction annotation"
            endif
        else
            appendInfoLine: "WARNING: Reduction tier should not contain boundaries"
        endif
        if phone_boundaries_complete == 1 and reduction_complete == 1
            annotation_incomplete = 0
        endif
    endwhile
    # check if corpus folder exists
    Create Strings as directory list: "directoryList", output_path$ + user_name$ + "/*"
    To WordList
    corpus_exists = Has word: corpus$
    Remove
    removeObject: "Strings directoryList"
    if corpus_exists == 0
        createDirectory: output_path$ + user_name$ + "/" + corpus$
    endif
    selectObject: "TextGrid " + s_name$
    Save as text file: output_path$ + user_name$ + "/" + corpus$ + "/" + replace$(filepath$, "/", "_", 0) + "_" + chan$ + "_" + c_start$ + "_" + c_end$ + "_" + c_tier$ + "_" + word_chunk_i$ + ".TextGrid"
    Remove
    removeObject: "Sound " + s_name$
endproc

selectObject: "Table chunks"

if annotation_mode == 1
    @annotateChunk: chunk_index
elsif annotation_mode == 2
    num_chunks = Get number of rows
    Create Table with column names: "last_chunk", 1, "chunk_id"
    for id from 1 to num_chunks
        selectObject: "Table chunks"
        @annotateChunk: id
        selectObject: "Table last_chunk"
        Set numeric value: 1, "chunk_id", id
        Save as tab-separated file: output_path$ + user_name$ + "/last_chunk.log"
    endfor
else
    Read Table from tab-separated file: output_path$ + user_name$ + "/last_chunk.log"
    last_chunk = Get number of rows
    last_chunk_id = Get value: last_chunk, "chunk_id"
    last_chunk_id = last_chunk_id + 1
    Append row
    selectObject: "Table chunks"
    num_chunks = Get number of rows
    for id from last_chunk_id to num_chunks
        selectObject: "Table chunks"
        @annotateChunk: id
        selectObject: "Table last_chunk"
        Set numeric value: last_chunk + 1, "chunk_id", id
        Save as tab-separated file: output_path$ + user_name$ + "/last_chunk.log"
    endfor
endif
