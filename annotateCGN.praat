form Give chunks
    word chunk_path /Volumes/tensusers/timzee/cgn/fa_eval_a.csv
    word cgn_path /Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-
    word kaldi_path /Volumes/tensusers/timzee/cgn/kaldi_annot/v2/comp-
    word output_path /Volumes/tensusers/timzee/cgn/man_annot/comp-
    word log_path /Volumes/tensusers/timzee/cgn/man_annot/
endform

# Make sure input file has a header
Read Table from comma-separated file: chunk_path$
Rename: "chunks"

beginPause: "Options"
    optionMenu: "Annotation mode", 1
        option: "One chunk"
        option: "All chunks"
        option: "From last chunk"
    comment: "If one chunk:"
    natural: "Chunk index", 1
endPause: "Continue", 1

procedure annotateChunk: annotateChunk.id
    selectObject: "Table chunks"
    filepath$ = Get value: annotateChunk.id, "wav"
    name_length = length(filepath$)
    if fileReadable(output_path$ + filepath$ + ".awd") == 0
        if left$(filepath$, 1) == "p"
            pair_length = name_length - 10
            pair_folder$ = "PP" + mid$(filepath$, 3, pair_length)
            Read from file: "/Volumes/tensusers/timzee/ECSD/kaldi_annot/v2/" + pair_folder$ + "/" + filepath$ + ".awd"
        else
            Read from file: kaldi_path$ + filepath$ + ".awd"
        endif
    else
        Read from file: output_path$ + filepath$ + ".awd"
    endif
    s_name$ = selected$("TextGrid")
    selectObject: "Table chunks"
    w_ort$ = Get value: annotateChunk.id, "word_ort"
    c_start = Get value: annotateChunk.id, "chunk_start"
    c_end = Get value: annotateChunk.id, "chunk_end"
    c_tier = Get value: annotateChunk.id, "tier"
    c_speaker$ = Get value: annotateChunk.id, "speaker"
    word_chunk_i = Get value: annotateChunk.id, "word_chunk_i"
#    tier = c_tier * 4 - 3
    selectObject: "TextGrid " + s_name$
    tier_name$ = ""
    tier = 0
    while tier_name$ != c_speaker$
        tier += 1
        tier_name$ = Get tier name: tier
    endwhile
    speaker$ = Get tier name: tier
    assert c_speaker$ == speaker$
    word_int = Get high interval at time: tier, c_start
    word_int -= 1
    word_counter = 0
    while word_counter != word_chunk_i
        word_int += 1
        int_lab$ = Get label of interval: tier, word_int
        if int_lab$ != ""
            word_counter += 1
        endif
    endwhile
    int_lab$ = replace_regex$(int_lab$, "[.?! ]", "", 0)
    appendInfoLine: "Asserting '" + int_lab$ + "' == '" + w_ort$ + "'"
    assert int_lab$ == w_ort$
    w_start = Get start time of interval: tier, word_int
    w_end = Get end time of interval: tier, word_int

    if left$(filepath$, 1) == "p"
        pair_length = name_length - 10
        pair_folder$ = "PP" + mid$(filepath$, 3, pair_length)
        Open long sound file: "/Volumes/tensusers/timzee/ECSD/Speech/" + pair_folder$ + "/" + filepath$ + "_S.wav"
        Rename: filepath$
    else
        Open long sound file: cgn_path$ + filepath$ + ".wav"
    endif
    plusObject: "TextGrid " + s_name$
    View & Edit
    editor: "TextGrid " + s_name$
        Zoom: w_start, w_end
        Zoom out
    endeditor
    beginPause: "Save and continue"
        comment: "Annotate " + w_ort$ + " on tier " + string$(tier)
        comment: "Click continue to save the annotation."
    endPause: "Continue", 1
    selectObject: "TextGrid " + s_name$
    Save as text file: output_path$ + filepath$ + ".awd"
    Remove
    selectObject: "LongSound " + s_name$
    Remove
endproc

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
        Save as tab-separated file: output_path$ + "last_chunk.log"
    endfor
else
    Read Table from tab-separated file: output_path$ + "last_chunk.log"
    last_chunk = Get number of rows
    last_chunk_id = Get value: last_chunk, "chunk_id"
    Append row
    selectObject: "Table chunks"
    num_chunks = Get number of rows
    for id from last_chunk_id to num_chunks
        selectObject: "Table chunks"
        @annotateChunk: id
        selectObject: "Table last_chunk"
        Set numeric value: last_chunk + 1, "chunk_id", id
        Save as tab-separated file: output_path$ + "last_chunk.log"
    endfor
endif
