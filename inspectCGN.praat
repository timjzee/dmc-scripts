form Give chunks
    word chunk_path /Volumes/tensusers/timzee/cgn/eval_genitives.csv
    word cgn_path /Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-
    word kaldi_path /Volumes/tensusers/timzee/cgn/kaldi_annot/comp-
    word output_path /Volumes/tensusers/timzee/cgn/man_annot/
endform

# Make sure input file has a header
Read Table from comma-separated file: chunk_path$
Rename: "chunks"

procedure inspectChunk: annotateChunk.id
    selectObject: "Table chunks"
    filepath$ = Get value: annotateChunk.id, "wav"
    Read from file: output_path$ + "comp-" + filepath$ + ".awd"
    s_name$ = selected$("TextGrid")
    selectObject: "Table chunks"
    w_ort$ = Get value: annotateChunk.id, "word_ort"
    c_start = Get value: annotateChunk.id, "chunk_start"
    c_end = Get value: annotateChunk.id, "chunk_end"
    c_tier = Get value: annotateChunk.id, "tier"
    c_speaker$ = Get value: annotateChunk.id, "speaker"
    word_chunk_i = Get value: annotateChunk.id, "word_chunk_i"
    tier = c_tier * 4 - 3
    selectObject: "TextGrid " + s_name$
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

    Extract one tier: tier
    Rename: "words"
    selectObject: "TextGrid " + s_name$
    Extract one tier: tier + 3
    Rename: "tim"
    Set tier name: 1, "tim"

    Read from file: kaldi_path$ + filepath$ + ".awd"
    Rename: "kaldi_all"
    Extract one tier: tier + 3
    Rename: "kaldi"
    Set tier name: 1, "kaldi"
    removeObject: "TextGrid kaldi_all"
    selectObject: "TextGrid words"
    plusObject: "TextGrid tim"
    plusObject: "TextGrid kaldi"
    Merge
    Rename: s_name$ + "_merged"
    removeObject: "TextGrid words"
    removeObject: "TextGrid tim"
    removeObject: "TextGrid kaldi"

    Open long sound file: cgn_path$ + filepath$ + ".wav"
    plusObject: "TextGrid " + s_name$ + "_merged"
    View & Edit
    editor: "TextGrid " + s_name$ + "_merged"
        Zoom: w_start, w_end
        Zoom out
    endeditor
    beginPause: "Save and continue"
        comment: "Click continue to go to next chunk."
    endPause: "Continue", 1
    removeObject: "TextGrid " + s_name$
    removeObject: "LongSound " + s_name$
    removeObject: "TextGrid " + s_name$ + "_merged"
endproc


num_chunks = Get number of rows
for id from 1 to num_chunks
    selectObject: "Table chunks"
    @inspectChunk: id
endfor
