form Give chunks
    word chunk_path /Volumes/tensusers/timzee/ECSD/aardappel_index_ecsd.txt
    word log_path /Volumes/tensusers/timzee/
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
    c_chan = Get value: annotateChunk.id, "chan"
    c_start = Get value: annotateChunk.id, "from"
    c_end = Get value: annotateChunk.id, "to"
    c_tier = Get value: annotateChunk.id, "tier"
    name_length = length(filepath$)
    if left$(filepath$, 1) == "p"
        output_path$ = "/Volumes/tensusers/timzee/ECSD/aardappel_annot/"
        corpus$ = "ecsd"
        pair_length = name_length - 10
        pair_folder$ = "PP" + mid$(filepath$, 3, pair_length)
        Open long sound file: "/Volumes/tensusers/timzee/ECSD/Speech/" + pair_folder$ + "/" + filepath$ + "_S.wav"
        Rename: filepath$
        if fileReadable(output_path$) == 0
            createDirectory: output_path$
        endif
        if fileReadable(output_path$ + filepath$ + ".awd") == 0
            Read from file: "/Volumes/tensusers/timzee/ECSD/kaldi_annot/v2/" + pair_folder$ + "/" + filepath$ + ".awd"
        else
            Read from file: output_path$ + filepath$ + ".awd"
        endif
    elsif left$(filepath$, 1) == "D"
        output_path$ = "/Volumes/tensusers/timzee/IFADVcorpus/aardappel_annot/"
        corpus$ = "ifadv"
        Open long sound file: "/Volumes/tensusers/timzee/IFADVcorpus/Speech/" + filepath$ + ".wav"
        if fileReadable(output_path$) == 0
            createDirectory: output_path$
        endif
        if fileReadable(output_path$ + filepath$ + ".awd") == 0
            Read from file: "/Volumes/tensusers/timzee/IFADVcorpus/kaldi_annot/v2/" + filepath$ + ".awd"
        else
            Read from file: output_path$ + filepath$ + ".awd"
        endif
    else
        corpus$ = left$(filepath$, 1)
        Open long sound file: "/Volumes/bigdata2/corpora2/CGN2/data/audio/wav/comp-" + filepath$ + ".wav"
        output_path$ = "Volumes/tensusers/timzee/cgn/aardappel_annot/comp-"
        if fileReadable(output_path$ + corpus$ + "/nl/") == 0
            createDirectory: output_path$ + corpus$
            createDirectory: output_path$ + corpus$ + "/nl/"
        endif
        if fileReadable(output_path$ + filepath$ + ".awd") == 0
            Read from file: "/Volumes/tensusers/timzee/cgn/kaldi_annot/v2/comp-" + filepath$ + ".awd"
        else
            Read from file: output_path$ + filepath$ + ".awd"
        endif
    endif
    selectObject: "LongSound " + filepath$
    sample_freq = Get sampling frequency
    sound_dur = Get end time
    Create Sound from formula: "start", 1, 0, c_start, sample_freq, "randomGauss(0,0.01)"
    selectObject: "LongSound " + filepath$
    Extract part: c_start, c_end, "yes"
    Extract one channel: c_chan
    removeObject: "Sound " + filepath$
    Rename: filepath$
    Create Sound from formula: "end", 1, c_end, sound_dur, sample_freq, "randomGauss(0,0.01)"
    plusObject: "Sound " + filepath$
    plusObject: "Sound start"
    Concatenate
    removeObject: "Sound start"
    removeObject: "Sound " + filepath$
    removeObject: "Sound end"
    selectObject: "Sound chain"
    plusObject: "TextGrid " + filepath$
    View & Edit
    editor: "TextGrid " + filepath$
        Zoom: c_start, c_end
    endeditor
    beginPause: "Save and continue"
        comment: "Annotate aardappel(s|en) on tier " + string$(c_tier)
        comment: "Click continue to save the annotation."
    endPause: "Continue", 1
    selectObject: "TextGrid " + filepath$
    Save as text file: output_path$ + filepath$ + ".awd"
    Remove
    removeObject: "LongSound " + filepath$
    removeObject: "Sound chain"
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
        Save as tab-separated file: log_path$ + "last_chunk.log"
    endfor
else
    Read Table from tab-separated file: log_path$ + "last_chunk.log"
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
        Save as tab-separated file: log_path$ + "last_chunk.log"
    endfor
endif
