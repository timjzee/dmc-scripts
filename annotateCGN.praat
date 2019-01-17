form Give chunks
    word chunk_path /vol/tensusers/timzee/cgn/chunks3h.txt
    word cgn_path /vol/bigdata/corpora2/CGN2/data/audio/wav/comp-
    word output_path /vol/tensusers/timzee/cgn/man_annot/
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
    annotateChunk.f_path$ = Get value: annotateChunk.id, "filepath"
    annotateChunk.c_start$ = Get value: annotateChunk.id, "chunk_start"
    annotateChunk.c_start = number(annotateChunk.c_start$)
    annotateChunk.c_end$ = Get value: annotateChunk.id, "chunk_end"
    annotateChunk.c_end = number(annotateChunk.c_end$)
    annotateChunk.c_ort$ = Get value: annotateChunk.id, "chunk_ort"
    Open long sound file: cgn_path$ + annotateChunk.f_path$ + ".wav"
    annotateChunk.s_name$ = selected$("LongSound")
    To TextGrid: "phones words orth", ""
    Insert boundary: 3, annotateChunk.c_start
    Insert boundary: 3, annotateChunk.c_end
    Set interval text: 3, 2, annotateChunk.c_ort$
    plusObject: "LongSound " + annotateChunk.s_name$
    View & Edit
    editor: "TextGrid " + annotateChunk.s_name$
        Select: annotateChunk.c_start, annotateChunk.c_end
        Zoom to selection
    endeditor
    beginPause: "Save and continue"
        comment: "Click continue to save the annotation."
    endPause: "Continue", 1
    selectObject: "TextGrid " + annotateChunk.s_name$
    annotateChunk.n_path$ = replace$(annotateChunk.f_path$, "/", "_", 0)
    Save as text file: output_path$ + annotateChunk.n_path$ + "_" + annotateChunk.c_start$ + "_" + annotateChunk.c_end$ + ".TextGrid"
    Remove
    selectObject: "LongSound " + annotateChunk.s_name$
    Remove
endproc

if annotation_mode == 1
    @annotateChunk: chunk_index
elsif annotation_mode == 2
    num_chunks = Get number of rows
    for id from 1 to num_chunks
        selectObject: "Table chunks"
        @annotateChunk: id
    endfor
else
    num_chunks = Get number of rows
    for id from 1 to num_chunks
        selectObject: "Table chunks"
        filepath$ = Get value: id, "filepath"
        namepath$ = replace$(filepath$, "/", "_", 0)
        chunkfrom$ = Get value: id, "chunk_start"
        chunkto$ = Get value: id, "chunk_end"
        tg_name$ = namepath$ + "_" + chunkfrom$ + "_" + chunkto$ + ".TextGrid"
        if fileReadable(output_path$ + tg_name$) == 0
            @annotateChunk: id
        endif
    endfor
endif

