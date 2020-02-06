corpus$ = "cgn"

if corpus$ == "cgn"
    component$ = "k"
endif

chunk_path$ = "/vol/tensusers/timzee/" + corpus$ + "/"
wrd_path$ = "/vol/bigdata2/corpora2/CGN2/data/annot/text/wrd/comp-"
tens_path$ = chunk_path$ + "cgn_annot/"

Read Table from comma-separated file: chunk_path$ + "comp-" + component$ + "_en_core.csv"
Rename: "chunks"

writeFileLine: chunk_path$ + "comp-" + component$ + "_en_eval.csv", "wav,chan,chunk_start,chunk_end,tier,word_chunk_i,word_ort,speaker,word_phon"

old_name$ = ""
num_chunks = Get number of rows

for id from 1 to num_chunks
    selectObject: "Table chunks"
    f_path$ = Get value: id, "wav"
    f_name$ = right$(f_path$, length(f_path$) - rindex(f_path$, "/"))
    c_tier$ = Get value: id, "tier"
    c_tier = number(c_tier$)
    c_start$ = Get value: id, "chunk_start"
    c_start = number(c_start$)
    c_end$ = Get value: id, "chunk_end"
    c_end = number(c_end$)
    c_chan = Get value: id, "chan"
    w_in_c = Get value: id, "word_chunk_i"
    w_ort$ = Get value: id, "word_ort"
    speaker$ = Get value: id, "speaker"

    if f_name$ != old_name$
        appendInfoLine: "Line ", id, " File ", f_name$
        ## Remove old files
        if id > 1
            removeObject: "TextGrid " + old_name$ + "_wrd"
        endif
        ## Load TextGrids
        runSystem_nocheck: "cp -f " + wrd_path$ + f_path$ + ".wrd.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + f_name$ + ".wrd.gz"
        Read from file: tens_path$ + f_name$ + ".wrd"
        runSystem_nocheck: "rm -f " + tens_path$ + f_name$ + ".wrd"
        Rename: f_name$ + "_wrd"
        old_name$ = f_name$
    endif
    # exclude een z'n m'n
    if w_ort$ == "een" or w_ort$ == "z'n" or w_ort$ == "m'n"
        goto NEXT
    endif
    selectObject: "TextGrid " + f_name$ + "_wrd"
    Extract one tier: c_tier * 2
    Rename: "tier"
    Extract part: c_start, c_end, "no"
    Rename: "chunk"
    removeObject: "TextGrid tier"
    num_ints = Get number of intervals: 1
    # handle '=' and '-'
    word_num = 0
    for int from 1 to num_ints
        int_lab$ = Get label of interval: 1, int
        int_lab$ = replace$(int_lab$, "=", "", 0)
        if int < num_ints
            next_lab$ = Get label of interval: 1, int + 1
            if index(next_lab$, "-")
                int_lab$ = replace_regex$(int_lab$, "-.$", "", 0)
            endif
        endif
        int_lab$ = replace_regex$(int_lab$, "^.-", "", 0)
        int_lab$ = replace_regex$(int_lab$, "#", "\[SPN\]", 0)
        if int_lab$ != "" and int_lab$ != "_" and int_lab$ != " "
            word_num += 1
            if word_num == w_in_c
                goto FOUNDIT
            endif
        endif
    endfor
    int_lab$ = "NA"
    label FOUNDIT
    removeObject: "TextGrid chunk"
    # exclude words annotated without n
    if right$(int_lab$, 1) != "n"
        goto NEXT
    endif
    appendFileLine: chunk_path$ + "comp-" + component$ + "_en_eval.csv", f_path$ + "," + string$(c_chan) + "," + c_start$ + "," + c_end$ + "," + c_tier$ + "," + string$(w_in_c) + "," + w_ort$ + "," + speaker$ + "," + int_lab$
    label NEXT
endfor
