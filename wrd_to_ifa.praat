prop$ = Report system properties
os$ = extractWord$(prop$, newline$)

wrd_path$ = "/bigdata2/corpora2/CGN2/data/annot/text/wrd/comp-"
component$ = "a"

if os$ == "macintosh"
    tens_path$ = "/Volumes/tensusers/timzee/cgn/"
    wrd_tg_path$ = "/Volumes" + wrd_path$
else
    tens_path$ = "/vol/tensusers/timzee/cgn/"
    wrd_tg_path$ = "/vol" + wrd_path$
endif

procedure getSegments: .int_lab$, .int_dur
#    .int_lab$ = replace$(.int_lab$, "=", "", 0)
#    .int_lab$ = replace_regex$(.int_lab$, "-.$", "", 0)
#    .int_lab$ = replace_regex$(.int_lab$, "^.-", "", 0)
    .num_phons = 0
    .num_letters = length(.int_lab$)
    .phon$ = ""
    for .let from 1 to .num_letters
        .let$ = mid$(.int_lab$, .let, 1)
        if .let$ != "+" and .let$ != "~" and .let$ != ":"
            if .phon$ != ""
                .phons$[.num_phons] = .phon$
            endif
            .num_phons += 1
            .phon$ = .let$
        else
            .phon$ = .phon$ + .let$
            .phons$[.num_phons] = .phon$
            .phon$ = ""
        endif
        if .let == .num_letters and .phon$ != ""
            .phons$[.num_phons] = .phon$
        endif
    endfor
    .seg_dur = .int_dur / .num_phons
endproc

index_path$ = tens_path$ + "n_tests/prep_" + component$ + "_core.txt"

Read Table from comma-separated file: index_path$
Rename: "chunks"

old_name$ = ""
old_tier = 0
num_chunks = Get number of rows

for id from 1 to num_chunks
    selectObject: "Table chunks"
    f_path$ = Get value: id, "wav"
    f_name$ = right$(f_path$, length(f_path$) - rindex(f_path$, "/"))
    c_tier$ = Get value: id, "tier"
    c_tier = number(c_tier$)
    c_start$ = Get value: id, "from"
    c_start = number(c_start$)
    c_end$ = Get value: id, "to"
    c_end = number(c_end$)

    if f_name$ != old_name$
        appendInfoLine: "Line ", id, " File ", f_name$
        ## Remove old files
        if id > 1
            removeObject: "TextGrid " + old_name$ + "_wrd"
        endif
        ## Load TextGrids
        runSystem_nocheck: "cp -f " + wrd_tg_path$ + f_path$ + ".wrd.gz " + tens_path$
        runSystem_nocheck: "gunzip -f " + tens_path$ + f_name$ + ".wrd.gz"
        Read from file: tens_path$ + f_name$ + ".wrd"
        runSystem_nocheck: "rm -f " + tens_path$ + f_name$ + ".wrd"
        Rename: f_name$ + "_wrd"
        old_tier = 0
        old_name$ = f_name$
    endif
    selectObject: "TextGrid " + f_name$ + "_wrd"
    Extract one tier: c_tier * 2
    Rename: "tier"
    Extract part: c_start, c_end, "no"
    Rename: "chunk"
    removeObject: "TextGrid tier"
    num_ints = Get number of intervals: 1
    # handle '=' and '-'
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
        int_lab$ = replace_regex$(int_lab$, "\[\]", "\*", 0)
        int_lab$ = replace_regex$(int_lab$, "#", "\*", 0)
        Set interval text: 1, int, int_lab$
    endfor
    # handle '_'
    for int from 1 to num_ints
        int_lab$ = Get label of interval: 1, int
        int_start = Get start time of interval: 1, 1
        int_end = Get end time of interval: 1, int
        if int_lab$ == "_"
            prev_lab$ = Get label of interval: 1, int - 1
            prev_start = Get start time of interval: 1, int - 1
            prev_end = Get end time of interval: 1, int - 1
            prev_dur = prev_end - prev_start
            @getSegments: prev_lab$, prev_dur
            prev_n = getSegments.num_phons
            for i from 1 to prev_n
                prev_phons$[i] = getSegments.phons$[i]
            endfor
            next_lab$ = Get label of interval: 1, int + 1
            next_start = Get start time of interval: 1, int + 1
            next_end = Get end time of interval: 1, int + 1
            next_dur = next_end - next_start
            @getSegments: next_lab$, next_dur
            next_n = getSegments.num_phons
            for i from 1 to next_n
                next_phons$[i] = getSegments.phons$[i]
            endfor
            shared_n = 0
            pre_segs$ = ""
            post_segs$ = "bla"
            while pre_segs$ != post_segs$
                shared_n += 1
                pre_segs$ = ""
                for i from prev_n - (shared_n - 1) to prev_n
                    pre_segs$ = pre_segs$ + prev_phons$[i]
                endfor
                post_segs$ = ""
                for i from 1 to shared_n
                    post_segs$ = post_segs$ + next_phons$[i]
                endfor
#                appendInfoLine: pre_segs$, " ", post_segs$, " ", f_path$, " ", c_start$, " ", c_end$
            endwhile
            Set interval text: 1, int - 1, left$(prev_lab$, length(prev_lab$) - length(pre_segs$))
            Set interval text: 1, int, pre_segs$
            Set interval text: 1, int + 1, right$(next_lab$, length(next_lab$) - length(post_segs$))
        endif
    endfor
    # make segments
    Insert interval tier: 2, "PHONEMES"
    int_start = Get start time of interval: 1, 1
    for int from 1 to num_ints
        int_lab$ = Get label of interval: 1, int
        int_end = Get end time of interval: 1, int
        int_dur = int_end - int_start
        if int_lab$ == "" and int != num_ints
            Insert boundary: 2, int_end
        else
            @getSegments: int_lab$, int_dur
            num_phons = getSegments.num_phons
            for i from 1 to num_phons
                phons$[i] = getSegments.phons$[i]
            endfor
            seg_dur = getSegments.seg_dur
            seg_start = int_start
#            Insert boundary: 2, seg_start
            for phon from 1 to num_phons
                phon$ = phons$[phon]
                seg_end = seg_start + seg_dur
                phon_int = Get number of intervals: 2
                Set interval text: 2, phon_int, phon$
                if not (phon == num_phons and int == num_ints)
                    Insert boundary: 2, seg_end
                endif
                seg_start = seg_end
            endfor
        endif
        int_start = int_end
    endfor
    Extract one tier: 2
    removeObject: "TextGrid chunk"
    # now make directory structure as necessary and save file
    Create Strings as directory list: "directoryList", tens_path$ + "/n_tests/validation/" + component$ + "/*"
    To WordList
    exists = Has word: f_name$
    Remove
    removeObject: "Strings directoryList"
    if exists != 1
        runSystem_nocheck: "mkdir " + tens_path$ + "/n_tests/validation/" + component$ + "/" + f_name$
    endif
    Create Strings as directory list: "directoryList", tens_path$ + "/n_tests/validation/" + component$ + "/" + f_name$ + "/*"
    To WordList
    exists = Has word: c_tier$
    Remove
    removeObject: "Strings directoryList"
    if exists != 1
        runSystem_nocheck: "mkdir " + tens_path$ + "/n_tests/validation/" + component$ + "/" + f_name$ + "/" + c_tier$
    endif
    selectObject: "TextGrid PHONEMES"
    Save as short text file: tens_path$ + "/n_tests/validation/" + component$ + "/" + f_name$ + "/" + c_tier$ + "/" + c_start$ + "-" + c_end$ + "_CGN.aspex"
    Remove
endfor
