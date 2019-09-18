import sys
import re

n_gen = 3

if len(sys.argv) > 1:
    n_gen = int(sys.argv[1])

# features
liquid = ["r", "l"]
nasal = ["m", "n", "N"]
Vfricative = ["G", "v", "z", "Z"]
Ufricative = ["x", "f", "s", "S"]
Vplosive = ["b", "d", "g"]
Uplosive = ["p", "t", "k"]
semivowel = ["j", "w"]
consonant = liquid + nasal + Uplosive + Vplosive + Ufricative + Vfricative + semivowel
monophthong = ["a", "e", "i", "o", "u", "y", "A", "E", "I", "O", "U", "EU", "E2"]
diphthong = ["EI", "UI", "AU"]
vowel = monophthong + diphthong
vowel_lb = [v[-1] for v in vowel]  # look-behind requires fixed length

target = {}
rewrite = {}
insert = {}
pattern = {}

# ------------------- phonological rules from the literature
# rule 0 schwa insertion after liquid before a final consonant other than /n/, /t/ or /d/
# but kErn --> kEr@n seems fine; and pars --> par@s seems wrong
target[0] = nasal + ["p", "k", "b", "g", "x", "f", "G", "v"]
insert[0] = ["@"]
pattern[0] = re.compile('(?<=(r|l) )[{}]'.format("".join(target[0])))

# word final n deletion after schwa
target[1] = ["n"]
rewrite[1] = [""]
pattern[1] = re.compile('(?<=@ )n $')

# RVA of obstruents before voiced plosives
target[2] = Uplosive + Ufricative
rewrite[2] = Vplosive + Vfricative
pattern[2] = re.compile('(?<= )(' + '|'.join(target[2]) + ')(?= (' + '|'.join(Vplosive) + ') )')

# devoicing of plosives following voiceless plosives
target[3] = Vplosive
rewrite[3] = Uplosive
pattern[3] = re.compile('(?<= (' + '|'.join(Uplosive) + ') )(' + '|'.join(target[3]) + ')')

# devoicing of fricatives in all word positions
target[4] = Vfricative
rewrite[4] = Ufricative
pattern[4] = re.compile('(?<= )[{}](?= )'.format("".join(target[4])))

# ----------------- coarticulation
# voicing of intervocalic obstruents
target[5] = Uplosive + Ufricative
rewrite[5] = Vplosive + Vfricative
pattern[5] = re.compile('(?<=(' + '|'.join(vowel_lb) + ') )(' + '|'.join(target[5]) + ')(?= (' + '|'.join(vowel) + ') )')

# devoicing of obstruents in obstruent clusters
# fricatives handled above
# PVA of plosives also handled above
# potentially pre context should be removed, but that would create opposite rule to RVA
target[6] = Vplosive
rewrite[6] = Uplosive
pattern[6] = re.compile('(?<= (' + '|'.join(Uplosive + Vplosive) + ') )(' + '|'.join(target[6]) + ')(?= (' + '|'.join(Uplosive + Vplosive) + ') )')

# -------------------- lenitions
# word-initial /b/ pronounced as [m]
target[7] = ["b"]
rewrite[7] = ["m"]
pattern[7] = re.compile('(?<=^ )b')

# long vowels produced as short
target[8] = ["a", "e", "i", "o", "y"]
rewrite[8] = ["A", "E", "I", "O", "U"]
pattern[8] = re.compile('(?<= )(' + '|'.join(target[8]) + ')(?= )')

# vowel produced as schwa
target[9] = vowel
rewrite[9] = ["@"]
pattern[9] = re.compile('(?<= )(' + "|".join(target[9]) + ')(?= )')

# ------------------------- Table 3 Schuppler et al 2011
# n-deletion between vowel and s
target[10] = ["n"]
rewrite[10] = [""]
pattern[10] = re.compile('(?<=(' + '|'.join(vowel_lb) + ') )n (?=s )')

# deletion of bilabial plosives after m
target[11] = ["p", "b"]
rewrite[11] = [""]
pattern[11] = re.compile('(?<= m )(p|b) ')

# k deletion after N
target[12] = ["k"]
rewrite[12] = [""]
pattern[12] = re.compile('(?<= N )k ')

# s deletion after n
target[13] = ["s"]
rewrite[13] = [""]
pattern[13] = re.compile('(?<= n )s ')

# ------------ r deletion
# changed pre and post context to be in line with van den Heuvel & Cucchiarini, 2001
target[14] = ["r"]
rewrite[14] = [""]
pattern[14] = re.compile('(?<=(' + '|'.join(vowel_lb + ["@"]) + ') )r (?=(' + '|'.join(consonant) + ') )')

# ----------- t deletion
# between s and consonant
target[15] = ["t"]
rewrite[15] = [""]
pattern[15] = re.compile('(?<= s )t (?=(' + '|'.join(consonant) + ') )')

# between consonant and plosive
target[16] = ["t"]
rewrite[16] = [""]
pattern[16] = re.compile('(?<= (' + '|'.join(consonant) + ') )t (?=(' + '|'.join(Uplosive + Vplosive) + ') )')

# word final and preceded by consonant
target[17] = ["t"]
rewrite[17] = [""]
pattern[17] = re.compile('(?<= (' + '|'.join(consonant) + ') )t $')

# between vowel and plosive
target[18] = ["t"]
rewrite[18] = [""]
pattern[18] = re.compile('(?<=(' + '|'.join(vowel_lb) + ') )t (?=(' + '|'.join(Uplosive + Vplosive) + ') )')

# ---------------------- word specific: l@k -> @k or k
# added postpost array
target[19] = ["l"]
rewrite[19] = [""]
pattern[19] = re.compile('l (?=@ k )')

# absence of /h/ in heb, hebben, het
target[20] = ["h"]
rewrite[20] = [""]
pattern[20] = re.compile('(?<=^ )h (?=E (' + '|'.join(["b", "p", "t"]) + ') )')

# absence of /x/ in nog, toch
target[21] = ["x"]
rewrite[21] = [""]
pattern[21] = re.compile('(?<= O )x $')

# lenition of /d/ in gereden, verleden, deden
target[22] = ["d"]
rewrite[22] = ["j"]
pattern[22] = re.compile('(?<= e )d(?= @ )')

# ----------------------- vowel and schwa deletion
# simplified
target[23] = ["@"]
rewrite[23] = [""]
pattern[23] = re.compile('@ ')

# ---------------------- final devoicing
# voiced fricatives already handled above
target[24] = Vplosive
rewrite[24] = Uplosive
pattern[24] = re.compile('(?<= )(' + '|'.join(Vplosive) + ')(?= )')

# --------------------- place assimilations

# alveolar nasals to velar or alveolar

# alveolar fricatives to palatal


def rewrite_rule(rulenumber, occ, m_ls, res):
    global out_list
    occ -= 1  # go from back to front because otherwise indices would change if segment deleted
    match_ob = m_ls[occ]
    if len(rewrite[rulenumber]) > 1:
        rewr_i = target[rulenumber].index(match_ob.group())
    else:
        rewr_i = 0
    rewr = rewrite[rulenumber][rewr_i]
    apply_res = res[:match_ob.start()] + rewr + res[match_ob.end():]
    out_list.append(apply_res[1:-1])  # remove leading and trailing space
    if occ > 0:
        rewrite_rule(rulenumber, occ, m_ls, apply_res)
        rewrite_rule(rulenumber, occ, m_ls, res)


def insert_rule(rulenumber, occ, m_ls, res):
    global out_list
    occ -= 1
    match_ob = m_ls[occ]
    ins = insert[rulenumber][0]
    apply_res = res[:match_ob.start()] + ins + " " + res[match_ob.start():]
    out_list.append(apply_res)
    if occ > 0:
        insert_rule(rulenumber, occ, m_ls, apply_res)
        insert_rule(rulenumber, occ, m_ls, res)


def apply_rule(rulenumber, instr):
    global out_list
    if rulenumber in rewrite:
        instr = " " + instr + " "
        m_ls = list(pattern[rulenumber].finditer(instr))
        occ = len(m_ls)
        out_list = []
        if occ > 0:
            rewrite_rule(rulenumber, occ, m_ls, instr)
    elif rulenumber in insert:
        m_ls = list(pattern[rulenumber].finditer(instr))
        occ = len(m_ls)
        out_list = []
        if occ > 0:
            insert_rule(rulenumber, occ, m_ls, instr)
    return out_list


def create_gen(gen, tran):
    if gen <= n_gen:
        output = []
        for rulenumber in range(25):
            outp = apply_rule(rulenumber, tran)
            for o in outp:
                print("[-{}- gen] {} {}".format(gen, ortho, o))
            output.extend(outp)
            for i in output:
                create_gen(gen + 1, i)
    return


for input in sys.stdin:
    ortho, pron = input[:-1].split("\t")
    print("[-0- gen] {} {}".format(ortho, pron))
    create_gen(1, pron)
