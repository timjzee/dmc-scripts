import os
import sys

os.chdir("/Volumes/tensusers/mbentum/BAK/") if sys.platform == "darwin" else os.chdir("/vol/tensusers/mbentum/BAK/")

import load_eeg     # noqa: E402
import read_xml     # noqa: E402

pp = read_xml.load_participant(pp_id="1", add_words=True)
load_eeg.load_word_epochs_participant(pp)
print(pp.blocks)

o_bid1 = pp.blocks[0]

# some blocks correspond to multiple sound files, check:
o_bid1.fids     # for file names
o_bid1.fid_st   # for starting sample ids in eeg data (sampling frequency is 1000 Hz)



load_eeg.make_eeg_word_epochs(o_bid1)   # we've already loaded all eeg data above, use this to load eeg data for a specific block

# words st and et are based on forced alignments in CGN and IFADV
word1 = o_bid1.words[0]

word1_data = o_bid1.data[:, word1.st_sample:word1.et_sample]
