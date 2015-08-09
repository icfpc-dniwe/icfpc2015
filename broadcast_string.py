#!/usr/bin/env python

import requests
from requests.auth import HTTPBasicAuth
import json
import sys
import time

TOKEN = "FaMnv73T2LUfm2Q83f3V0DGkslIyAxs8qouqJ5lanQk="
TEAM = 180
URI = "https://davar.icfpcontest.org/teams/{team}/solutions".format(team = TEAM)

SEED = 18
NUM = 24

phrases = [
#	  "ei!"
#	, "ia!"
#	, "r'lyeh"
#	, "yuggoth"
	"deep seven"
	, "chtonians"
#	, "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn!"
	, "bigboote"
	, "tsathoggua"
	, "unnamable"
	, "yith"
	, "great race of yith"
	, "celeano"
	, "great hall of celeano"
	, "plateau of leng"
	, "abyss"
	, "lost carcosa"
	, "unknown kadath"
	, "the dreamlands"
	, "the underworld"
	, "dreamlands"
	, "underworlds"
]

def test_words(n, seed, words):
	import pdb
	pdb.set_trace()
	for idx, word in enumerate(words):
		resp = requests.post(URI,\
			headers = {'Content-type': 'application/json'},\
			data = json.dumps([{'problemId': n, 'seed': seed, 'tag': "T".format(idx), 'solution': word}]),\
			auth = HTTPBasicAuth('', TOKEN)
		)
		if resp.status_code == 200:
			print("Tag {0} -- OK".format(idx))
		else:
			print("Tag {0} -- Fail".format(idx))
			break
		#time.sleep(300)

if len(sys.argv) != 3:
	print("Usage: {0} seed problem_id".format(sys.argv[0]))
else:
	test_words(sys.argv[2], sys.argv[1], phrases)