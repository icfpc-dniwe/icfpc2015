# -*- coding: utf-8 -*-

import json
from common.constants import Move, Rotate

# IDEA power phrase search

def _fmt_commands(commands):
    def process(x):
        if x == Move.W:
            return 'p'
        elif x == Move.E:
            return 'b'
        elif x == Move.SW:
            return 'a'
        elif x == Move.SE:
            return 'l'
        elif x == Rotate.CW:
            return 'd'
        elif x == Rotate.CCW:
            return 'k'
    print(commands)
    return ''.join(list(map(process, commands)))
            

def output_solution(problem_id, seed, commands, tag=None):
    p = {
        'problemId': problem_id
      , 'seed': seed
      , 'solution': _fmt_commands(commands)
    }

    if tag is not None:
        p['tag'] = tag

    return p


def combine_solutions(solutions):
    return json.dumps(solutions)
