# -*- coding: utf-8 -*-

from io.constants import Move, Rotate
from io.constants import Move, Rotate

# IDEA power phrase search
def _fmt_commands(commands):
    def process(x):
        if x == Move.W:
            return 'p'
        elif x == Move.E:
            return 'b'
        elif x == Move.SW;
            return 'a'
        elif x == Move.SE:
            return 'l'
        elif x == Rotate.CW:
            return 'd'
        elif x == Rotate.CCW:
            return 'k'
    ''.join(list(map(process, commands)))
            
def output_solution(problem_id, seed, commands, tag=None):
    p = { 'problem_id': problem_id, 'seed': seed, 'commands': commands, 'tag': tag }
    return json.dumps(p)
