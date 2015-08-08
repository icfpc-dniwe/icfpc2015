# -*- coding: utf-8 -*-

from io.constants import Move, Rotate


# IDEA power phrase search
def _fmt_commands(commands):
    # TODO
    return []


def output_solution(problem_id, seed, commands, tag=None):
    # TODO
    return '''
{
%s
%s
%s
%s
}
''' % (str(problem_id), str(seed), tag, _fmt_commands(commands))

