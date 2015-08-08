# -*- coding: utf-8 -*-

from copy import deepcopy

from common.constants import *
from commot.tools import get_score
from common.rng import next_number

# TODO Move and Rotate, not ascii characters!
def get_solution(problem, seed, phrase):
    commands = []
    depth = 4
    return commands


def generate_moves(board, unit):
    seq = [[], [], [], []]
    seq[0] = [Move.E, Move.SW, Move.W, Move.SE]
    seq[1] = [Move.W, Move.SE, Move.E, Move.SW]
    seq[2] = [Move.SE, Move.SW]
    seq[3] = [Move.SW, Move.SE]
    #moves = []
    for s in seq:
        cur_u = unit.clone()
        cur = 0
        direc = []
        while not board.is_locked(cur_u.cells):
            cur_u.move(s[cur])
            direc += [s[cur]]
            cur += 1
            if cur >= len(s):
                cur = 0
        cur_u.undo_move(s[cur])
        yield [(cur_u, direc)]
    #return moves


def generate_tree(problem, unit_idx, depth):
    if depth <= 0:
        return 0
    board = deepcopy(problem.board)
    unit = problem.units[unit_idx]
    max_score = -10000
    max_move = None
    for move in generate_moves(board, unit):
        if len(move[1]) == 0:
            return (move, -1000) # end
        size = problem.units[unit_idx].shape[0]
        lines = board.add_cells(unit.cells)
        score = get_score(size, lines)
        if score > max_score:
            max_score = score
            max_move = move


# TODO
def run_commands(problem, seed, phrase, run):
    return ''
