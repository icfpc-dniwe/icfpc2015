# -*- coding: utf-8 -*-

from copy import deepcopy
import networkx as nx
from networkx.algorithms.traversal.depth_first_search import dfs_tree

from common.constants import *
from common.tools import get_score
from common.rng import next_number


def get_solution(problem, seed, phrase):
    commands = []
    max_depth = 10
    tree = nx.DiGraph()
    tree.add_node(1)
    tree.node[1]['board'] = problem.board
    tree.node[1]['score'] = 0
    tree.node[1]['unit_idx'] = seed % len(problem.units)
    tree.node[1]['r_num'] = seed
    tree.node[1]['directions'] = []
    root_node = 1
    generate_tree(problem.units, max_depth, tree, root_node)
    while len(tree.neighbors(root_node)) > 0:
        score, move_to = best_move(tree, root_node)
        root_node = move_to
        commands += tree.node[move_to]['directions']
        generate_tree(problem.units, max_depth, tree, root_node)
    return commands


def generate_moves(board, unit):
    seq = [[], [], [], []]
    seq[0] = [Move.E, Move.SW, Move.W, Move.SE]
    seq[1] = [Move.W, Move.SE, Move.E, Move.SW]
    seq[2] = [Move.SE, Move.SW]
    seq[3] = [Move.SW, Move.SE]
    shift = board.get_start_shift(unit.cells)
    for s in seq:
        cur_u = unit.clone()
        cur = 0
        direc = []
        [cur_u.shift(Move.E) for _ in range(shift)]
        while not board.is_locked(cur_u.cells):
            cur_u.move(s[cur])
            direc += [s[cur]]
            cur += 1
            if cur >= len(s):
                cur = 0
        if len(direc) <= 0:
            continue
        else:
            cur_u.undo_move(s[cur])
            yield (cur_u, direc)
    #return moves


def generate_tree(unit_list, depth, tree, node):
    if depth <= 0:
        return
    if len(tree.neighbors(node)) > 0:
        #print('Going deeper on depth', depth)
        for next_node in tree.neighbors_iter(node):
            generate_tree(unit_list, depth - 1, tree, next_node)
    else:
        #print('Generating on depth', depth)
        cur_score = tree.node[node]['score']
        board = tree.node[node]['board']
        unit = unit_list[tree.node[node]['unit_idx']]
        r_num = tree.node[node]['r_num']
        for move in generate_moves(board, unit):
            #if move is None:
            #    continue
            #print('  Creating move idx', idx)
            size = unit.cells.shape[0]
            next_board = deepcopy(board)
            lines = next_board.add_cells(move[0].cells)
            score = get_score(size, lines)
            nr_num, nunit_idx = next_number(r_num)
            next_node = len(tree.nodes()) + 1
            tree.add_edge(node, next_node)
            tree.node[next_node]['board'] = next_board
            tree.node[next_node]['unit_idx'] = nunit_idx % len(unit_list)
            tree.node[next_node]['r_num'] = nr_num
            tree.node[next_node]['score'] = score + cur_score
            tree.node[next_node]['directions'] = move[1]
            generate_tree(unit_list, depth - 1, tree, next_node)


def best_move(tree, node):
    if len(tree.neighbors(node)) == 0:
        return tree.node[node]['score'], node
    max_score = -10000
    max_move = None
    for move_to in tree.neighbors_iter(node):
        score, _ = best_move(tree, move_to)
        if score > max_score:
            max_score = score
            max_move = move_to
    return max_score, max_move


# TODO
def run_commands(problem, seed, phrase, run):
    return ''
