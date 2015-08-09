# -*- coding: utf-8 -*-

from copy import deepcopy
import networkx as nx
from networkx.algorithms.traversal.depth_first_search import dfs_tree

from common.constants import *
from common.tools import get_score, hex2points
from common.rng import next_number


def get_solution(problem, seed, phrase):
    '''
    Find solution to problem
    '''
    commands = []
    max_depth = 1 # maximum depth of tree-search
    tree = nx.DiGraph()
    tree.add_node(1)
    tree.node[1]['board'] = problem.board
    tree.node[1]['score'] = 0
    tree.node[1]['unit_idx'] = seed % len(problem.units)
    tree.node[1]['r_num'] = seed
    tree.node[1]['directions'] = []
    root_node = 1
    # generate tree
    generate_tree(problem.units, max_depth, tree, root_node)
    while len(tree.neighbors(root_node)) > 0: # while we need to update tree
        score, move_to = best_move(tree, root_node) # find best next move
        print('Score:', score)
        print('Directions:', tree.node[move_to]['directions'][:4])
        # do best move
        root_node = move_to
        commands += tree.node[move_to]['directions']
        # update tree (search deeper from move_to node)
        generate_tree(problem.units, max_depth, tree, root_node)
    return commands


def generate_moves(board, unit, last_move=None):
    '''
    Generating final positions for (board, unit)
    '''
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
        if last_move is not None:
            cur_u.move(last_move)
        while not board.is_locked(cur_u.cells):
            cur_u.move(s[cur])
            direc += [s[cur]]
            cur += 1
            if cur >= len(s):
                cur = 0
        if len(direc) <= 0:
            continue
        else:
            #cur_u.undo_move(s[cur])
            yield (cur_u, direc)
    #return moves


def generate_tree(unit_list, depth, tree, node):
    '''
    Function generates of updates game tree
    '''
    if depth <= 0:
        return
    if len(tree.neighbors(node)) > 0:
        # we already have this node => go deeper
        print('Going deeper on depth', depth)
        for next_node in tree.neighbors_iter(node):
            generate_tree(unit_list, depth - 1, tree, next_node)
    else:
        # we need to generate new nodes
        print('Generating on depth', depth)
        cur_score = tree.node[node]['score']
        board = tree.node[node]['board']
        unit = unit_list[tree.node[node]['unit_idx']]
        r_num = tree.node[node]['r_num']
        if (len(tree.node[node]['directions'])) > 0:
            # last direction command
            last_move = tree.node[node]['directions'][-1]
        else:
            last_move = None
        print('LM:', last_move)
        for move in generate_moves(board, unit, last_move):
            # creating new node that correspond to 'move'
            print('  Creating move with dir:', len(move[1]), move[1][:4])
            #input('Press ENTER to continue...')
            # scoring curent board
            size = unit.cells.shape[0]
            next_board = deepcopy(board)
            lines = next_board.add_cells(move[0].cells)
            score = get_score(size, lines)
            # generating new random number for next unit
            nr_num, nunit_idx = next_number(r_num)
            # adding node
            next_node = len(tree.nodes()) + 1
            tree.add_edge(node, next_node)
            tree.node[next_node]['board'] = next_board
            tree.node[next_node]['unit_idx'] = nunit_idx % len(unit_list)
            tree.node[next_node]['r_num'] = nr_num
            tree.node[next_node]['score'] = score + cur_score
            tree.node[next_node]['directions'] = move[1]
            # we need to go deeper
            generate_tree(unit_list, depth - 1, tree, next_node)


def best_move(tree, node):
    '''
    Finding best next move from node
    '''
    if len(tree.neighbors(node)) == 0:
        # leaf => return score
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
