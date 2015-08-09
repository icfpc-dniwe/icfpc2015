# -*- coding: utf-8 -*-

# IDEA only non-empty rows to store

# Can't import this or any other module in ipython otherwise
import os, sys
PACKAGE_PARENT = '..'
SCRIPT_DIR = os.path.dirname(os.path.realpath(os.path.join(os.getcwd(), os.path.expanduser(__file__))))
sys.path.append(os.path.normpath(os.path.join(SCRIPT_DIR, PACKAGE_PARENT)))

import numpy as np

from common.tools import points2hex, hex2points
from common.constants import Move, Rotate

from copy import deepcopy

POSSIBLE_MOVES = set([Move.E, Move.W, Move.SW, Move.SE, Rotate.CW, Rotate.CCW])
OPPOSITE_MOVES = {\
    Move.W : set([Move.E]),\
    Move.E : set([Move.W]),\
    Rotate.CW : set([Rotate.CCW]),\
    Rotate.CCW : set([Rotate.CW]),\
    Move.SW : set([]),\
    Move.SE : set([]),\
    -1 : set([])
}
ROTATION_ACTION = set([Rotate.CW, Rotate.CCW])
HORIZONTAL_MOVE_ACTION = set([Move.E, Move.W])
VERTICAL_MOVE_ACTION = set([Move.SW, Move.SE])

class Board:
    def __init__(self, width, height, filled=[]):
        self.width = width
        self.height = height
        if len(filled) < 1:
            self.filled = np.zeros((0, 3), dtype='int32')
        else:
            self.filled = points2hex(filled)
    
    
    def get_start_shift(self, points):
        points2d = hex2points(points)
        unit_width = points2d[:, 0].max() + 1
        return (self.width - unit_width) // 2
    
    
    def check_bounds(self, points):
        '''
        return (any(points[:, 2] < 0) or  # top
                any(points[:, 0] >= self.width - (points[:, 2] // 2)) or  # right x
                any(points[:, 0] >= - self.width - ((points[:, 2] + 1) // 2)) or  # right y
                any(points[:, 2] >= height) or  # bottom
                any(points[:, 0] < -(points[:, 2] // 2)) or  # left x
                any(points[:, 0] < -((points[:, 2] + 1) // 2))) # left y
        '''
        pts = hex2points(points)
        return not all(map(lambda p: (p[0] < self.width) and (p[0] >= 0) and (p[1] < self.height) and (p[1] >= 0), pts))
    
    def add_cells(self, points):
        '''
        Add points to board and check for filled rows
        Return number of rows completed (and deleted)
        '''
        self.filled = np.vstack((self.filled, points))
        return self.check_rows()


    def is_locked(self, points):
        '''
        Return 1 if points cannot be placed on board
        '''
        return self.check_bounds(points) or \
          any([(points == cell).any() for cell in self.filled])
    
    
    def shift_down(self, height):
        '''
        "Collapsing" board from height
        ''' 
        above_idx = np.where(self.filled[:, 2] <= height)
        above = self.filled[above_idx]
        shift_se = np.array([0, -1, 1], dtype='int32')
        shift_sw = np.array([-1, 0, 1], dtype='int32')
        se_idx = np.where(above[:, 2] & 1 == 0)
        sw_idx = np.where(above[:, 2] & 1)
        above[se_idx] += shift_se
        above[sw_idx] += shift_sw
        self.filled[above_idx] = above
    
    
    def check_rows(self):
        '''
        Return number of rows completed ob board.
        Delete this rows.
        '''
        rows_deleted = 0
        for z in range(self.height):
            w = np.where(self.filled[:, 2] == z)
            if len(w[0]) >= self.width:
                self.filled = np.delete(self.filled, w, 0)
                self.shift_down(z)
                rows_deleted += 1
        return rows_deleted

    def __check_move_chain(self, move_chain, move):
        if move not in VERTICAL_MOVE_ACTION:
            if OPPOSITE_MOVES[move] not in move_chain:
                if len(move_chain) > 1:
                    is_rmr = move_chain[0] in ROTATION_ACTION
                    if is_rmr:
                        return move not in ROTATION_ACTION
                    else:
                        return move not in HORIZONTAL_MOVE_ACTION
            else:
                return False
        return True

    def get_valid_final_states(self, unit):
        all_paths = []
        tree = {'action': -1, 'move_chain': [], 'rot_count': 0, 'path': [], 'unit': deepcopy(unit), 'orientation': 0}
        working_list = [{'parent' : tree}]
        while len(working_list) > 0:
            __curt = working_list.pop()
            allowed_moves = POSSIBLE_MOVES - (OPPOSITE_MOVES[__curt['parent']['action']])
            for action in allowed_moves:
                curt = {}
                curt['action'] = action
                curt['move_chain'] = deepcopy(__curt['parent']['move_chain'])
                curt['rot_count'] = __curt['parent']['rot_count']
                curt['orientation'] = __curt['parent']['orientation']
                if (action in ROTATION_ACTION):
                    curt['rot_count'] += 1
                    curt['orientation'] += 1 if action == Rotate.CW else -1
                    if (action == __curt['parent']['action']):
                        if (curt['rot_count'] > 3):
                            print("Fail rot")
                            continue #3 due > 180 degrees rotation
                else:
                    curt['rot_count'] = 0

                if not self.__check_move_chain(curt['move_chain'], action):
                    print("Fail chain")
                    continue #chain of (r+m+r+)* or (m+r+m+)* causes infinite cycle
                
                if action in VERTICAL_MOVE_ACTION:
                    curt['move_chain'] = []

                newu = deepcopy(__curt['parent']['unit'])
                newu.move(action)
                if not self.is_locked(np.array(newu.cells)):
                    curt['unit'] = newu
                    curt['path'] = deepcopy(__curt['parent']['path']) + [action]
                    if action not in curt['move_chain']:
                        curt['move_chain'] += [action]
                    working_list += [{'parent': curt}]
                    print("Added")
                    #res = self.__get_all_final_states(newu, path + [action], action, new_rot_count, move_chain.union(set([action])))
                    #all_paths += res
                else:
                    print("Locked")
                    all_paths += [(deepcopy(__curt['parent']['path']), deepcopy(unit), __curt['parent']['orientation'])]
        return all_paths
