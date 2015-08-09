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
import time

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
        points2d = hex2points(points)
        return ((points2d < 0).any() or 
                any(points2d[:, 0] >= self.width) or 
                any(points2d[:, 1] >= self.height))
    
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
        filled_set = set([tuple(x) for x in self.filled])
        points_set = set([tuple(x) for x in points])
        return (self.check_bounds(points) or 
          len(filled_set.intersection(points_set)) > 0)
    
    
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
        '''
        Returns [(path, unit, orientation)]
        path - list of moves
        unit - final state
        orientation - sum of rotations
        '''
        before = time.time()
        allowed_rots = 3 if not unit.is_central_symmetric() else 1
        all_paths = []
        tree = {'action': -1, 'move_chain': [], 'rot_count': 0, 'path': [], 'unit': deepcopy(unit), 'orientation': 0, 'state_set': set(set([(cell[0], cell[1], cell[2]) for cell in unit.cells]))}
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
                curt['state_set'] = deepcopy(__curt['parent']['state_set'])
                if (action in ROTATION_ACTION):
                    curt['rot_count'] += 1
                    curt['orientation'] += 1 if action == Rotate.CW else -1
                    if (action == __curt['parent']['action']):
                        if (curt['rot_count'] > allowed_rots):
                            #print("Fail rot")
                            continue
                else:
                    curt['rot_count'] = 0

                if not self.__check_move_chain(curt['move_chain'], action):
                    #print("Fail chain")
                    continue #chain of (r+m+r+)* or (m+r+m+)* causes infinite cycle
                
                if action in VERTICAL_MOVE_ACTION:
                    curt['move_chain'] = []

                newu = deepcopy(__curt['parent']['unit'])
                newu.move(action)

                set_of_cells = set([(cell[0], cell[1], cell[2]) for cell in newu.cells])
                if set_of_cells in curt['state_set']:
                    continue
                else:
                    curt['state_set'] = curt['state_set'].union(deepcopy(set_of_cells))

                curt['unit'] = newu
                curt['path'] = deepcopy(__curt['parent']['path']) + [action]
                if action not in curt['move_chain']:
                    curt['move_chain'] += [action]

                if not self.is_locked(np.array(newu.cells)):
                    working_list += [{'parent': curt}]
                    #print("Added")
                else:
                    #print("Locked")
<<<<<<< Updated upstream
                    all_paths += [(deepcopy(__curt['parent']['path']), deepcopy(unit), __curt['parent']['orientation'])]
        return all_paths
=======
                    all_paths += [(deepcopy(curt['path']), deepcopy(newu), curt['orientation'])]
        after = time.time()
        print("%.2gs" % (after-before))
        return all_paths

    def get_all_longest_paths(self, unit):
        all_sols = get_valid_final_states(unit)
        maxpthlen = 0
        maxsol = None
        for sol in all_sols:
            if (len(sol[0]) > maxpthlen):
                maxsol = sol
>>>>>>> Stashed changes
