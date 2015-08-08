# -*- coding: utf-8 -*-

# IDEA only non-empty rows to store

# Can't import this or any other module in ipython otherwise
import os, sys
PACKAGE_PARENT = '..'
SCRIPT_DIR = os.path.dirname(os.path.realpath(os.path.join(os.getcwd(), os.path.expanduser(__file__))))
sys.path.append(os.path.normpath(os.path.join(SCRIPT_DIR, PACKAGE_PARENT)))

import numpy as np

from common.tools import points2hex, hex2points

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
        #~ print(any(points[:, 2] < 0))
        #~ print(any(points[:, 0] >= self.width - (points[:, 2] // 2)))
        #~ print(any(points[:, 0] >= - self.width - ((points[:, 2] + 1) // 2)))
        #~ print(any(points[:, 2] >= self.height))
        #~ print(any(points[:, 0] < -(points[:, 2] // 2)))
        #~ print(any(points[:, 0] < -((points[:, 2] + 1) // 2)))
        #~ return (any(points[:, 2] < 0) or  # top
                #~ any(points[:, 0] >= self.width - (points[:, 2] // 2)) or  # right x
                #~ any(points[:, 0] >= - self.width - ((points[:, 2] + 1) // 2)) or  # right y
                #~ any(points[:, 2] >= self.height) or  # bottom
                #~ any(points[:, 0] < -(points[:, 2] // 2)) or  # left x
                #~ any(points[:, 0] < -((points[:, 2] + 1) // 2))) # left y
    
    
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

    def __get_all_final_states(self, unit, path):
        moves = [Move.W, Move.E, Move.SW, Move.SE, Rotate.CW, Rotate.CCW]
        newst = deepcopy(unit).move(action)
        all_paths = []
        if not is_locked(newst):    
            all_paths += [(self.__get_all_final_states(newst, path + [action]) for action in moves)]
        else:
            return all_paths

    def get_valid_final_states(self, unit):
        return self.__get_all_final_states(unit, [])