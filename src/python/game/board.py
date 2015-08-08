# -*- coding: utf-8 -*-

# IDEA only non-empty rows to store

import numpy as np

from common.tools import points2hex

class Board:
    def __init__(self, width, height, filled=[]):
        self.width = width
        self.height = height
        self.filled = points2hex(filled)
        #self._rows = {}
    
    
    def check_bounds(self, points):
        return (any(points[:, 2] < 0) or  # top
                any(points[:, 0] >= self.width - (points[:, 2] // 2)) or  # right x
                any(points[:, 0] >= - self.width - ((points[:, 2] + 1) // 2)) or  # right y
                any(points[:, 2] >= height) or  # bottom
                any(points[:, 0] < -(points[:, 2] // 2)) or  # left x
                any(points[:, 0] < -((points[:, 2] + 1) // 2))) # left y
    
    
    def add_cells(self, points):
        '''
        Add points to board and check for filled rows
        Return number of rows completed (and deleted)
        '''
        self.filled = np.append(self.filled, points)
        return self.check_rows()


    def is_locked(self, points):
        '''
        Return 1 if points cannot be placed on board
        '''
        return check_bounds(self, points) or \
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
