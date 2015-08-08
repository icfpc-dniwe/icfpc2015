# -*- coding: utf-8 -*-

# IDEA only non-empty rows to store

import numpy as np

from common.tools import point2hex

class Board:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.fills
        #self._rows = {}
    
    
    def __check_bounds__(self, points):
      return (any(points[:, 2] < 0) or \ # top
              any(points[:, 0] >= self.width - (points[:, 2] // 2)) or \ # right x
              any(points[:, 0] >= - self.width - ((points[:, 2] + 1) // 2)) or \ # right y
              any(points[:, 2] >= height) or \ # bottom
              any(points[:, 0] < -(points[:, 2] // 2)) or \ # left x
              any(points[:, 0] < -((points[:, 2] + 1) // 2)) # left y


    def add_cells(self, points):
        points = point2hex(points)


    def is_locked(self, points):
        return __check_bounds__(self, points) or \
          any([(points == cell).any() for cell in self.filled])
