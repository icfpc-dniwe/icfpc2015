# -*- coding: utf-8 -*-

# IDEA only non-empty rows to store

import numpy as np

from common.tools import point2hex

class Board:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.filled = []
        #self._rows = {}


    def add_cells(self, points):
        points = point2hex(points)


    def is_filled(self, points):
        pass

