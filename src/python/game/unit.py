# -*- coding: utf-8 -*-

from copy import deepcopy

from common.constants import *

# IDEA pools of units to avoid multiple entries

class Unit:
    def __init__(self, pivot, cells):
        self.pivot = pivot
        self.cells = cells
        
    def clone(self):
        return deepcopy(self)

    # TODO
    def rotate(self, direction):
        pass

    # TODO
    def move(self, direction):
        pass

