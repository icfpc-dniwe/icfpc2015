# -*- coding: utf-8 -*-

# Can't import this or any other module in ipython otherwise
import os, sys
PACKAGE_PARENT = '..'
SCRIPT_DIR = os.path.dirname(os.path.realpath(os.path.join(os.getcwd(), os.path.expanduser(__file__))))
sys.path.append(os.path.normpath(os.path.join(SCRIPT_DIR, PACKAGE_PARENT)))

import numpy as np

from copy import deepcopy

from common.constants import *
from common.tools import points2hex, hex2points

# IDEA pools of units to avoid multiple entries

class Unit:
    def __init__(self, pivot, cells):
        #pivot2d = np.array(pivot, dtype='int32')
        #self.pivot = np.zeros((3,), dtype='int32')
        #self.pivot[0] = pivot2d[0] - (pivot2d[1] - (pivot2d[1]&1)) // 2
        #self.pivot[1] = -self.pivot[0] - pivot2d[1]
        #self.pivot[2] = pivot2d[1]
        self.pivot = points2hex(pivot)
        
        #cells2d = np.array(cells)
        #self.cells = np.zeros((len(cells), 3), dtype='int32')
        #self.cells[:, 0] = cells2d[:, 0] - (cells2d[:, 1] - (cells2d[:, 1] & 1)) // 2
        #self.cells[:, 1] = -self.cells[:, 0] - cells2d[:, 1]
        #self.cells[:, 2] = cells2d[:, 1]
        self.cells = points2hex(cells)

    def __repr__(self):
        x,y = hex2points(np.array([self.pivot]))[0]
        return "pvt: ({0}, {1})".format(x,y)

    def clone(self):
        return deepcopy(self)


    def undo_move(self, direction):
        if direction == Move.E:
            self.shift(Move.W)
        elif direction == Move.W:
            self.shift(Move.E)
        elif direction == Move.SE:
            print('SE')
            shift = np.array([0, -1, 1],, dtype='int32')
            self.cells -= shift
            self.pivot -= shift
        elif direction == Move.SW:
            print('SW')
            shift = np.array([-1, 0, 1], dtype='int32')
            self.cells -= shift
            self.pivot -= shift
        elif direction == Rotate.CW:
            self.rotate(Rotate.CCW)
        elif direction == Rotate.CCW:
            self.rotate(Rotate.CW)


    def move(self, direction):
        if direction == Rotate.CW or direction == Rotate.CCW:
            self.rotate(direction)
        else:
            self.shift(direction)


    def rotate(self, direction):
        # moving to (0, 0, 0)
        self.cells = self.cells - self.pivot
        if direction == Rotate.CW: # CW
            self.cells = np.vstack((-self.cells[:, 2], -self.cells[:, 0], -self.cells[:, 1])).T
        else: # CCW
            self.cells = np.vstack((-self.cells[:, 1], -self.cells[:, 2], -self.cells[:, 0])).T
        # moving back
        self.cells = self.cells + self.pivot


    def shift(self, direction):
        if direction == Move.W: # W
            shift = np.array([-1, 1, 0], dtype='int32')
        elif direction == Move.E: # E
            shift = np.array([1, -1, 0], dtype='int32')
        elif direction == Move.SW: # SW
            shift = np.array([-1, 0, 1], dtype='int32')
        elif direction == Move.SE: # SE
            shift = np.array([0, -1, 1], dtype='int32')
        self.cells = self.cells + shift
        self.pivot = self.pivot + shift
