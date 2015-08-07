#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np

from unit import shift_unit

'''
Module for board
'''


def define_board(width, height, filled):
  board = np.zeros((width, height), dtype='int32')
  for cell in filled:
    board[cell] = 1
  return {'width': width,
          'height': height,
          'board': board,
          'unit': None}


def add_unit(board, unit):
  board['unit'] = unit
  unit_width = np.max(unit[:, 0]) + 1
  shift = (board['width'] - unit_width) // 2
  [shift_unit(unit, 0) for _ in range(shift)]


#def get_neighbors(cell):
  #neighbors = np.zeros((6, 2), dtype='int32')
  #neighbors[0, 0] = cell
  #return None
