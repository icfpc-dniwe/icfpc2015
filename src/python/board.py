#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
from copy import deepcopy

from unit import shift_unit, rotate_unit

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
  if check_unit(board):
    return 1
  board['unit'] = None
  return 0


def move_unit(board, move):
  '''
  0 = 'e'
  1 = 'w'
  2 = 'sw'
  3 = 'se'
  4 = 'cw'
  5 = 'ccw'
  '''
  new_unit = deepcopy(board['unit'])
  if move < 4:
    shift_unit(new_unit, move)
  else:
    rotate_unit(new_unit, move - 4)
  if check_unit(board, new_unit):
    board['unit'] = new_unit
    return 1
  return 0


def check_unit(board, unit=None):
  '''
  Return 0 if unit cannot be placed
  '''
  if not unit:
    unit = board['unit']
  unit = unit['members']
  return all(board['board'][unit[:, 0], unit[:, 1]] == 0)


#def get_neighbors(cell):
  #neighbors = np.zeros((6, 2), dtype='int32')
  #neighbors[0, 0] = cell
  #return None
