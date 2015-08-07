#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
from copy import deepcopy

from unit import shift_unit, rotate_unit

'''
Module for board
'''


def define_board(width, height, filled = []):
  board = np.zeros((width, height), dtype='int32')
  for cell in filled:
    board[cell[1], cell[0]] = 1
  return {'width': width,
          'height': height,
          'board': board,
          'unit': None}


def add_unit(board, unit):
  '''
  Return 1 if can be added.
  '''
  if not unit:
    return 0
  board['unit'] = deepcopy(unit)
  unit_width = unit['members'][:, 0].max() + 1
  shift = (board['width'] - unit_width) // 2
  [shift_unit(board['unit'], 0) for _ in range(shift)]
  if check_unit(board):
    return 1
  del board['unit']
  board['unit'] = None
  return 0


def move_unit(board, move):
  '''
  0 = 'e'
  1 = 'w'
  2 = 'se'
  3 = 'sw'
  4 = 'cw'
  5 = 'ccw'
  
  Return 1 if can be moved.
  '''
  if not board['unit']:
    return 0
  new_unit = deepcopy(board['unit'])
  if move < 4:
    shift_unit(new_unit, move)
  else:
    return 0
    rotate_unit(new_unit, move - 4)
  if check_unit(board, new_unit):
    board['unit'] = new_unit
    return 1
  return 0


def check_unit(board, unit=None):
  '''
  Return 1 if unit can be placed.
  '''
  if not unit:
    unit = board['unit']
  unit = unit['members']
  if ((unit < 0).any() or 
      any(unit[:, 0] >= board['width']) or 
      any(unit[:, 1] >= board['height'])):
    return 0
  return all(board['board'][unit[:, 1], unit[:, 0]] == 0)


def lock_unit(board):
  if not board['unit']:
    print('returning')
    return
  print('locking')
  unit = board['unit']['members']
  board['board'][unit[:, 1], unit[:, 0]] = 1
  del board['unit']
  board['unit'] = None


def print_board(board):
  pr = np.zeros(board['board'].shape, dtype='int32')
  bd = board['board']
  un = board['unit']['members']
  pr[bd == 1] = 1
  pr[un[:, 1], un[:, 0]] = 2
  print(pr)


#def get_neighbors(cell):
  #neighbors = np.zeros((6, 2), dtype='int32')
  #neighbors[0, 0] = cell
  #return None
