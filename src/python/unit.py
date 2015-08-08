# -*- coding: utf-8 -*-

import numpy as np

'''
Module for Unit
'''


def define_unit(members, pivot):
  '''
  Function defines Unit
  members: list of Cells (tuples)
  pivot: Cell (tuple)
  '''
  return {'members': np.array(members, dtype='int32'), 
          'pivot': np.array(pivot, dtype='int32')}


def shift_unit(unit, direction):
  if direction == 0:
    unit['members'][:, 0] = unit['members'][:, 0] + 1
    unit['pivot'][0] = unit['pivot'][0] + 1
  elif direction == 1:
    unit['members'][:, 0] = unit['members'][:, 0] - 1
    unit['pivot'][0] = unit['pivot'][0] - 1
  elif direction == 2:
    unit['members'][:, 0] = unit['members'][:, 0] + unit['members'][:, 1] % 2
    unit['members'][:, 1] = unit['members'][:, 1] + 1
    unit['pivot'][0] = unit['pivot'][0] + unit['pivot'][1] % 2
    unit['pivot'][1] = unit['pivot'][1] + 1
  elif direction == 3:
    unit['members'][:, 0] = unit['members'][:, 0] - 1 + unit['members'][:, 1] % 2
    unit['members'][:, 1] = unit['members'][:, 1] + 1
    unit['pivot'][0] = unit['pivot'][0] -1 + unit['pivot'][1] % 2
    unit['pivot'][1] = unit['pivot'][1] + 1


def rotate_unit(unit, direction):
  return


#def chr2move(chr_move):
  #return


#def default_moves():
  #moves = dict()
  #moves['w'] = {'p', "'", '!', '.', '0', '3'}
  #moves['e'] = {'b', 'c', 'e', 'f', 'y', '2'}
  #moves['sw'] = {'a', 'g', 'h', 'i', 'j', '4'}
  #moves['se'] = {'l', 'm', 'n', 'o', ' ', '5'}
  #moves['cw'] = {'d', 'q', 'r', 'v', 'z', 1}
  #moves['cc'] = {'k', 's', 't', 'u', 'w', 'x'}
