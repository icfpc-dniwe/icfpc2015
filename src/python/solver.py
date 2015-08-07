#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import json

from unit import define_unit
from board import *
from rng import next_number

'''
Module for solving problems
'''


def create_problem(str_problem):
  '''
  Creating problem dictionary that stors board and everything else
  '''
  json_problem = json.loads(str_problem)
  filled = [(jf['x'], jf['y']) for jf in json_problem['filled']]
  board = define_board(json_problem['width'], json_problem['height'], filled)
  units = [
            define_unit([(m['x'], m['y']) for m in un['members']], 
              (un['pivot']['x'], un['pivot']['y']))
            for un in json_problem['units']
          ]
  seeds = json_problem['sourceSeeds']
  problem = {'id': json_problem['id'], 
             'board': board, 
             'units': units, 
             'seeds': seeds, 
             'cur_num': None}
  return problem


def next_unit(problem):
  '''
  Gives next unit
  '''
  if problem['cur_num'] is None:
    return None
  lock_unit(problem['board'])
  problem['cur_num'] = next_number(problem['cur_num'])
  unit_num = problem['cur_num'] % len(problem['units'])
  return problem['units'][unit_num]


def solve(problem):
  seq = [0, 3, 1, 2]
  ret_seq = [0] * len(problem['seeds'])
  for idx, seed in enumerate(problem['seeds']):
    ret_seq[idx] = []
    problem['cur_num'] = seed
    unit = next_unit(problem)
    while add_unit(problem['board'], unit):
      print(unit)
      cur = 0
      while move_unit(problem['board'], seq[cur]):
        print(cur)
        print_board(problem['board'])
        ret_seq[idx] += [seq[cur]]
        cur += 1
        if cur >= len(seq):
          cur = 0
        input("Press ENTER to continue...")
      ret_seq[idx] += [seq[cur]]
      lock_unit(problem['board'])
      unit = next_unit(problem)
      print(problem['board'])
  return ret_seq
