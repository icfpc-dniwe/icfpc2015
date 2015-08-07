#!/usr/bin/python
# -*- coding: utf-8 -*-

import json

'''
Module generates solution
'''


def generate_solution(problem, seq, tag):
  solution = [
    {'problemId': problem['id'],
     'seed': seed, 
     'tag': tag,
     'solution': ''.join([move2chr(move) for move in seq[idx]])}
     for idx, seed in enumerate(problem['seeds'])
    ]
  with open('../../submissions/' + str(problem['id']) + '_AllDown.json', 'w') as f:
    print(json.dumps(solution), file=f)


def move2chr(move):
  return {
    0: 'e',
    1: '!',
    2: ' ',
    3: 'i'
    }.get(move, '\n')
