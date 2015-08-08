# -*- coding: utf-8 -*-

import json

from game.unit import unit 
from game.board import Board

#  "id": number
#     A unique number identifying the problem 
#  
#  "units": [Unit]
#     The various unit configurations that may appear in this game.
#     There might be multiple entries for the same unit.
#     When a unit is spawned, it will start off in the orientation
#     specified in this field. 
#  
#  "width": number
#    The number of cells in a row 
#  
#  "height": number
#    The number of rows on the board 
#  
#  "filled": [Cell]
#    Which cells start filled 
#  
#  "sourceLength": number
#    How many units in the source 
#  
#  "sourceSeeds": [number]
#    How to generate the source and how many games to play 




class Problem:
    def __init__(self, problem_id, board, units, src_length):
        self.problem_id = problem_id
        self.board = board
        self.units = units
        self.src_length = src_length


# TODO for now just extract all the data
def _parse_unit(p):
    pass

def _parse_board(p):
    pass

def parse_problem(json_string):
    p = json.loads(json_string)
   
    problem_id = p['id']
    src_length = p['sourceLength']
    seeds = p['sourceSeeds']
    
    width = p['width']
    height = p['height']

    #TODO fill the board
    board = Board(width, height)

    # TODO create units
    units = []

    problem = Problem(problem_id, board, units, src_length)

    return (problem, seeds)
