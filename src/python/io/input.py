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


def parse_problem(json_string):
    json.loads(json_string)
   
    # TODO read from json directly
    problem_id = None
    src_length = None
    seeds = []
    
    width = None
    height = None

    #TODO fill the board
    board = Board(width, height)

    # TODO create units
    units = []

    problem = Problem(problem_id, board, units, src_length)

    return (problem, seeds)
