#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import argparse

from read import read_problem
from generator import generate_solution
from solver import create_problem, solve


def main(json_file):
  str_problem = read_problem(json_file)
  problem = create_problem(str_problem)
  sequence = solve(problem)
  generate_solution(problem, sequence, 'AllDown')

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('-f', help='File to JSON problem file', type=str)
  args = parser.parse_args()
  main(args.f)
