#!/usr/bin/python
# -*- coding: utf-8 -*-

# Command line arguments
#
# Official:
# -f  FILENAME  File containing JSON encoded input.
# -t  NUMBER    Time limit, in seconds, to produce output
# -m  NUMBER    Memory limit, in megabytes, to produce output
# -p  STRING    Phrase of power, as quoted string
#
# Debugging:
# -r  STRING    Run specified series of commands instead of bot
#
# Site interaciton:
# -n  NUMBER    Load JSON #n problem from the site
# -u  --        Do upload the solution
# -a  STRING    Tag of the solution


import sys
import argparse

from io.input import parse_problem
from io.output import output_solution
from io.web import *

from icfpc.game.solution import get_solution, run_commands


DEBUG_MODE = 1


def process_problem(json_string, args):
 
  (problem, seeds) = parse_problem(json_string)
  phrases = args.p
  results = []

  for seed in seeds:
    run = args.r or get_solution(problem, seed, phrases)

    if DEBUG_MODE == 1:
      dbg = run_commands(problem, seed, run, phrases)
      print(dbg)

    results.append((problem.problem_id, seed, run))

  return results



def main(args):
  
  problem_num = args.n
  filenames = args.f
  
  if problem_num is not None:
    json_string = load_problem(problem_num)
    results = process_problem(json_string, args)
    solution = '\n\n'.join(map(lambda d: output_solution(*d, tag=args.a), results))
 
    if args.u:
      upload_solution(solution)
    else:
      print(solution)
                              
  else:
    for fn in filenames:
      h = open(fn, 'r') 
      json_string = h.read()
      results = process_problem(json_string, args)
      solution = '\n\n'.join(map(lambda d: output_solution(*d), results))
      print(solution)


if __name__ == '__main__':
  parser = argparse.ArgumentParser()

  parser.add_argument('-f', action='append',     type=str, help='File to JSON problem file')
  parser.add_argument('-p', action='append',     type=str, help='Phrase of power, as quoted string')
  parser.add_argument('-t', action='store',      type=int, help='Time limit, in seconds, to produce output')
  parser.add_argument('-m', action='store',      type=int, help='Memory limit, in megabytes, to produce output')

  parser.add_argument('-r', action='store',      type=str, help='Run specified series of commands instead of bot')

  parser.add_argument('-n', action='store',      type=int, help='Load JSON problem #n from the site')
  parser.add_argument('-a', action='store',      type=int, help='Tag of the solution')
  parser.add_argument('-u', action='store_true',           help='Do upload the solution')

  args = parser.parse_args()
  main(args)
