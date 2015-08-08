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

from game.solution import get_solution, run_commands


DEBUG_MODE = 1


def process_problem(json_string, args):
  
  phrases = args.p
  # TODO phrases!

  (problem, seeds) = parse_problem(json_string)
  
  for seed in seeds:
    
    run = args.r

    # if no run provided, get the bot output (this is an actual solution)
    if run is None:
      run = get_solution(problem, seed, None)

    # execute commands in a given run (this is for debug reasons)
    if DEBUG_MODE = 1:
      dbg = run_commands(problem, seed, run)
      # TODO output debug information

    return (problem.problem_id, seed, run)

# phrases, time_limit, memory_limit, runs):

def main(args):
  
  problem_num = args.n
  filenames = args.f
  
  if problem_num is not None:
    json_string = load_problem(problem_num)
    (problem_id, seed, run) = process_problem(json_string, args)
    solution = output_solution(problem_id, seed, run, args.a)
 
    if args.u:
      upload_solution(solution)
    else:
      print(solution)
                              
  else:
    for fn in filenames:
      h = open(fn, 'r') 
      result = process_problem(f.read(), args)
      if result is not None:
        (problem_id, seed, run) = result
        solution = output_solution(problem_id, seed, run, args.a)
        print solution


if __name__ == '__main__':
  parser = argparse.ArgumentParser()

  parser.add_argument('-f', action='append',     type=str, help='File to JSON problem file')
  parser.add_argument('-p', action='append',     type=str, help='Phrase of power, as quoted string')
  parser.add_argument('-t', action='store',      type=int, help='Time limit, in seconds, to produce output')
  parser.add_argument('-m', action='store',      type=int, help='Memory limit, in megabytes, to produce output')

  parser.add_argument('-r', action='append',     type=str, help='Run specified series of commands instead of bot')

  parser.add_argument('-n', action='store',      type=int, help='Load JSON problem #n from the site')
  parser.add_argument('-a', action='store',      type=int, help='Tag of the solution')
  parser.add_argument('-u', action='store_true',           help='Do upload the solution')

  args = parser.parse_args()
  main(args)
