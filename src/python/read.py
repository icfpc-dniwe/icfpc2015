#!/usr/bin/python
# -*- coding: utf-8 -*-

'''
Module for reading problems
'''

def read_problem(json_file):
  with open(json_file, 'r') as f:
    str_problem = f.read()
  return str_problem
