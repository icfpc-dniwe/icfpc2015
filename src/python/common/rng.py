# -*- coding: utf-8 -*-

'''
Module for random number generator
'''

def next_number(prev):
  mult = 1103515245
  inc = 12345
  mod = 1 << 32
  num = (prev * mult + inc) % mod
  return (num, num & (0xFFFF << 16))
