# -*- coding: utf-8 -*-


# Commands

class Move:
 W  = 0 
 E  = 1 
 SW = 2 
 SE = 3 

class Rotate:
 CW =  4
 CCW = 5


SYMBOLS = [
  ["p", "'", "!", ".", "0", "3"]
, ["b", "c", "e", "f", "y", "2"]
, ["a", "g", "h", "i", "j", "4"]
, ["l", "m", "n", "o", " ", "5"]
, ["d", "q", "r", "v", "z", "1"]
, ["k", "s", "t", "u", "w", "x"]]


def from_symbol(s):
  i = 0
  while i < 6:
   if s in SYMBOLS[i]: return i
   i += 1

  assert 1 == 0, 'incorrect symbol'
