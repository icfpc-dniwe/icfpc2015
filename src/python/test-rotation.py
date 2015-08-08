#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import json
import random

from game.unit import Unit
from common.constants import *
from common.tools import hex2points

# Root ::= {"units": [Unit]}
# Unit ::= {"color": Color, "pivot": [Cell], "members": [Cell]}
# Cell ::= {"x": Int, "y": Int}

COLORS = [
  [1, 0, 0]
, [0, 1, 0]
, [0, 0, 1]
, [1, 0, 1]
, [0, 1, 1]]


def fmt_color(color):
    return '[%s]' % ', '.join(map(str, color))

def fmt_cell(cell):
    (x, y) = cell
    return '{"x": %s, "y": %s}' % (str(x), str(y))


def fmt_unit(unit, color):
    return '''{ "color": %s
              , "pivot": %s
              , "members": [%s]}''' % (
                    fmt_color(color)
                  , fmt_cell(hex2points(unit.pivot))
                  , '\n, '.join(map(fmt_cell, map(hex2points, unit.cells))))




def rand_point(radius):
    return (random.randint(-radius, radius), random.randint(-radius, radius))

def point_sum(p1, p2):
    (x1, y1) = p1
    (x2, y2) = p2
    return (x1+x2, y1+y2)

def rand_members(length, pivot, radius):
    return [point_sum(pivot, rand_point(radius)) for _ in range(length)]


random.seed(1488)

units = list(map(lambda p: Unit(p, rand_members(4, p, 3)), [
    (4, 4), (11, 4), (11, 4), (11, 11)
]))


result = []

for unit in units:
    src = unit
    dst = unit.clone()
    dst.rotate(Rotate.CW)

    result.append(fmt_unit(src, COLORS[0]))
    result.append(fmt_unit(dst, COLORS[1]))
    

print('{"units": [%s]}' % (', '.join(result)))
    
