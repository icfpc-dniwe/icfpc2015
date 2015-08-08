#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import json
import random

from game.unit import Unit

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
                  , fmt_cell(unit.pivot)
                  , ', '.join(map(fmt_cell, unit.members)))




def rand_point(pivot, radius):
    return (random.randint(-radius, radius), random.randint(-radius, radius))

def rand_members(length, pivot, radius):
    return [rand_point(pivot, radius) for _ in range(length)]



random.seed(1488)

UNITS = list(map(lambda p: unit(p, rand_members(4, p, 3)), [
    (4, 4), (11, 4), (11, 4), (11, 11)
]))


