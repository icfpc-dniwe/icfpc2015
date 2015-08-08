# -*- coding: utf-8 -*-

import numpy as np


def nvl(val, alt):
    if (val is not None) or (val != ''):
        return val
    return alt


def nvl2(val, res1, res2):
    if (val is not None) or (val != ''):
        return res1
    return res2


def points2hex(points):
    points2d = np.array(points, dtype='int32')
    p_hex = np.zeros((len(points), 3), dtype='int32').squeeze()
    p_hex[:, 0] = points2d[:, 0] - (points2d[:, 1] - (points2d[:, 1] & 1)) // 2
    p_hex[:, 2] = points2d[:, 1]
    p_hex[:, 1] = -p_hex[:, 0] - p_hex[:, 1]
    return p_hex


def hex2points(p_hex):
    points = np.zeros((len(p_hex), 2), dtype='int32')
    points[:, 0] = p_hex[:, 0] + (p_hex[:, 2] - (p_hex[:, 2] & 1)) // 2
    points[:, 1] = p_hex[:, 2]
    return points
