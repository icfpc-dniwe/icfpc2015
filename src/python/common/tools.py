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
    p_hex = np.zeros((points2d.shape[0], 3), dtype='int32').squeeze()
    p_hex[:, 0] = points2d[:, 0] - (points2d[:, 1] - (points2d[:, 1] & 1)) // 2
    p_hex[:, 2] = points2d[:, 1]
    p_hex[:, 1] = -p_hex[:, 0] - p_hex[:, 1]
    return p_hex
