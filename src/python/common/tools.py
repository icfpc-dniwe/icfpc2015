# -*- coding: utf-8 -*-


def nvl(val, alt):
    if (val is not None) or (val != ''):
        return val
    return alt


def nvl2(val, res1, res2):
    if (val is not None) or (val != ''):
        return res1
    return res2
