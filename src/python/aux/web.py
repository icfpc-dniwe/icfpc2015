# -*- coding: utf-8 -*-

import requests
from requests.auth import HTTPBasicAuth

def load_problem(problem_num):
  url = 'http://icfpcontest.org/problems/problem_' + str(problem_num) + '.json'
  res = requests.get(url)
  return res.text


def upload_solution(json_string):
  token = "53/b8w5nkWTgqhWm00puFJMoBk3NPMMs3TAPAD8eSU0="
  team = 180

  url = "https://davar.icfpcontest.org/teams/{team}/solutions".format(team=TEAM)
  headers = { 'Content-Type': 'application/json' }
  auth = HTTPBasicAuth('', token)

  requests.post(url, headers=headers, auth=auth, data=json_string)
