#!/usr/bin/env ruby

require 'net/http'
require 'json'

$token = "53/b8w5nkWTgqhWm00puFJMoBk3NPMMs3TAPAD8eSU0="
$team = 180

$phrases = ["deep seven",
	        "chtonians",
			"bigboote",
			"tsathoggua",
			"unnamable",
			"yith",
			"great race of yith",
			"celeano",
			"great hall of celeano",
			"plateau of leng",
			"abyss",
			"lost carcosa",
			"unknown kadath",
			"the dreamlands",
			"the underworld",
			"dreamlands",
			"underworlds",
			"2003 ub313"
	       ]

uri = URI("https://davar.icfpcontest.org/teams/#{$team}/solutions")
req = Net::HTTP::Post.new uri
req.basic_auth '', $token
req['Content-Type'] = 'application/json'

Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
  $phrases.each_with_index do |w,i|
    data = { problemId: 24, seed: 18, tag: i.to_s, solution: w }
    req.body = data.to_json
    res = http.request req
    puts req
    puts res
    raise "Cthulu is upon us!" unless res.is_a? Net::HTTPSuccess
  end
end