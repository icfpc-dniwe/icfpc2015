#!/usr/bin/env ruby

require 'net/http'
require 'json'

$token = ENV['TOKEN'] or raise "Token not found"
$team = ENV['TEAM_ID'] or raise "Team ID not found"

$phrases = [#"deep seven",#not
	        #"chtonians", #not
			#"bigboote", #not, but hint
			#"tsathoggua",#POWER_WORD
			#"buckaroo banzai", #not
			#"i'll tell you later", #not
			#"lectroids", #not
			#"lectroid", #not
			#"the blue blaze irregulars",#not
			#"yododyne", #not
			#"red lectroids", #not
			#"black lectroids", #not
			#"unnamable", #not
			#"ia!", #NOT ?!
			#"r'lyeh", #POWER_WORD
			#"yuggoth", #POWER_WORD
			#"yith", #not
			#"great race of yith", #not
			#"celeano", #not
			#"great hall of celeano", #not
			#"plateau of leng", #not
			#"abyss", #not
			#"lovecraft", #not
			#"necronomicon", #POWER_WORD
			"ia! ia!",
			"abbith",
			"aldebaran",
			"algol",
			"arcturus",
			"celaeno",
			"corona borealis",
			"cykranosh",
			"fomalhaut",
			"haddath",
			"hyades",
			"k'gil'mnon",
			"korvaz",
			"kr'llyand",
			"ktynga",
			"kynarth",
			"kythanil",
			"l'gy'hx",
			"mirkalu",
			"mthura",
			"ogntlach",
			"pherkard",
			"pleiades",
			"rigel",
			"shaggai",
			"shonhi",
			"thuggon",
			"thyoph",
			"tond",
			"trifid nebula",
			"urakhu",
			"vhoorl",
			"world of seven suns",
			"xandra",
			"xecorra",
			"xentilx",
			"xiclotl",
			"xithor",
			"xoth",
			"yaddith",
			"yaksh",
			"yarnak",
			"yekub",
			"yilla",
			"yith",
			"ylidiomph",
			"ymar",
			"zaoth",
			"z'ylsm",
			"lost carcosa",
			"unknown kadath",
			"the dreamlands",
			"the underworld",
			"dreamlands",
			"underworlds",
			"2003 ub313",
			"r1 q0 p1 q1 p1 o0 n0 n0 p1 r1 q1 p1 o0 p1 q1 r1 p1 n0 n0 q1 s1 n1 t1 s1 r1 p1 r1 q1 p1 o0 o0 p1 q1 r1 p1 n0 n0",
			"r1q0p1q1p1o0n0n0p1r1q1p1o0p1q1r1p1n0n0q1s1n1t1s1r1p1r1q1p1o0o0p1q1r1p1n0n0",
			"elder sign",
			"the elder sign",
			"shoggoth",
			"yog sothoth",
			"shub niggurath",
			"shantaks",
			"deep ones",
			"azathoth",
			"ghatanothoa",
			"hastur",
			"hypnos",
			"ithaqua",
			"nodens",
			"nyarlathotep",
			"arkham", 
			"dunwich",
			"innsmouth",
			"kingsport",
			"miskatonic university",
			"miskatonic river",
			"the nameless city",
			"h.p.lovecraft", #--
			"h. p. lovecraft"
	       ]
seeds = [0,
		 0,
		 25221,
		 18451,
		 24851,
		 25460,
		 27669,
		 14104,
		 26637,
		 0,
		 20528,
		 24803,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 0,
		 18,
		]

$task_offset = 10

uri = URI("https://davar.icfpcontest.org/teams/#{$team}/solutions")
req = Net::HTTP::Post.new uri
req.basic_auth '', $token
req['Content-Type'] = 'application/json'

$leaderboardUri = URI "https://davar.icfpcontest.org/rankings.js"
def get_scores(teamId)
	board = JSON.parse(Net::HTTP.get($leaderboardUri).sub(/^var data = /, ''))
	problems = board['data']['settings']
	retv = []
	($task_offset .. problems.length - 1).map { |pid|
		ranks = problems[pid]['rankings']
		us = ranks.map { |teamdesc| teamdesc if teamdesc['teamId'] == teamId.to_i }.compact
		retv << us[0]
	}
	return retv
end

def puts_scores(scores)
	scores.each_with_index { |score, i| 
		puts "#{$task_offset+i} => #{score}"
	}
end

scores = get_scores($team)
interval = 30

puts "INITIAL"
puts_scores(scores)
puts

Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
  $phrases.each_with_index do |w,i|
  	puts "Checking \"#{w}\""
  	($task_offset..24).map { |pid|
    	data = [{ problemId: pid, seed: seeds[pid], tag: i.to_s, solution: w }]
    	req.body = data.to_json
    	res = http.request req
    	raise "Cthulhu is upon us!" unless res.is_a? Net::HTTPSuccess
	}

	for t in (0..300/interval)
		sleep interval
		begin
			new_scores = get_scores $team
		rescue
			retry
		end

		i = 0
		for score in scores
			if score != new_scores[i]
				puts "#{$task_offset+i} => #{new_scores[i]}, SD = #{new_scores[i]['score']-score['score']}, PSD = #{new_scores[i]['power_score']-score['power_score']}"
			end
			i += 1
		end

		scores = new_scores
	 end
  end
end

# def dive(p, b)
# 	if b.instance_of? Array
# 		puts "#{p}: #{b.size}"
# 		dive "#{p}.[]", b[0]
# 	elsif b.instance_of? Hash
# 		puts "#{p}: #{b.keys}"
# 		for k, v in b
# 			dive "#{p}.#{k}", v
# 		end
# 	end
# end
# dive "", JSON.parse(Net::HTTP.get($leaderboardUri).sub(/^var data = /, ''))