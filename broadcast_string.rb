#!/usr/bin/env ruby

require 'net/http'
require 'json'

$phrase = "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn!"
#$phrase = "ei! 000k4k4ei! 22224ei! 24ei!044ei!004222ei!004224222240044"
$token = "53/b8w5nkWTgqhWm00puFJMoBk3NPMMs3TAPAD8eSU0="
$team = 180

uri = URI("https://davar.icfpcontest.org/teams/#{$team}/solutions")
req = Net::HTTP::Post.new uri
req.basic_auth '', $token
req['Content-Type'] = 'application/json'

Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
  data = (0..23).map { |i| { problemId: i, seed: 0, tag: "H", solution: $phrase } }
  req.body = data.to_json
  http.request req
end
