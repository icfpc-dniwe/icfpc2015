#!/usr/bin/env ruby

require 'net/http'
require 'json'

$token = "53/b8w5nkWTgqhWm00puFJMoBk3NPMMs3TAPAD8eSU0="
$team = 180

uri = URI("https://davar.icfpcontest.org/teams/#{$team}/solutions")
req = Net::HTTP::Post.new uri
req.basic_auth '', $token
req['Content-Type'] = 'application/json'
data = File.read(ARGF.argv[0])

Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
  req.body = data.to_json
  http.request req
end
