#!/usr/bin/env ruby

require 'net/http'
require 'json'

$token = ENV['TOKEN'] or raise "Token not found"
$team = ENV['TEAM_ID'] or raise "Team ID not found"

uri = URI "https://davar.icfpcontest.org/teams/#{$team}/solutions"
req = Net::HTTP::Post.new uri
req.basic_auth '', $token
req['Content-Type'] = 'application/json'
data = File.read (ARGF.argv[0] or raise "Give me path to a file")

Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
  req.body = data
  res = http.request req
  puts "#{res.code}: #{res.message}"
end
