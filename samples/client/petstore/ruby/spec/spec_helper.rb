require 'rubygems'
require 'bundler/setup'
require 'monkey'
require 'swagger'
require 'vcr'
require 'typhoeus'
require 'json'
require 'yaml'
require 'rspec'

Dir[File.join(File.dirname(__FILE__), "../lib/*.rb")].each {|file| require file }
Dir[File.join(File.dirname(__FILE__), "../models/*.rb")].each {|file| require file }
Dir[File.join(File.dirname(__FILE__), "../resources/*.rb")].each {|file| require file }

RSpec.configure do |config|
  # some (optional) config here
end

WebMock.allow_net_connect! if defined? WebMock

def help
  puts "\nOh noes! You gotta stuff your swagger credentials in ~/.swagger.yml like so:\n\n"
  puts "api_key: '12345abcdefg'"
  puts "username: 'fumanchu'"
  puts "password: 'kalamazoo'\n\n"
  exit
end

# Parse ~/.swagger.yml for user credentials
begin
  CREDENTIALS = YAML::load_file(File.join(ENV['HOME'], ".swagger.yml")).symbolize_keys
rescue
  help
end

def configure_swagger
  Swagger.configure do |config|
    config.api_key = "special-key"
    config.username = ""
    config.password = ""

    config.host = 'petstore.swagger.wordnik.com'
    config.base_path = '/api'
  end
end

configure_swagger

# A random string to tack onto stuff to ensure we're not seeing 
# data from a previous test run
RAND = ("a".."z").to_a.sample(8).join
