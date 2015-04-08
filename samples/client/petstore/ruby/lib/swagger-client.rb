require 'monkey'
require 'swagger'

Dir[File.join(File.dirname(__FILE__), "../lib/*.rb")].each {|file| require file if file !~ /swagger-client\.rb\z/ }
Dir[File.join(File.dirname(__FILE__), "../models/*.rb")].each {|file| require file }
