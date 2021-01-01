require "./ameba/*"
require "./ameba/ast/**"
require "./ameba/rule/**"
require "./ameba/formatter/*"

# Ameba's entry module.
#
# To run the linter with default parameters:
#
# ```
# Ameba.run
# ```
#
# To configure and run it:
#
# ```
# config = Ameba::Config.load
# config.formatter = formatter
# config.files = file_paths
#
# Ameba.run config
# ```
#
module Ameba
  extend self

  VERSION = {{ `shards version "#{__DIR__}"`.chomp.stringify }}

  # Initializes `Ameba::Runner` and runs it.
  # Can be configured via `config` parameter.
  #
  # Examples:
  #
  # ```
  # Ameba.run
  # Ameba.run config
  # ```
  #
  def run(config = Config.load)
    Runner.new(config).run
  end
end
