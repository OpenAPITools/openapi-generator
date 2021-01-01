require "../src/ameba"
require "benchmark"

private def get_files(n)
  Dir["src/**/*.cr"].first(n)
end

puts "== Compare:"
Benchmark.ips do |x|
  [
    1,
    3,
    5,
    10,
    20,
    30,
    40,
  ].each do |n|
    config = Ameba::Config.load
    config.formatter = Ameba::Formatter::BaseFormatter.new
    config.globs = get_files(n)
    s = n == 1 ? "" : "s"
    x.report("#{n} source#{s}") { Ameba.run config }
  end
end

puts "== Measure:"
config = Ameba::Config.load
config.formatter = Ameba::Formatter::BaseFormatter.new
puts Benchmark.measure { Ameba.run config }
