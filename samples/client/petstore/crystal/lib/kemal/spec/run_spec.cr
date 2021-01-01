require "./spec_helper"

private def run(code)
  code = <<-CR
    require "./src/kemal"
    #{code}
    CR

  stdout = IO::Memory.new
  stderr = IO::Memory.new
  status = Process.new("crystal", ["eval"], input: IO::Memory.new(code), output: stdout, error: stderr).wait
  fail(stderr.to_s) unless status.success?
  stdout.to_s
end

describe "Run" do
  it "runs a code block after starting" do
    run(<<-CR).should eq "started\nstopped\n"
      Kemal.config.env = "test"
      Kemal.run do
        puts "started"
        Kemal.stop
        puts "stopped"
      end
      CR
  end

  it "runs without a block being specified" do
    run(<<-CR).should eq "[test] Kemal is ready to lead at http://0.0.0.0:3000\ntrue\n"
      Kemal.config.env = "test"
      Kemal.run
      puts Kemal.config.running
      CR
  end

  it "allows custom HTTP::Server bind" do
    run(<<-CR).should eq "[test] Kemal is ready to lead at http://127.0.0.1:3000, http://0.0.0.0:3001\n"
      Kemal.config.env = "test"
      Kemal.run do |config|
        server = config.server.not_nil!
        server.bind_tcp "127.0.0.1", 3000, reuse_port: true
        server.bind_tcp "0.0.0.0", 3001, reuse_port: true
      end
      CR
  end
end
