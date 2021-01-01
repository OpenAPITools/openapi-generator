require "./spec_helper"

describe "Config" do
  it "sets default port to 3000" do
    Kemal::Config.new.port.should eq 3000
  end

  it "sets default environment to development" do
    Kemal::Config.new.env.should eq "development"
  end

  it "sets environment to production" do
    config = Kemal.config
    config.env = "production"
    config.env.should eq "production"
  end

  it "sets default powered_by_header to true" do
    Kemal::Config.new.powered_by_header.should be_true
  end

  it "sets host binding" do
    config = Kemal.config
    config.host_binding = "127.0.0.1"
    config.host_binding.should eq "127.0.0.1"
  end

  it "adds a custom handler" do
    config = Kemal.config
    config.add_handler CustomTestHandler.new
    Kemal.config.setup
    config.handlers.size.should eq(7)
  end

  it "toggles the shutdown message" do
    config = Kemal.config
    config.shutdown_message = false
    config.shutdown_message.should eq false
    config.shutdown_message = true
    config.shutdown_message.should eq true
  end

  it "adds custom options" do
    config = Kemal.config
    ARGV.push("--test")
    ARGV.push("FOOBAR")
    test_option = nil

    config.extra_options do |parser|
      parser.on("--test TEST_OPTION", "Test an option") do |opt|
        test_option = opt
      end
    end
    Kemal::CLI.new ARGV
    test_option.should eq("FOOBAR")
  end

  it "gets the version from shards.yml" do
    Kemal::VERSION.should_not be("")
  end
end
