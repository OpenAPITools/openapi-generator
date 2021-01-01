require "../../../spec_helper"

module Ameba::Rule::Lint
  describe SharedVarInFiber do
    subject = SharedVarInFiber.new

    it "doesn't report if there is only local shared var in fiber" do
      s = Source.new %(
        spawn do
          i = 1
          puts i
        end

        Fiber.yield
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if there is only block shared var in fiber" do
      s = Source.new %(
        10.times do |i|
          spawn do
            puts i
          end
        end

        Fiber.yield
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if there a spawn macro is used" do
      s = Source.new %(
        i = 0
        while i < 10
          spawn puts(i)
          i += 1
        end

        Fiber.yield
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is a shared var in spawn" do
      s = Source.new %(
        i = 0
        while i < 10
          spawn do
            puts(i)
          end
          i += 1
        end

        Fiber.yield
      )
      subject.catch(s).should_not be_valid
    end

    it "reports reassigned reference to shared var in spawn" do
      s = Source.new %(
        channel = Channel(String).new
        n = 0

        while n < 10
          n = n + 1
          spawn do
            m = n
            channel.send m
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report reassigned reference to shared var in block" do
      s = Source.new %(
        channel = Channel(String).new
        n = 0

        while n < 3
          n = n + 1
          m = n
          spawn do
            channel.send m
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "does not report block is called in a spawn" do
      s = Source.new %(
        def method(block)
          spawn do
            block.call(10)
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports multiple shared variables in spawn" do
      s = Source.new %(
        foo, bar, baz = 0, 0, 0
        while foo < 10
          baz += 1
          spawn do
            puts foo
            puts foo + bar + baz
          end
          foo += 1
        end
      )
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 3
      s.issues[0].location.to_s.should eq ":5:10"
      s.issues[0].end_location.to_s.should eq ":5:12"
      s.issues[0].message.should eq "Shared variable `foo` is used in fiber"

      s.issues[1].location.to_s.should eq ":6:10"
      s.issues[1].end_location.to_s.should eq ":6:12"
      s.issues[1].message.should eq "Shared variable `foo` is used in fiber"

      s.issues[2].location.to_s.should eq ":6:22"
      s.issues[2].end_location.to_s.should eq ":6:24"
      s.issues[2].message.should eq "Shared variable `baz` is used in fiber"
    end

    it "doesn't report if variable is passed to the proc" do
      s = Source.new %(
        i = 0
        while i < 10
          proc = ->(x : Int32) do
            spawn do
            puts(x)
            end
          end
          proc.call(i)
          i += 1
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if a channel is declared in outer scope" do
      s = Source.new %(
        channel = Channel(Nil).new
        spawn { channel.send(nil) }
        channel.receive
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if there is a loop in spawn" do
      s = Source.new %(
        channel = Channel(String).new

        spawn do
          server = TCPServer.new("0.0.0.0", 8080)
          socket = server.accept
          while line = socket.gets
            channel.send(line)
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if a var is mutated in spawn and referenced outside" do
      s = Source.new %(
        def method
          foo = 1
          spawn { foo = 2 }
          foo
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if variable is changed without iterations" do
      s = Source.new %(
        def foo
          i = 0
          i += 1
          spawn { i }
        end
      ), "source.cr"

      subject.catch(s).should be_valid
    end

    it "doesn't report if variable is in a loop inside spawn" do
      s = Source.new %(
        i = 0
        spawn do
          while i < 10
            i += 1
          end
        end
      )

      subject.catch(s).should be_valid
    end

    it "doesn't report if variable declared inside loop" do
      s = Source.new %(
        while true
          i = 0
          spawn { i += 1 }
        end
      )

      subject.catch(s).should be_valid
    end

    it "reports rule, location and message" do
      s = Source.new %(
        i = 0
        while true
          i += 1
          spawn { i }
        end
      ), "source.cr"

      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:4:11"
      issue.end_location.to_s.should eq "source.cr:4:11"
      issue.message.should eq "Shared variable `i` is used in fiber"
    end
  end
end
