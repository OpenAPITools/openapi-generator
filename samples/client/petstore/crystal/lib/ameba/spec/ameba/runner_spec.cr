require "../spec_helper"

module Ameba
  private def runner(files = [__FILE__], formatter = DummyFormatter.new)
    config = Config.load
    config.formatter = formatter
    config.globs = files

    config.update_rule ErrorRule.rule_name, enabled: false

    Runner.new(config)
  end

  describe Runner do
    formatter = DummyFormatter.new
    default_severity = Severity::Convention

    describe "#run" do
      it "returns self" do
        runner.run.should_not be_nil
      end

      it "calls started callback" do
        runner(formatter: formatter).run
        formatter.started_sources.should_not be_nil
      end

      it "calls finished callback" do
        runner(formatter: formatter).run
        formatter.finished_sources.should_not be_nil
      end

      it "calls source_started callback" do
        runner(formatter: formatter).run
        formatter.started_source.should_not be_nil
      end

      it "calls source_finished callback" do
        runner(formatter: formatter).run
        formatter.finished_source.should_not be_nil
      end

      it "skips rule check if source is excluded" do
        path = "source.cr"
        source = Source.new "", path

        all_rules = ([] of Rule::Base).tap do |rules|
          rule = ErrorRule.new
          rule.excluded = [path]
          rules << rule
        end

        Runner.new(all_rules, [source], formatter, default_severity).run.success?.should be_true
      end

      context "exception in rule" do
        it "raises an exception raised in fiber while running a rule" do
          rule = RaiseRule.new
          rule.should_raise = true
          rules = [rule] of Rule::Base
          source = Source.new "", "source.cr"

          expect_raises(Exception, "something went wrong") do
            Runner.new(rules, [source], formatter, default_severity).run
          end
        end
      end

      context "invalid syntax" do
        it "reports a syntax error" do
          rules = [Rule::Lint::Syntax.new] of Rule::Base
          source = Source.new "def bad_syntax"

          Runner.new(rules, [source], formatter, default_severity).run
          source.should_not be_valid
          source.issues.first.rule.name.should eq Rule::Lint::Syntax.rule_name
        end

        it "does not run other rules" do
          rules = [Rule::Lint::Syntax.new, Rule::Style::ConstantNames.new] of Rule::Base
          source = Source.new %q(
              MyBadConstant = 1

              when my_bad_syntax
          )

          Runner.new(rules, [source], formatter, default_severity).run
          source.should_not be_valid
          source.issues.size.should eq 1
        end
      end

      context "unneeded disables" do
        it "reports an issue if such disable exists" do
          rules = [Rule::Lint::UnneededDisableDirective.new] of Rule::Base
          source = Source.new %(
            a = 1 # ameba:disable LineLength
          )

          Runner.new(rules, [source], formatter, default_severity).run
          source.should_not be_valid
          source.issues.first.rule.name.should eq Rule::Lint::UnneededDisableDirective.rule_name
        end
      end
    end

    describe "#explain" do
      io = IO::Memory.new

      it "writes nothing if sources are valid" do
        io.clear
        runner = runner(formatter: formatter).run
        runner.explain({file: "source.cr", line: 1, column: 2}, io)
        io.to_s.should be_empty
      end

      it "writes the explanation if sources are not valid and location found" do
        io.clear
        rules = [ErrorRule.new] of Rule::Base
        source = Source.new %(
            a = 1
          ), "source.cr"

        runner = Runner.new(rules, [source], formatter, default_severity).run
        runner.explain({file: "source.cr", line: 1, column: 1}, io)
        io.to_s.should_not be_empty
      end

      it "writes nothing if sources are not valid and location is not found" do
        io.clear
        rules = [ErrorRule.new] of Rule::Base
        source = Source.new %(
            a = 1
          ), "source.cr"

        runner = Runner.new(rules, [source], formatter, default_severity).run
        runner.explain({file: "source.cr", line: 1, column: 2}, io)
        io.to_s.should be_empty
      end
    end

    describe "#success?" do
      it "returns true if runner has not been run" do
        runner.success?.should be_true
      end

      it "returns true if all sources are valid" do
        runner.run.success?.should be_true
      end

      it "returns false if there are invalid sources" do
        rules = Rule.rules.map &.new.as(Rule::Base)
        s = Source.new %q(
          WrongConstant = 5
        )
        Runner.new(rules, [s], formatter, default_severity).run.success?.should be_false
      end

      it "depends on the level of severity" do
        rules = Rule.rules.map &.new.as(Rule::Base)
        s = Source.new %q(WrongConstant = 5)
        Runner.new(rules, [s], formatter, Severity::Error).run.success?.should be_true
        Runner.new(rules, [s], formatter, Severity::Warning).run.success?.should be_true
        Runner.new(rules, [s], formatter, Severity::Convention).run.success?.should be_false
      end

      it "returns false if issue is disabled" do
        rules = [NamedRule.new] of Rule::Base
        source = Source.new %(
          def foo
            bar = 1 # ameba:disable #{NamedRule.name}
          end
        )
        source.add_issue NamedRule.new, location: {2, 1},
          message: "Useless assignment"

        Runner
          .new(rules, [source], formatter, default_severity)
          .run.success?.should be_true
      end
    end
  end
end
