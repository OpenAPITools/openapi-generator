require "../spec_helper"

module Ameba
  describe Severity do
    describe "#symbol" do
      it "returns the symbol for each severity in the enum" do
        Severity.values.each do |severity|
          severity.symbol.should_not be_nil
        end
      end

      it "returns symbol for Error" do
        Severity::Error.symbol.should eq 'E'
      end

      it "returns symbol for Warning" do
        Severity::Warning.symbol.should eq 'W'
      end

      it "returns symbol for Convention" do
        Severity::Convention.symbol.should eq 'C'
      end
    end

    describe ".parse" do
      it "creates error severity by name" do
        Severity.parse("Error").should eq Severity::Error
      end

      it "creates warning severity by name" do
        Severity.parse("Warning").should eq Severity::Warning
      end

      it "creates convention severity by name" do
        Severity.parse("Convention").should eq Severity::Convention
      end

      it "raises when name is incorrect" do
        expect_raises(Exception, "Incorrect severity name BadName. Try one of [Error, Warning, Convention]") do
          Severity.parse("BadName")
        end
      end
    end
  end

  struct SeverityConvertable
    include YAML::Serializable

    @[YAML::Field(converter: Ameba::SeverityYamlConverter)]
    property severity : Severity
  end

  describe SeverityYamlConverter do
    describe ".from_yaml" do
      it "converts from yaml to Severity::Error" do
        yaml = {severity: "error"}.to_yaml
        converted = SeverityConvertable.from_yaml(yaml)
        converted.severity.should eq Severity::Error
      end

      it "converts from yaml to Severity::Warning" do
        yaml = {severity: "warning"}.to_yaml
        converted = SeverityConvertable.from_yaml(yaml)
        converted.severity.should eq Severity::Warning
      end

      it "converts from yaml to Severity::Convention" do
        yaml = {severity: "convention"}.to_yaml
        converted = SeverityConvertable.from_yaml(yaml)
        converted.severity.should eq Severity::Convention
      end

      it "raises if severity is not a scalar" do
        yaml = {severity: {convention: true}}.to_yaml
        expect_raises(Exception, "Severity must be a scalar") do
          SeverityConvertable.from_yaml(yaml)
        end
      end

      it "raises if severity has a wrong type" do
        yaml = {severity: [1, 2, 3]}.to_yaml
        expect_raises(Exception, "Severity must be a scalar") do
          SeverityConvertable.from_yaml(yaml)
        end
      end
    end

    describe ".to_yaml" do
      it "converts Severity::Error to yaml" do
        yaml = {severity: "error"}.to_yaml
        converted = SeverityConvertable.from_yaml(yaml).to_yaml
        converted.should eq "---\nseverity: Error\n"
      end

      it "converts Severity::Warning to yaml" do
        yaml = {severity: "warning"}.to_yaml
        converted = SeverityConvertable.from_yaml(yaml).to_yaml
        converted.should eq "---\nseverity: Warning\n"
      end

      it "converts Severity::Convention to yaml" do
        yaml = {severity: "convention"}.to_yaml
        converted = SeverityConvertable.from_yaml(yaml).to_yaml
        converted.should eq "---\nseverity: Convention\n"
      end
    end
  end
end
