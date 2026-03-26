require 'petstore_helper'
require 'spec_helper'
require 'json'

describe "Mammal" do
  before do
  end

  after do
  end

  describe "anyOf" do
    it "should construct a new anyOf object mammal_anyof" do
      whale = Petstore::Whale.new('classname' => "Whale", 'has_teeth' => true)
      zebra = Petstore::Zebra.new('classname' => "Zebra", 'type' => 'plains')

      # oneOf whale test
      expect(whale.to_hash[:'classname']).to eq("Whale")
      result = Petstore::MammalAnyof.build(whale.to_hash)
      expect(result).to be_a Petstore::Whale

      # oneOf zebra test
      expect(zebra.to_hash[:'classname']).to eq("Zebra")
      result2 = Petstore::MammalAnyof.build(zebra.to_hash)
      expect(result2).to be_a Petstore::Zebra

      # invalid data/hash should result in nil
      result3 = Petstore::MammalAnyof.build({ "something": 123 })
      expect(result3).to be_nil
    end
  end

  describe "oneOf" do
    it "should construct a new oneOf object mammal" do
      whale = Petstore::Whale.new('classname' => "Whale", 'has_teeth' => true)
      zebra = Petstore::Zebra.new('classname' => "Zebra", 'type' => 'plains')

      # oneOf whale test
      expect(whale.to_hash[:'classname']).to eq("Whale")
      result = Petstore::Mammal.build(whale.to_hash)
      expect(result).to be_a Petstore::Whale

      # oneOf zebra test
      expect(zebra.to_hash[:'classname']).to eq("Zebra")
      result2 = Petstore::Mammal.build(zebra.to_hash)
      expect(result2).to be_a Petstore::Zebra

      # invalid data/hash should result in nil
      result3 = Petstore::Mammal.build({ "something": 123 })
      expect(result3).to be_nil
    end

    it "should construct a new oneOf object mammal_without_discriminator" do
      whale = Petstore::Whale.new('classname' => "Whale", 'has_teeth' => true)
      zebra = Petstore::Zebra.new('classname' => "Zebra", 'type' => 'plains')

      # oneOf whale test
      expect(whale.to_hash[:'classname']).to eq("Whale")
      result = Petstore::MammalWithoutDiscriminator.build(whale.to_hash)
      expect(result).to be_a Petstore::Whale

      # oneOf zebra test
      expect(zebra.to_hash[:'classname']).to eq("Zebra")
      result2 = Petstore::MammalWithoutDiscriminator.build(zebra.to_hash)
      expect(result2).to be_a Petstore::Zebra

      # invalid data/hash should result in nil
      result3 = Petstore::MammalWithoutDiscriminator.build({ "something": 123 })
      expect(result3).to be_nil
    end

  end
end

describe 'OneOfPrimitiveTypes' do
  it 'should be able to parse integer' do
    int_value = Petstore::OneOfPrimitiveTypes.build(
      123
    )
    expect(int_value).to eq 123
  end
  it 'should be able to parse float' do
    float_value = Petstore::OneOfPrimitiveTypes.build(
      123.45
    )
    expect(float_value).to eq 123.45
  end
  it 'should be able to parse string' do
    string_value = Petstore::OneOfPrimitiveTypes.build(
      'ABC1'
    )
    expect(string_value).to eq 'ABC1'
  end
  it 'should be able to parse date' do
    date_value = Petstore::OneOfPrimitiveTypes.build(
      '2020-01-02'
    )
    expect(date_value).to be_a Date
    expect(date_value).to eq Date.iso8601('2020-01-02')
  end
  it 'should be able to parse date time' do
    datetime_value = Petstore::OneOfPrimitiveTypes.build(
      '2020-01-02T12:34:56Z'
    )
    expect(datetime_value).to eq Date.iso8601('2020-01-02T12:34:56Z')
  end
  it 'should be able to parse boolean' do
    false_value = Petstore::OneOfPrimitiveTypes.build(
      false
    )
    expect(false_value).to eq false

    true_value = Petstore::OneOfPrimitiveTypes.build(
      true
    )
    expect(true_value).to eq true
  end
  it 'should be able to parse array of integer' do
    array_value = Petstore::OneOfPrimitiveTypes.build(
      [1, 2, 3]
    )
    expect(array_value).to be_a Array
    expect(array_value).to eq [1, 2, 3]
  end
  it 'should be able to parse array of string' do
    array_value = Petstore::OneOfPrimitiveTypes.build(
      %w[ABC1 DEF2 GHI3]
    )
    expect(array_value).to be_a Array
    expect(array_value).to eq %w[ABC1 DEF2 GHI3]
  end
  it 'should be able to parse array of float' do
    array_value = Petstore::OneOfPrimitiveTypes.build(
      [1.1, 2.2, 3.3]
    )
    expect(array_value).to be_a Array
    expect(array_value).to eq [1.1, 2.2, 3.3]
  end
  it 'should be able to parse array of date' do
    array_value = Petstore::OneOfPrimitiveTypes.build(
      %w[2020-01-02 2020-03-04]
    )
    expect(array_value).to be_a Array
    expect(array_value).to eq [Date.iso8601('2020-01-02'), Date.iso8601('2020-03-04')]
  end
  it 'should be able to parse an invalid date' do
    invalid_date_value = Petstore::OneOfPrimitiveTypes.build(
      '2020-13-02'
    )
    expect(invalid_date_value).to eq '2020-13-02'
  end
  it 'should be able to parse array of date time' do
    array_value = Petstore::OneOfPrimitiveTypes.build(
      %w[2020-01-02T12:34:56Z 2020-03-04T01:23:45Z]
    )
    expect(array_value).to be_a Array
    expect(array_value).to eq [Date.parse('2020-01-02T12:34:56Z'), Date.parse('2020-03-04T01:23:45Z')]
  end
  it 'should be able to parse array of boolean' do
    array_value = Petstore::OneOfPrimitiveTypes.build(
      [true, false, true]
    )
    expect(array_value).to be_a Array
    expect(array_value).to eq [true, false, true]
  end
  it 'should return nil for unsupported type' do
    unsupported_value = Petstore::OneOfPrimitiveTypes.build(
      { foo: 'bar' }
    )
    expect(unsupported_value).to be_nil
  end
end
