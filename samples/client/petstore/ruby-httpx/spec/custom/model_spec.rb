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
      result3 = Petstore::MammalAnyof.build({"something": 123})
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
      result3 = Petstore::Mammal.build({"something": 123})
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
      result3 = Petstore::MammalWithoutDiscriminator.build({"something": 123})
      expect(result3).to be_nil
    end

  end
end
