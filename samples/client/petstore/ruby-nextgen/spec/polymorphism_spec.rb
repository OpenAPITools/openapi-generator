# frozen_string_literal: true

require 'spec_helper'

RSpec.describe Petstore::Polymorphism do
  describe '.cast' do
    it 'keeps nil array elements for primitive candidates' do
      expect(described_class.cast('Array<Integer>', [1, nil])).to eq([1, nil])
      expect(described_class.cast('Array<String>', ['a', nil])).to eq(['a', nil])
      expect(described_class.cast('Array<Boolean>', [true, nil])).to eq([true, nil])
    end

    it 'keeps nil hash values for primitive candidates' do
      expect(described_class.cast('Hash<String, Integer>', { 'a' => 1, 'b' => nil })).to eq('a' => 1, 'b' => nil)
    end

    it 'keeps nil array elements for model candidates' do
      pet = { 'id' => 1, 'name' => 'Rex', 'photoUrls' => [] }
      result = described_class.cast('Array<Pet>', [pet, nil])
      expect(result.size).to eq(2)
      expect(result[0]).to be_a(Petstore::Models::Pet)
      expect(result[0].name).to eq('Rex')
      expect(result[1]).to be_nil
    end

    it 'still rejects elements that do not match the candidate' do
      expect(described_class.cast('Array<Integer>', [1, 'nope'])).to be_nil
      expect(described_class.cast('Array<Integer>', 'nope')).to be_nil
      expect(described_class.cast('Hash<String, Integer>', { 'a' => 'nope' })).to be_nil
      expect(described_class.cast('Array<Pet>', [{}, nil])).to be_nil
    end

    it 'matches nil data on every candidate type' do
      expect(described_class.cast('Integer', nil)).to be_nil
      expect(described_class.cast('String', nil)).to be_nil
      expect(described_class.cast('Pet', nil)).to be_nil
    end
  end
end
