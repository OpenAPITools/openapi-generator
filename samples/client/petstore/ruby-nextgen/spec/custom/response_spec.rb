# frozen_string_literal: true

require 'spec_helper'

RSpec.describe Petstore::Response do
  it 'does not advertise to_hash when data has none' do
    response = described_class.new(data: [1], status: 200, headers: {})
    expect(response.respond_to?(:to_hash)).to be(false)
    expect { response.to_hash }.to raise_error(NoMethodError)
  end

  it 'does not advertise to_hash when data is nil' do
    response = described_class.new(data: nil, status: 200, headers: {})
    expect(response.respond_to?(:to_hash)).to be(false)
    expect { response.to_hash }.to raise_error(NoMethodError)
  end

  it 'delegates to_hash when data supports it' do
    response = described_class.new(data: { a: 1 }, status: 200, headers: {})
    expect(response.respond_to?(:to_hash)).to be(true)
    expect(response.respond_to?(:to_hash, true)).to be(true)
    expect(response.to_hash).to eq(a: 1)
  end

  it 'does not advertise to_hash when it is not public on data' do
    guarded = Class.new do
      def to_hash
        { a: 1 }
      end
      protected :to_hash
    end
    hidden = Class.new do
      def to_hash
        { a: 1 }
      end
      private :to_hash
    end

    [guarded.new, hidden.new].each do |data|
      response = described_class.new(data: data, status: 200, headers: {})
      expect(response.respond_to?(:to_hash)).to be(false)
      expect(response.respond_to?(:to_hash, true)).to be(false)
      expect { response.to_hash }.to raise_error(NoMethodError)
    end
  end

  it 'keeps to_h working regardless of the data type' do
    expect(described_class.new(data: [1, 2], status: 200, headers: {}).to_h).to eq([1, 2])
    expect(described_class.new(data: { a: 1 }, status: 200, headers: {}).to_h).to eq(a: 1)
    expect(described_class.new(data: nil, status: 200, headers: {}).to_h).to be_nil
  end

  it 'keeps the other delegators working for array data' do
    response = described_class.new(data: [1, 2], status: 200, headers: {})
    expect(response.map { |v| v * 2 }).to eq([2, 4])
    expect(response[0]).to eq(1)
    expect(response[1]).to eq(2)

    nested = described_class.new(data: [[1], [2, 3]], status: 200, headers: {})
    expect(nested.dig(1, 0)).to eq(2)
  end
end
