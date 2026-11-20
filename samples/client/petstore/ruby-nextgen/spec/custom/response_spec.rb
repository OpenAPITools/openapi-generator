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
    expect(response.to_hash).to eq(a: 1)
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
    expect(response.dig(1)).to eq(2)
  end
end
