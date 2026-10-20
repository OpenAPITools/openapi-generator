# frozen_string_literal: true

require 'spec_helper'

RSpec.describe Qdrant::Models::RecommendInput do
  # from_hash (deserialization path) builds without validating, so it works even for
  # models with required fields -- unlike the strict, validating `new`.
  it 'round-trips through JSON' do
    instance = described_class.from_hash({})
    expect(described_class.from_json(instance.to_json)).to eq(instance)
  end

  it 'reports missing required attributes' do
    required = described_class.openapi_attributes.select { |_, a| a[:required] }.keys
    instance = described_class.from_hash({})
    if required.any?
      expect(instance.list_invalid_properties).not_to be_empty
    else
      expect(instance.list_invalid_properties).to be_empty
    end
  end

  it 'rejects unknown attributes' do
    expect { described_class.new(this_is_not_an_attribute: true) }.to raise_error(ArgumentError)
  end
end
