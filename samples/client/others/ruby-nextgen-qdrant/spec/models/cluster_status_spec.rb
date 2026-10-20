# frozen_string_literal: true

require 'spec_helper'

RSpec.describe Qdrant::Models::ClusterStatus do
  it 'exposes its candidate types' do
    expect(described_class::CANDIDATES).not_to be_empty
  end
end
