# frozen_string_literal: true

require 'spec_helper'

RSpec.describe Qdrant::Api::Collections::Index do
  let(:client) { Qdrant::Client.new(base_url: 'http://localhost:6333') }

  it 'is reachable and shares the client connection' do
    api = described_class.new(client.connection)
    expect(api).to be_a(described_class)
  end
end
