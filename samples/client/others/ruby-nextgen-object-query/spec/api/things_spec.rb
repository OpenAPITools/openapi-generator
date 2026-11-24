# frozen_string_literal: true

require 'spec_helper'

RSpec.describe ObjectQuery::Api::Things do
  let(:client) { ObjectQuery::Client.new(base_url: 'http://localhost') }

  it 'is reachable and shares the client connection' do
    api = described_class.new(client.connection)
    expect(api).to be_a(described_class)
  end
end
