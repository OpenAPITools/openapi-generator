# frozen_string_literal: true

require 'spec_helper'

RSpec.describe Petstore::Api::Store::Order do
  let(:client) { Petstore::Client.new(base_url: 'http://petstore.swagger.io/v2') }

  it 'is reachable and shares the client connection' do
    api = described_class.new(client.connection)
    expect(api).to be_a(described_class)
  end
end
