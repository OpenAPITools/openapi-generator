# frozen_string_literal: true

require 'spec_helper'

# Hand-written (not generated): a `format: date-time` attribute or anyOf/oneOf
# candidate must only accept ISO 8601 date-time strings (a missing offset means
# local time). Lenient `Time.parse` turned "12", "may" and date-only strings
# into `Time`s, so the `Time` branch swallowed values that belong to its
# sibling candidates.
RSpec.describe 'Polymorphism Time branch' do
  it 'coerces an RFC 3339 date-time string into a Time' do
    order = Petstore::Models::Order.from_hash('shipDate' => '2026-05-06T10:00:00Z')

    expect(order.ship_date).to eq(Time.utc(2026, 5, 6, 10, 0, 0))
  end

  it 'keeps the UTC offset of an RFC 3339 date-time' do
    order = Petstore::Models::Order.from_hash('shipDate' => '2026-05-06T10:00:00+01:00')

    expect(order.ship_date).to eq(Time.new(2026, 5, 6, 10, 0, 0, '+01:00'))
    # Time#== compares instants only, so the offset has to be checked on its own
    expect(order.ship_date.utc_offset).to eq(3600)
  end

  it 'reads a date-time without an offset as local time' do
    order = Petstore::Models::Order.from_hash('shipDate' => '2026-05-06T10:00:00')

    expect(order.ship_date).to eq(Time.new(2026, 5, 6, 10, 0, 0))
  end

  it 'keeps a bare number a String' do
    expect(Petstore::Models::Order.from_hash('shipDate' => '12').ship_date).to eq('12')
  end

  it 'keeps a month name a String' do
    expect(Petstore::Models::Order.from_hash('shipDate' => 'may').ship_date).to eq('may')
  end

  it 'keeps a date-only string a String' do
    expect(Petstore::Models::Order.from_hash('shipDate' => '2026-05-06').ship_date).to eq('2026-05-06')
  end

  it 'does not match the Time candidate for a non date-time string' do
    expect(Petstore::Polymorphism.cast('Time', '12')).to be_nil
    expect(Petstore::Polymorphism.cast('Time', 'may')).to be_nil
    expect(Petstore::Polymorphism.cast('Time', '2026-05-06')).to be_nil
  end

  it 'matches the Time candidate for an RFC 3339 date-time' do
    expect(Petstore::Polymorphism.cast('Time', '2026-05-06T10:00:00Z'))
      .to eq(Time.utc(2026, 5, 6, 10, 0, 0))
  end
end
