# frozen_string_literal: true

require 'spec_helper'

# Pins the Date branch of the Polymorphism runtime to RFC 3339 full-date
# strings. Date.parse is a lenient heuristic, so the original Date.parse-based
# branch made a Date candidate of an anyOf/oneOf grab values belonging to
# other branches (the table in the review of PR #25760).
RSpec.describe DatePolymorphism::Polymorphism do
  describe 'format: date deserialization (from_hash)' do
    it 'deserializes a full-date string as a Date' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => '2026-01-02')
      expect(pet.birthday).to eq(Date.new(2026, 1, 2))
    end

    it 'leaves "12" a String' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => '12')
      expect(pet.birthday).to eq('12')
    end

    it 'leaves "may" a String' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => 'may')
      expect(pet.birthday).to eq('may')
    end

    it 'leaves a date-time string a String, time intact' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => '2026-01-02T23:30:00+05:00')
      expect(pet.birthday).to eq('2026-01-02T23:30:00+05:00')
    end

    it 'leaves a calendar-invalid full-date-shaped string a String' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => '2026-13-40')
      expect(pet.birthday).to eq('2026-13-40')
    end

    it 'leaves a non-String input untouched' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => 12)
      expect(pet.birthday).to eq(12)
    end
  end

  describe 'cast' do
    it 'matches a full-date string' do
      expect(described_class.cast('Date', '2026-01-02')).to eq(Date.new(2026, 1, 2))
    end

    it 'does not match values that are not full-dates' do
      expect(described_class.cast('Date', '12')).to be_nil
      expect(described_class.cast('Date', 'may')).to be_nil
      expect(described_class.cast('Date', '3rd Feb 2001')).to be_nil
      expect(described_class.cast('Date', '2026-01-02T23:30:00+05:00')).to be_nil
      expect(described_class.cast('Date', '2026-13-40')).to be_nil
      expect(described_class.cast('Date', nil)).to be_nil
    end
  end

  describe 'DateOrCode.build (anyOf: Date | code)' do
    it 'keeps a valid code a String' do
      expect(DatePolymorphism::Models::DateOrCode.build('mar')).to eq('mar')
    end

    it 'resolves a full-date to a Date' do
      expect(DatePolymorphism::Models::DateOrCode.build('2026-01-02')).to eq(Date.new(2026, 1, 2))
    end
  end

  describe 'DateOrTime.build (oneOf: Date | Time)' do
    it 'resolves a date-time to a Time' do
      expect(DatePolymorphism::Models::DateOrTime.build('2026-05-06T10:00:00Z')).to be_a(Time)
    end

    # Pre-existing behavior, tracked in openapi-generator#25768: the Time
    # branch (Time.parse) also accepts a date-only string, so a date-only
    # payload still matches both candidates and stays ambiguous. Not
    # introduced by the Date branch; the Date candidate alone no longer
    # matches non-dates.
    it 'leaves a date-only string ambiguous' do
      expect { DatePolymorphism::Models::DateOrTime.build('2026-05-06') }
        .to raise_error(ArgumentError, /ambiguous oneOf .* 2 candidates matched/)
    end
  end
end
