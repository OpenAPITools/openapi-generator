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

  describe 'already-typed Date values' do
    # The Integer, Float, String and Boolean arms accept values that are already
    # typed, and to_hash keeps Date objects: the Date arm must accept them too,
    # or a to_hash -> from_hash round trip of a union-typed property fails.
    it 'casts a Date instance as itself' do
      expect(described_class.cast('Date', Date.new(2026, 1, 2))).to eq(Date.new(2026, 1, 2))
    end

    it 'builds an anyOf with a Date arm from a Date instance' do
      expect(DatePolymorphism::Models::DateOrCode.build(Date.new(2026, 1, 2))).to eq(Date.new(2026, 1, 2))
    end

    it 'builds a oneOf with a Date arm from a Date instance' do
      expect(DatePolymorphism::Models::DateOrTime.build(Date.new(2026, 1, 2))).to eq(Date.new(2026, 1, 2))
    end

    it 'keeps a Date through a to_hash -> from_hash round trip of a union-typed property' do
      pet = DatePolymorphism::Models::Pet.from_hash('marked_on' => '2026-01-02')
      expect(DatePolymorphism::Models::Pet.from_hash(pet.to_hash).marked_on).to eq(Date.new(2026, 1, 2))
    end
  end

  describe 'strings the full-date guard cannot inspect' do
    # A String of an unusable encoding makes the guard itself raise. coerce must
    # then fall back to the raw value, as it does for an unparseable one.
    # The unary plus copies the frozen string literal before force_encoding mutates it.
    def invalid_utf8
      (+"\xff").force_encoding(Encoding::UTF_8)
    end

    it 'leaves an invalidly encoded String untouched in from_hash' do
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => invalid_utf8)
      expect(pet.birthday).to eq(invalid_utf8)
    end

    it 'leaves a full-date-shaped String carrying invalid bytes untouched' do
      value = (+"2026-01-02\xfe").force_encoding(Encoding::UTF_8)
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => value)
      expect(pet.birthday).to eq(value)
    end

    it 'leaves a String in an incompatible encoding untouched' do
      value = '2026-01-02'.encode(Encoding::UTF_16LE)
      pet = DatePolymorphism::Models::Pet.from_hash('birthday' => value)
      expect(pet.birthday).to eq(value)
    end

    it 'does not match them in cast' do
      expect(described_class.cast('Date', invalid_utf8)).to be_nil
      expect(described_class.cast('Date', '2026-01-02'.encode(Encoding::UTF_16LE))).to be_nil
    end
  end

  describe 'a payload no union candidate matches' do
    # `coerce` never rejects data: an unmatched anyOf/oneOf payload falls back to
    # the raw value, as a wrong-shaped model payload already does.
    it 'returns the raw payload from coerce instead of raising' do
      expect(described_class.coerce('DateOrTime', 'not a date')).to eq('not a date')
      expect(described_class.coerce('DateOrCode', 12)).to eq(12)
    end

    it 'keeps the raw value of an unmatched union-typed property in from_hash' do
      pet = DatePolymorphism::Models::Pet.from_hash('marked_on' => 12)
      expect(pet.marked_on).to eq(12)
    end
  end
end
