# frozen_string_literal: true

module Qdrant
  # Runtime support for anyOf/oneOf candidate resolution. Casts raw
  # deserialized data (Hash/Array/primitive) against a declared candidate
  # type name and returns the matching value, or nil when it does not match.
  #
  # Candidate type names are plain OpenAPI-Generator Ruby type declarations
  # (e.g. 'Integer', "Array<String>", "Hash<String, Vector>", or a bare model
  # name resolved under Qdrant::Models). This mirrors the type-name
  # dispatch used by the stock `ruby` generator's `find_and_cast_into_type`,
  # adapted to the idiomatic Models:: namespace and attribute DSL.
  module Polymorphism
    ARRAY_TYPE = /\AArray<(?<sub_type>.+)>\z/
    HASH_TYPE = /\AHash<String, ?(?<sub_type>.+)>\z/

    # Internal sentinel distinguishing "no candidate matched" from a
    # genuinely null value that legitimately matched a nullable candidate
    # (e.g. a nullable array/hash element, or a nullable model reference).
    # `nil` alone is ambiguous for that purpose, so it cannot be reused here.
    NO_MATCH = Object.new.freeze
    private_constant :NO_MATCH

    # Public entry point used by generated anyOf/oneOf wrappers. Returns the
    # matching value, or nil when no candidate matched.
    def self.cast(type_name, data)
      result = cast_raw(type_name, data)
      result.equal?(NO_MATCH) ? nil : result
    end

    def self.cast_raw(type_name, data)
      case type_name
      when 'Boolean'
        [true, false].include?(data) ? data : NO_MATCH
      when 'Integer'
        data.is_a?(Integer) ? data : NO_MATCH
      when 'Float'
        data.is_a?(Numeric) ? data.to_f : NO_MATCH
      when 'String'
        data.is_a?(String) ? data : NO_MATCH
      when 'Time'
        cast_time(data)
      when 'Object', 'Hash'
        data.is_a?(Hash) ? data : NO_MATCH
      when ARRAY_TYPE
        cast_array(Regexp.last_match[:sub_type], data)
      when HASH_TYPE
        cast_hash(Regexp.last_match[:sub_type], data)
      else
        data.nil? ? nil : cast_model(type_name, data)
      end
    end
    private_class_method :cast_raw

    def self.cast_time(data)
      return NO_MATCH unless data.is_a?(String)

      Time.parse(data)
    rescue ArgumentError, TypeError
      NO_MATCH
    end
    private_class_method :cast_time

    def self.cast_array(sub_type, data)
      return NO_MATCH unless data.is_a?(Array)

      cast_items = data.map { |item| cast_raw(sub_type, item) }
      cast_items.any? { |item| item.equal?(NO_MATCH) } ? NO_MATCH : cast_items
    end
    private_class_method :cast_array

    def self.cast_hash(sub_type, data)
      return NO_MATCH unless data.is_a?(Hash)

      cast_pairs = data.transform_values { |value| cast_raw(sub_type, value) }
      cast_pairs.any? { |_k, v| v.equal?(NO_MATCH) } ? NO_MATCH : cast_pairs
    end
    private_class_method :cast_hash

    # Resolves a bare model name under Models:: and dispatches according to
    # what kind of generated constant it is:
    # - a real model class (Serializable/Validations): responds to `from_hash`
    # - an enum module: responds to class-level `valid?(value)`
    # - a nested oneOf/anyOf wrapper module: responds to `build(data)` only
    def self.cast_model(type_name, data)
      return NO_MATCH unless Models.const_defined?(type_name, false)

      const = Models.const_get(type_name, false)
      if const.respond_to?(:from_hash)
        obj = const.from_hash(data)
        obj.valid? ? obj : NO_MATCH
      elsif const.respond_to?(:valid?)
        const.valid?(data) ? data : NO_MATCH
      elsif const.respond_to?(:build)
        const.build(data)
      else
        NO_MATCH
      end
    # A non-matching candidate must not abort the whole anyOf/oneOf resolution: a wrong
    # shape (Array where a Hash is expected, a nested build that raises, ...) simply means
    # "this candidate does not match", so treat any resolution failure as NO_MATCH.
    rescue NameError, TypeError, ArgumentError
      # NameError already covers its subclass NoMethodError.
      NO_MATCH
    end
    private_class_method :cast_model

    # Non-validating structural coercion used by `from_hash` deserialization.
    # Unlike `cast`/`cast_raw` (which gate on `valid?` to pick the right
    # anyOf/oneOf candidate), `coerce` never rejects data: it builds the best
    # typed representation it can and falls back to returning the data
    # unchanged when it cannot resolve a type. This ensures deserialization
    # never silently drops a valid-but-incomplete nested object.
    def self.coerce(type_name, data)
      return nil if data.nil?

      case type_name
      when 'Boolean', 'Integer', 'String', 'Object', 'Hash', nil
        data
      when 'Float'
        data.is_a?(Numeric) ? data.to_f : data
      when 'Time'
        coerce_time(data)
      when ARRAY_TYPE
        sub_type = Regexp.last_match[:sub_type]
        data.is_a?(Array) ? data.map { |item| coerce(sub_type, item) } : data
      when HASH_TYPE
        sub_type = Regexp.last_match[:sub_type]
        data.is_a?(Hash) ? data.transform_values { |value| coerce(sub_type, value) } : data
      else
        coerce_model(type_name, data)
      end
    end

    def self.coerce_time(data)
      return data unless data.is_a?(String)

      begin
        Time.parse(data)
      rescue ArgumentError, TypeError
        data
      end
    end
    private_class_method :coerce_time

    # Resolves a bare model name under Models:: and dispatches according to
    # what kind of generated constant it is, without any `valid?` gating:
    # - a real model class (Serializable/Validations): `from_hash` (recurses)
    # - a nested oneOf/anyOf wrapper module: `build(data)`
    # - an enum module: returns the raw value unchanged
    # - anything unresolvable: returns the data unchanged
    def self.coerce_model(type_name, data)
      return data unless type_name.is_a?(String) && Models.const_defined?(type_name, false)

      const = Models.const_get(type_name, false)
      if const.respond_to?(:from_hash)
        const.from_hash(data)
      elsif const.respond_to?(:build)
        const.build(data)
      else
        data
      end
    rescue NameError
      data
    end
    private_class_method :coerce_model
  end
end
