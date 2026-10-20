# frozen_string_literal: true

module Qdrant
  # Shared serialization for all models. Models declare their attributes via the
  # `attribute` DSL (see Validations); this reads the resulting attribute table.
  module Serializable
    def to_hash
      result = self.class.openapi_attributes.each_with_object({}) do |(name, attr), hash|
        value = public_send(name)
        next if value.nil? && !attr[:required]

        hash[attr[:json_key]] = Serializable.serialize(value)
      end
      if respond_to?(:additional_properties) && additional_properties && !additional_properties.empty?
        # A declared attribute always wins over an overflow key of the same name.
        result.merge!(Serializable.serialize(additional_properties)) { |_key, declared, _extra| declared }
      end
      result
    end

    def to_body
      to_hash
    end

    def to_json(*args)
      to_hash.to_json(*args)
    end

    def to_s
      to_hash.to_s
    end

    def ==(other)
      other.is_a?(self.class) && to_hash == other.to_hash
    end
    alias eql? ==

    def hash
      to_hash.hash
    end

    def self.serialize(value)
      case value
      when Array then value.map { |v| serialize(v) }
      when Hash  then value.transform_values { |v| serialize(v) }
      else value.respond_to?(:to_hash) ? value.to_hash : value
      end
    end
  end
end
