# frozen_string_literal: true

module Petstore
  # Serializes object query parameters -- a Hash or a generated model -- according to their
  # OpenAPI `style` and `explode`. Each method returns the query entries the parameter
  # contributes, as a Hash; an omitted (nil) or empty parameter contributes none.
  module QueryParams
    module_function

    # `style: form, explode: true`: every property is a query entry of its own
    # (`?tld=com&size=big`) and the parameter name does not appear.
    def explode(name, value)
      properties(name, value)
    end

    # `style: form, explode: false`: one entry under the parameter name, holding the
    # property names and values in turn (`?filter=tld,com,size,big`). OpenAPI defines no
    # such form for a nested object or array, so those are rejected.
    def join(name, value)
      pairs = properties(name, value)
      pairs.each do |key, property|
        next unless property.is_a?(Hash) || property.is_a?(Array)

        raise ArgumentError, "#{name}.#{key} must be a scalar with explode: false, got #{property.class}"
      end
      pairs.empty? ? {} : { name => pairs.flatten.join(',') }
    end

    # `style: deepObject`: the properties stay nested under the parameter name, which
    # Faraday renders with brackets (`?filter[tld]=com`).
    def deep_object(name, value)
      pairs = properties(name, value)
      pairs.empty? ? {} : { name => pairs }
    end

    # The non-nil properties of `value` (`plain` drops the nil ones), keyed by String. A model is read through its
    # `to_hash`, so its declared attributes come out under their JSON names; so are the
    # models nested in the properties.
    def properties(name, value)
      return {} if value.nil?

      hash = plain(value)
      raise ArgumentError, "#{name} must be a Hash or a model, got #{value.class}" unless hash.is_a?(Hash)

      hash.transform_keys(&:to_s)
    end

    # `value` with every model in it, at any depth, replaced by its `to_hash`, and the nil
    # entries of every Hash dropped. A value whose `to_hash` is not a Hash is left as it is.
    def plain(value)
      return value.map { |item| plain(item) } if value.is_a?(Array)
      return value unless value.respond_to?(:to_hash)

      hash = value.to_hash
      return value unless hash.is_a?(Hash)

      hash.compact.transform_values { |item| plain(item) }
    end
  end
end
