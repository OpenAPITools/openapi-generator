module Crest
  module ParamsEncoder
    extend self

    alias Type = Nil | String | Array(Type) | Hash(String, Type)

    SUBKEYS_REGEX = /[^\[\]]+(?:\]?\[\])?/
    ARRAY_REGEX   = /[\[\]]+\Z/

    # Converts the given params into a URI querystring. Keys and values
    # will converted to strings and appropriately escaped for the URI.
    #
    # ```
    # Crest::ParamsEncoder.encode({"a" => ["one", "two", "three"], "b" => true, "c" => "C", "d" => 1})
    # # => 'a[]=one&a[]=two&a[]=three&b=true&c=C&d=1'
    # ```
    def encode(params : Hash) : String
      HTTP::Params.build do |form|
        flatten_params(params).each do |name, value|
          form.add(name.to_s, value.to_s)
        end
      end
    end

    # Converts the given URI querystring into a hash.
    #
    # ```
    # Crest::ParamsEncoder.decode("a[]=one&a[]=two&a[]=three&b=true&c=C&d=1")
    # # => {"a" => ["one", "two", "three"], "b" => "true", "c" => "C", "d" => "1"}
    # ```
    def decode(query : String) : Hash
      params = {} of String => Type

      query.split("&").each do |pair|
        key, value = pair.split("=", 2)
        key = URI.decode(key)
        value = URI.decode(value)

        decode_pair(params, key, value)
      end

      params
    end

    private def decode_pair(context, key : String, value : String)
      subkeys = key.scan(SUBKEYS_REGEX)

      subkeys.each_with_index do |subkey, i|
        is_array = false
        is_last_subkey = (i == subkeys.size - 1)
        subkey = subkey[0]

        if match = subkey.match(ARRAY_REGEX)
          is_array = true
          subkey = match.pre_match
        end

        context = prepare_context(context, subkey, is_array, is_last_subkey)
        add_to_context(context, value, subkey) if is_last_subkey
      end
    end

    private def prepare_context(context, subkey : String, is_array : Bool, is_last_subkey : Bool)
      if !is_last_subkey || is_array
        context = new_context(context, subkey, is_array)
      end

      context
    end

    private def new_context(context, subkey : String, is_array : Bool)
      value_type = is_array ? Array(Type) : Hash(String, Type)

      if context.is_a?(Hash)
        context[subkey] ||= value_type.new
      end
    end

    private def add_to_context(context, value : String, subkey : String)
      value = value.empty? ? nil : value

      if context.is_a?(Hash)
        context[subkey] = value
      elsif context.is_a?(Array)
        context << value
      end
    end

    # Transform deeply nested params containers into a flat hash of `key => value`.
    #
    # ```
    # Crest::ParamsEncoder.flatten_params({:key1 => {:key2 => "123"}})
    # # => [{"key1[key2]", "123"}]
    # ```
    def flatten_params(object : Hash, parent_key = nil)
      object.reduce([] of Tuple(String, TextValue | File)) do |memo, item|
        k, v = item

        processed_key = parent_key ? "#{parent_key}[#{k}]" : k.to_s

        case v
        when Hash, Array
          memo += flatten_params(v, processed_key)
        else
          memo << {processed_key, v}
        end

        memo
      end
    end

    # Transform deeply nested params containers into a flat hash of `key => value`.
    #
    # ```
    # Crest::ParamsEncoder.flatten_params({:key1 => {:arr => ["1", "2", "3"]}})
    # # => [{"key1[arr][]", "1"}, {"key1[arr][]", "2"}, {"key1[arr][]", "3"}]
    # ```
    def flatten_params(object : Array, parent_key = nil)
      object.reduce([] of Tuple(String, TextValue | File)) do |memo, item|
        k = :""
        v = item

        processed_key = parent_key ? "#{parent_key}[#{k}]" : k.to_s
        memo << {processed_key, v}

        memo
      end
    end
  end
end
