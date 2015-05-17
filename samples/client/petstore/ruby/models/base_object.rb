# base class containing fundamental method such as to_hash, build_from_hash and more
class BaseObject

  # return the object in the form of hash
  def to_body
    body = {}
    self.class.attribute_map.each_pair do |key, value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end

  # build the object from hash
  def build_from_hash(attributes)
    return nil unless attributes.is_a?(Hash)
    self.class.swagger_types.each_pair do |key, type|
      if type =~ /^array\[(.*)\]/i
        if attributes[self.class.attribute_map[key]].is_a?(Array)
          self.send("#{key}=", attributes[self.class.attribute_map[key]].map{ |v| _deserialize($1, v) } )
        else
          #TODO show warning in debug mode
        end
      elsif !attributes[self.class.attribute_map[key]].nil?
        self.send("#{key}=", _deserialize(type, attributes[self.class.attribute_map[key]]))
      else
        # data not found in attributes(hash), not an issue as the data can be optional
      end
    end

    self
  end

  def _deserialize(type, value)
    case type.to_sym
    when :DateTime
      DateTime.parse(value)
    when :string
      value.to_s
    when :int
      value.to_i
    when :double
      value.to_f
    when :boolean
      if value =~ /^(true|t|yes|y|1)$/i
        true
      else
        false
      end
    else # model
      _model = Object.const_get(type).new
      _model.build_from_hash(value)
    end
  end



  # to_body is an alias to to_body (backward compatibility)
  def to_hash
    hash = {}
    self.class.attribute_map.each_pair do |key, value|
      if self.send(key).is_a?(Array)
        next if self.send(key).empty?
        hash[value] = self.send(key).select{|v| !v.nil?}.map{ |v| _to_hash v} unless self.send(key).nil?
      else
        unless (_tmp_value = _to_hash self.send(key)).nil?
          hash[value] = _tmp_value
        end
      end
    end
    hash
  end

  # Method to output non-array value in the form of hash
  # For object, use to_hash. Otherwise, just return the value
  def _to_hash(value)
    if value.respond_to? :to_hash
      value.to_hash
    else
      value
    end
  end

end
