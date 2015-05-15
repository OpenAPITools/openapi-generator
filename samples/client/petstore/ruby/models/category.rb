require_relative 'base_object'

#
class Category < BaseObject
  attr_accessor :id, :name
  # attribute mapping from ruby-style variable name to JSON key
  def self.attribute_map
    {
      
      # 
      :'id' => :'id',
      
      # 
      :'name' => :'name'
      
    }
  end

  # attribute type
  def self.swagger_types
    {
      :'id' => :'int',
      :'name' => :'string'
      
    }
  end

  def initialize(attributes = {})
    return if !attributes.is_a?(Hash) || attributes.empty?

    # convert string to symbol for hash key
    attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

    
    if attributes[:'id']
      @id = attributes[:'id']
    end
    
    if attributes[:'name']
      @name = attributes[:'name']
    end
    
  end

#  # return the object in the form of hash
#  def to_body
#    body = {}
#    self.class.attribute_map.each_pair do |key, value|
#      body[value] = self.send(key) unless self.send(key).nil?
#    end
#    body
#  end
#
#  # build the object from hash
#  def build_from_hash(attributes)
#    self.class.swagger_types.each_pair do |key, type|
#      if type =~ /^array\[(.*)\]/i
#        if attributes[self.class.attribute_map[key]].is_a?(Array)
#          self.send("#{key}=", attributes[self.class.attribute_map[key]].map{ |v| _deserialize($1, v) } )
#        else
#          #TODO show warning in debug mode
#        end
#      elsif !attributes[self.class.attribute_map[key]].nil?
#        self.send("#{key}=", _deserialize(type, attributes[self.class.attribute_map[key]]))
#      else
#        # data not found in attributes(hash), not an issue as the data can be optional
#      end
#    end
#
#    self
#  end
#
#  def _deserialize(type, value)
#    case type
#    when :DateTime
#      DateTime.parse(value)
#    when :string
#      value.to_s
#    when :int
#      value.to_i
#    when :double
#      value.to_f
#    when :boolean
#      if value =~ /^(true|t|yes|y|1)$/i
#        true
#      else
#        false
#      end
#    else # model
#      _model = Object.const_get(type).new
#      _model.build_from_hash(value)
#    end
#  end
#
#
#
#  # to_body is an alias to to_body (backward compatibility)
#  def to_hash
#    hash = {}
#    self.class.attribute_map.each_pair do |key, value|
#      if self.send(key).is_a?(Array)
#        next if self.send(key).empty?
#        hash[value] = self.send(key).select{|v| !v.nil?}.map{ |v| _to_hash v} unless self.send(key).nil?
#      else
#        unless (_tmp_value = _to_hash self.send(key)).nil?
#          hash[value] = _tmp_value
#        end
#      end
#    end
#    hash
#  end
#
#  # Method to output non-array value in the form of hash
#  # For object, use to_hash. Otherwise, just return the value
#  def _to_hash(value)
#    if value.respond_to? :to_hash
#      value.to_hash
#    else
#      value
#    end
#  end

end
