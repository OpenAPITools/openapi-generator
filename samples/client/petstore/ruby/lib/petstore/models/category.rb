module Petstore
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
        :'id' => :'Integer',
        :'name' => :'String'
        
      }
    end

    def initialize(attributes = {})
      return if !attributes.is_a?(Hash) || attributes.empty?

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'id']
        self.id = attributes[:'id']
      end
      
      if attributes[:'name']
        self.name = attributes[:'name']
      end
      
    end

    def ==(o)
      return true if self.equal?(o)
      self.class == o.class &&
          id == o.id &&
          name == o.name
    end

    def eql?(o)
      self == o
    end

    def hash
      [id, name].hash
    end
  end
end
