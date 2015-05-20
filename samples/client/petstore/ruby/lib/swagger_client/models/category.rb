module SwaggerClient
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
  end
end
