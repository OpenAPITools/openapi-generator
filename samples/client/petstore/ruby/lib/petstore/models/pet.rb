module Petstore
  # 
  class Pet < BaseObject
    attr_accessor :id, :category, :name, :photo_urls, :tags, :status
    # attribute mapping from ruby-style variable name to JSON key
    def self.attribute_map
      {
        
        # 
        :'id' => :'id',
        
        # 
        :'category' => :'category',
        
        # 
        :'name' => :'name',
        
        # 
        :'photo_urls' => :'photoUrls',
        
        # 
        :'tags' => :'tags',
        
        # pet status in the store
        :'status' => :'status'
        
      }
    end

    # attribute type
    def self.swagger_types
      {
        :'id' => :'Integer',
        :'category' => :'Category',
        :'name' => :'String',
        :'photo_urls' => :'Array<String>',
        :'tags' => :'Array<Tag>',
        :'status' => :'String'
        
      }
    end

    def initialize(attributes = {})
      return if !attributes.is_a?(Hash) || attributes.empty?

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'id']
        self.id = attributes[:'id']
      end
      
      if attributes[:'category']
        self.category = attributes[:'category']
      end
      
      if attributes[:'name']
        self.name = attributes[:'name']
      end
      
      if attributes[:'photoUrls']
        if (value = attributes[:'photoUrls']).is_a?(Array)
          self.photo_urls = value
        end
      end
      
      if attributes[:'tags']
        if (value = attributes[:'tags']).is_a?(Array)
          self.tags = value
        end
      end
      
      if attributes[:'status']
        self.status = attributes[:'status']
      end
      
    end

    def status=(status)
      allowed_values = ["available", "pending", "sold"]
      if status && !allowed_values.include?(status)
        fail "invalid value for 'status', must be one of #{allowed_values}"
      end
      @status = status
    end

    def ==(o)
      return true if self.equal?(o)
      self.class == o.class &&
          id == o.id &&
          category == o.category &&
          name == o.name &&
          photo_urls == o.photo_urls &&
          tags == o.tags &&
          status == o.status
    end

    def eql?(o)
      self == o
    end

    def hash
      [id, category, name, photo_urls, tags, status].hash
    end
  end
end
