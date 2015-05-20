module SwaggerClient
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
        :'id' => :'int',
        :'category' => :'Category',
        :'name' => :'string',
        :'photo_urls' => :'array[string]',
        :'tags' => :'array[Tag]',
        :'status' => :'string'
        
      }
    end

    def initialize(attributes = {})
      return if !attributes.is_a?(Hash) || attributes.empty?

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'id']
        @id = attributes[:'id']
      end
      
      if attributes[:'category']
        @category = attributes[:'category']
      end
      
      if attributes[:'name']
        @name = attributes[:'name']
      end
      
      if attributes[:'photoUrls']
        if (value = attributes[:'photoUrls']).is_a?(Array)
          @photo_urls = value
        end
      end
      
      if attributes[:'tags']
        if (value = attributes[:'tags']).is_a?(Array)
          @tags = value
        end
      end
      
      if attributes[:'status']
        @status = attributes[:'status']
      end
      
    end
  end
end
