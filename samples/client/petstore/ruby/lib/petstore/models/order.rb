module Petstore
  # 
  class Order < BaseObject
    attr_accessor :id, :pet_id, :quantity, :ship_date, :status, :complete
    # attribute mapping from ruby-style variable name to JSON key
    def self.attribute_map
      {
        
        # 
        :'id' => :'id',
        
        # 
        :'pet_id' => :'petId',
        
        # 
        :'quantity' => :'quantity',
        
        # 
        :'ship_date' => :'shipDate',
        
        # Order Status
        :'status' => :'status',
        
        # 
        :'complete' => :'complete'
        
      }
    end

    # attribute type
    def self.swagger_types
      {
        :'id' => :'Integer',
        :'pet_id' => :'Integer',
        :'quantity' => :'Integer',
        :'ship_date' => :'DateTime',
        :'status' => :'String',
        :'complete' => :'BOOLEAN'
        
      }
    end

    def initialize(attributes = {})
      return if !attributes.is_a?(Hash) || attributes.empty?

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'id']
        self.id = attributes[:'id']
      end
      
      if attributes[:'petId']
        self.pet_id = attributes[:'petId']
      end
      
      if attributes[:'quantity']
        self.quantity = attributes[:'quantity']
      end
      
      if attributes[:'shipDate']
        self.ship_date = attributes[:'shipDate']
      end
      
      if attributes[:'status']
        self.status = attributes[:'status']
      end
      
      if attributes[:'complete']
        self.complete = attributes[:'complete']
      end
      
    end

    def status=(status)
      allowed_values = ["placed", "approved", "delivered"]
      if status && !allowed_values.include?(status)
        fail "invalid value for 'status', must be one of #{allowed_values}"
      end
      @status = status
    end

  end
end
