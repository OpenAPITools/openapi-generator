module Petstore
  class Order < BaseObject
    attr_accessor :id

    attr_accessor :pet_id

    attr_accessor :quantity

    attr_accessor :ship_date

    # Order Status
    attr_accessor :status

    attr_accessor :complete

    # Attribute mapping from ruby-style variable name to JSON key.
    def self.attribute_map
      {
        
        :'id' => :'id',
        
        :'pet_id' => :'petId',
        
        :'quantity' => :'quantity',
        
        :'ship_date' => :'shipDate',
        
        :'status' => :'status',
        
        :'complete' => :'complete'
        
      }
    end

    # Attribute type mapping.
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
      return unless attributes.is_a?(Hash)

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

    # Custom attribute writer method checking allowed values (enum).
    def status=(status)
      allowed_values = ["placed", "approved", "delivered"]
      if status && !allowed_values.include?(status)
        fail "invalid value for 'status', must be one of #{allowed_values}"
      end
      @status = status
    end

    # Check equality by comparing each attribute.
    def ==(o)
      return true if self.equal?(o)
      self.class == o.class &&
          id == o.id &&
          pet_id == o.pet_id &&
          quantity == o.quantity &&
          ship_date == o.ship_date &&
          status == o.status &&
          complete == o.complete
    end

    # @see the `==` method
    def eql?(o)
      self == o
    end

    # Calculate hash code according to all attributes.
    def hash
      [id, pet_id, quantity, ship_date, status, complete].hash
    end
  end
end
