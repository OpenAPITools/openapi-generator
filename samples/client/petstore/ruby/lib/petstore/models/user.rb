module Petstore
  class User < BaseObject
    attr_accessor :id

    attr_accessor :username

    attr_accessor :first_name

    attr_accessor :last_name

    attr_accessor :email

    attr_accessor :password

    attr_accessor :phone

    # User Status
    attr_accessor :user_status

    # Attribute mapping from ruby-style variable name to JSON key.
    def self.attribute_map
      {
        
        :'id' => :'id',
        
        :'username' => :'username',
        
        :'first_name' => :'firstName',
        
        :'last_name' => :'lastName',
        
        :'email' => :'email',
        
        :'password' => :'password',
        
        :'phone' => :'phone',
        
        :'user_status' => :'userStatus'
        
      }
    end

    # Attribute type mapping.
    def self.swagger_types
      {
        :'id' => :'Integer',
        :'username' => :'String',
        :'first_name' => :'String',
        :'last_name' => :'String',
        :'email' => :'String',
        :'password' => :'String',
        :'phone' => :'String',
        :'user_status' => :'Integer'
        
      }
    end

    def initialize(attributes = {})
      return unless attributes.is_a?(Hash)

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'id']
        self.id = attributes[:'id']
      end
      
      if attributes[:'username']
        self.username = attributes[:'username']
      end
      
      if attributes[:'firstName']
        self.first_name = attributes[:'firstName']
      end
      
      if attributes[:'lastName']
        self.last_name = attributes[:'lastName']
      end
      
      if attributes[:'email']
        self.email = attributes[:'email']
      end
      
      if attributes[:'password']
        self.password = attributes[:'password']
      end
      
      if attributes[:'phone']
        self.phone = attributes[:'phone']
      end
      
      if attributes[:'userStatus']
        self.user_status = attributes[:'userStatus']
      end
      
    end

    # Check equality by comparing each attribute.
    def ==(o)
      return true if self.equal?(o)
      self.class == o.class &&
          id == o.id &&
          username == o.username &&
          first_name == o.first_name &&
          last_name == o.last_name &&
          email == o.email &&
          password == o.password &&
          phone == o.phone &&
          user_status == o.user_status
    end

    # @see the `==` method
    def eql?(o)
      self == o
    end

    # Calculate hash code according to all attributes.
    def hash
      [id, username, first_name, last_name, email, password, phone, user_status].hash
    end
  end
end
