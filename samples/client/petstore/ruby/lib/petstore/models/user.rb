module Petstore
  # 
  class User < BaseObject
    attr_accessor :id, :username, :first_name, :last_name, :email, :password, :phone, :user_status
    # attribute mapping from ruby-style variable name to JSON key
    def self.attribute_map
      {
        
        # 
        :'id' => :'id',
        
        # 
        :'username' => :'username',
        
        # 
        :'first_name' => :'firstName',
        
        # 
        :'last_name' => :'lastName',
        
        # 
        :'email' => :'email',
        
        # 
        :'password' => :'password',
        
        # 
        :'phone' => :'phone',
        
        # User Status
        :'user_status' => :'userStatus'
        
      }
    end

    # attribute type
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
      return if !attributes.is_a?(Hash) || attributes.empty?

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'id']
        @id = attributes[:'id']
      end
      
      if attributes[:'username']
        @username = attributes[:'username']
      end
      
      if attributes[:'firstName']
        @first_name = attributes[:'firstName']
      end
      
      if attributes[:'lastName']
        @last_name = attributes[:'lastName']
      end
      
      if attributes[:'email']
        @email = attributes[:'email']
      end
      
      if attributes[:'password']
        @password = attributes[:'password']
      end
      
      if attributes[:'phone']
        @phone = attributes[:'phone']
      end
      
      if attributes[:'userStatus']
        @user_status = attributes[:'userStatus']
      end
      
    end
  end
end
