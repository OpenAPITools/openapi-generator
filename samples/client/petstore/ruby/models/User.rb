
class User
  attr_accessor :id, :username, :firstName, :lastName, :email, :password, :phone, :userStatus
  # :internal => :external
  def self.attribute_map
    {
      
      :id => :id,
      
      :username => :username,
      
      :firstName => :firstName,
      
      :lastName => :lastName,
      
      :email => :email,
      
      :password => :password,
      
      :phone => :phone,
      
      :userStatus => :userStatus
      
    }
  end

  def initialize(attributes = {})
    return if attributes.empty?
    # Morph attribute keys into undescored rubyish style
    
    if self.class.attribute_map[:"id"]
      
      @id = attributes["id"]
      
    end
    
    if self.class.attribute_map[:"username"]
      
      @username = attributes["username"]
      
    end
    
    if self.class.attribute_map[:"firstName"]
      
      @firstName = attributes["firstName"]
      
    end
    
    if self.class.attribute_map[:"lastName"]
      
      @lastName = attributes["lastName"]
      
    end
    
    if self.class.attribute_map[:"email"]
      
      @email = attributes["email"]
      
    end
    
    if self.class.attribute_map[:"password"]
      
      @password = attributes["password"]
      
    end
    
    if self.class.attribute_map[:"phone"]
      
      @phone = attributes["phone"]
      
    end
    
    if self.class.attribute_map[:"userStatus"]
      
      @userStatus = attributes["userStatus"]
      
    end
    
  end

  def to_body
    body = {}
    self.class.attribute_map.each_pair do |key, value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
