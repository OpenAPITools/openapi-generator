class User
  attr_accessor :id, :username, :first_name, :last_name, :email, :password, :phone, :user_status

  # :internal => :external
  def self.attribute_map
    {
      :id => :id,
      :username => :username,
      :first_name => :firstName,
      :last_name => :lastName,
      :email => :email,
      :password => :password,
      :phone => :phone,
      :user_status => :userStatus

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
    if self.class.attribute_map[:"first_name"]
      @first_name = attributes["firstName"]
    end
    if self.class.attribute_map[:"last_name"]
      @last_name = attributes["lastName"]
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
    if self.class.attribute_map[:"user_status"]
      @user_status = attributes["userStatus"]
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

