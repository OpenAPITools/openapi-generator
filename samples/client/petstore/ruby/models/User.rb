class User
  attr_accessor :id, :last_name, :phone, :username, :email, :user_status, :first_name, :password

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :last_name => :lastName, :phone => :phone, :username => :username, :email => :email, :user_status => :userStatus, :first_name => :firstName, :password => :password

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if User.attribute_map["id".to_sym] != nil
        name = "id".to_sym
        value = attributes["id"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["last_name".to_sym] != nil
        name = "last_name".to_sym
        value = attributes["lastName"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["phone".to_sym] != nil
        name = "phone".to_sym
        value = attributes["phone"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["username".to_sym] != nil
        name = "username".to_sym
        value = attributes["username"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["email".to_sym] != nil
        name = "email".to_sym
        value = attributes["email"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["user_status".to_sym] != nil
        name = "user_status".to_sym
        value = attributes["userStatus"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["first_name".to_sym] != nil
        name = "first_name".to_sym
        value = attributes["firstName"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if User.attribute_map["password".to_sym] != nil
        name = "password".to_sym
        value = attributes["password"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      end
  end

  def to_body
    body = {}
    User.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

