class Order
  attr_accessor :id, :pet_id, :status, :quantity, :ship_date

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :pet_id => :petId, :status => :status, :quantity => :quantity, :ship_date => :shipDate

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if Order.attribute_map["id".to_sym] != nil
        name = "id".to_sym
        value = attributes["id"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Order.attribute_map["pet_id".to_sym] != nil
        name = "pet_id".to_sym
        value = attributes["petId"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Order.attribute_map["status".to_sym] != nil
        name = "status".to_sym
        value = attributes["status"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Order.attribute_map["quantity".to_sym] != nil
        name = "quantity".to_sym
        value = attributes["quantity"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Order.attribute_map["ship_date".to_sym] != nil
        name = "ship_date".to_sym
        value = attributes["shipDate"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      end
  end

  def to_body
    body = {}
    Order.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

