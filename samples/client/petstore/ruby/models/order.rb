
class Order
  attr_accessor :id, :pet_id, :quantity, :ship_date, :status, :complete
  # :internal => :external
  def self.attribute_map
    {
      :id => :'id',
      :pet_id => :'petId',
      :quantity => :'quantity',
      :ship_date => :'shipDate',
      :status => :'status',
      :complete => :'complete'
      
    }
  end

  def initialize(attributes = {})
    return if attributes.empty?
    # Morph attribute keys into undescored rubyish style
    
    if self.class.attribute_map[:"id"]
      @id = attributes["id"]
    end
    
    if self.class.attribute_map[:"pet_id"]
      @pet_id = attributes["petId"]
    end
    
    if self.class.attribute_map[:"quantity"]
      @quantity = attributes["quantity"]
    end
    
    if self.class.attribute_map[:"ship_date"]
      @ship_date = attributes["shipDate"]
    end
    
    if self.class.attribute_map[:"status"]
      @status = attributes["status"]
    end
    
    if self.class.attribute_map[:"complete"]
      @complete = attributes["complete"]
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
