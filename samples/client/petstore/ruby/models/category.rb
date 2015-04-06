
class Category
  attr_accessor :id, :name
  # :internal => :external
  def self.attribute_map
    {
      :id => :'id',
      :name => :'name'
      
    }
  end

  def initialize(attributes = {})
    return if attributes.empty?
    # Morph attribute keys into undescored rubyish style
    
    if self.class.attribute_map[:"id"]
      @id = attributes["id"]
    end
    
    if self.class.attribute_map[:"name"]
      @name = attributes["name"]
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
