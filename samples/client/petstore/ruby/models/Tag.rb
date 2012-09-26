class Tag
  attr_accessor :id, :name

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :name => :name

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if Tag.attribute_map["id".to_sym] != nil
        name = "id".to_sym
        value = attributes["id"]

	      if value.is_a?(Array)
	        array = Array.new
	        value.each do |arrayValue|
	          array.push arrayValue
	        end
	        send("#{name}=", array) if self.respond_to?(name)
	      else
	        send("#{name}=", value) if self.respond_to?(name)
	      end
	    end
      if Tag.attribute_map["name".to_sym] != nil
        name = "name".to_sym
        value = attributes["name"]

	      if value.is_a?(Array)
	        array = Array.new
	        value.each do |arrayValue|
	          array.push arrayValue
	        end
	        send("#{name}=", array) if self.respond_to?(name)
	      else
	        send("#{name}=", value) if self.respond_to?(name)
	      end
	    end
      end
  end

  def to_body
    body = {}
    Tag.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

