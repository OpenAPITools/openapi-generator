class Pet
  attr_accessor :tags, :id, :category, :status, :name, :photo_urls

  # :internal => :external
  def self.attribute_map
  {
      :tags => :tags, :id => :id, :category => :category, :status => :status, :name => :name, :photo_urls => :photoUrls

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if Pet.attribute_map["tags".to_sym] != nil
        name = "tags".to_sym
        value = attributes["tags"]
        if value.is_a?(Array)
	        array = Array.new
	        value.each do |arrayValue|
	          array.push Tag.new(arrayValue)
	        end
	        send("#{name}=", array) if self.respond_to?(name)
	      end
        end
      if Pet.attribute_map["id".to_sym] != nil
        name = "id".to_sym
        value = attributes["id"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Pet.attribute_map["category".to_sym] != nil
        name = "category".to_sym
        value = attributes["category"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Pet.attribute_map["status".to_sym] != nil
        name = "status".to_sym
        value = attributes["status"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Pet.attribute_map["name".to_sym] != nil
        name = "name".to_sym
        value = attributes["name"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if Pet.attribute_map["photo_urls".to_sym] != nil
        name = "photo_urls".to_sym
        value = attributes["photoUrls"]
        if value.is_a?(Array)
	        array = Array.new
	        value.each do |arrayValue|
	          array.push arrayValue
	        end
	        send("#{name}=", array) if self.respond_to?(name)
	      end
        end
      end
  end

  def to_body
    body = {}
    Pet.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

