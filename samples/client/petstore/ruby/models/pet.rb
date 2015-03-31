
class Pet
  attr_accessor :id, :category, :name, :photo_urls, :tags, :status
  # :internal => :external
  def self.attribute_map
    {
      :id => :'id',
      :category => :'category',
      :name => :'name',
      :photo_urls => :'photoUrls',
      :tags => :'tags',
      :status => :'status'
      
    }
  end

  def initialize(attributes = {})
    return if attributes.empty?
    # Morph attribute keys into undescored rubyish style
    
    if self.class.attribute_map[:"id"]
      @id = attributes["id"]
    end
    
    if self.class.attribute_map[:"category"]
      @category = attributes["category"]
    end
    
    if self.class.attribute_map[:"name"]
      @name = attributes["name"]
    end
    
    if self.class.attribute_map[:"photo_urls"]
      if (value = attributes["photoUrls"]).is_a?(Array)
        @photo_urls = value
      end
    end
    
    if self.class.attribute_map[:"tags"]
      if (value = attributes["tags"]).is_a?(Array)
        @tags = value.map{ |v| Tag.new(v) }
      end
    end
    
    if self.class.attribute_map[:"status"]
      @status = attributes["status"]
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
