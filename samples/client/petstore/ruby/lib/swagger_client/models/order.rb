module SwaggerClient
  # 
  class Order < BaseObject
attr_accessor :id, :pet_id, :quantity, :ship_date, :status, :complete
# attribute mapping from ruby-style variable name to JSON key
def self.attribute_map
{
    # 
    :'id' => :'id',
    # 
    :'pet_id' => :'petId',
    # 
    :'quantity' => :'quantity',
    # 
    :'ship_date' => :'shipDate',
    # Order Status
    :'status' => :'status',
    # 
    :'complete' => :'complete'
}
end

# attribute type
def self.swagger_types
{
:'id' => :'int',
:'pet_id' => :'int',
:'quantity' => :'int',
:'ship_date' => :'DateTime',
:'status' => :'string',
:'complete' => :'boolean'
}
end

def initialize(attributes = {})
return if !attributes.is_a?(Hash) || attributes.empty?

# convert string to symbol for hash key
attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

    if attributes[:'id']
    @id = attributes[:'id']
    end
    if attributes[:'petId']
    @pet_id = attributes[:'petId']
    end
    if attributes[:'quantity']
    @quantity = attributes[:'quantity']
    end
    if attributes[:'shipDate']
    @ship_date = attributes[:'shipDate']
    end
    if attributes[:'status']
    @status = attributes[:'status']
    end
    if attributes[:'complete']
    @complete = attributes[:'complete']
    end
end
end
end
