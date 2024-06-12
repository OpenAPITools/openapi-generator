require 'spec_helper'

class ArrayMapObject < Petstore::Category
  attr_accessor :int_arr, :pet_arr, :int_map, :pet_map, :int_arr_map, :pet_arr_map, :boolean_true_arr, :boolean_false_arr

  def self.attribute_map
    {
      :int_arr => :int_arr,
      :pet_arr => :pet_arr,
      :int_map => :int_map,
      :pet_map => :pet_map,
      :int_arr_map => :int_arr_map,
      :pet_arr_map => :pet_arr_map,
      :boolean_true_arr  => :boolean_true_arr,
      :boolean_false_arr => :boolean_false_arr,
    }
  end

  def self.openapi_types
    {
      :int_arr => :'Array<Integer>',
      :pet_arr => :'Array<Pet>',
      :int_map => :'Hash<String, Integer>',
      :pet_map => :'Hash<String, Pet>',
      :int_arr_map => :'Hash<String, Array<Integer>>',
      :pet_arr_map => :'Hash<String, Array<Pet>>',
      :boolean_true_arr  => :'Array<Boolean>',
      :boolean_false_arr => :'Array<Boolean>',
    }
  end

  def initialize(attributes = {})
    if (!attributes.is_a?(Hash))
      fail ArgumentError, "The input argument (attributes) must be a hash in `ArrayMapObject` initialize method"
    end

    # check to see if the attribute exists and convert string to symbol for hash key
    attributes = attributes.each_with_object({}) { |(k, v), h|
      if (!self.class.attribute_map.key?(k.to_sym))
        fail ArgumentError, "`#{k}` is not a valid attribute in `ArrayMapObject`. Please check the name to make sure it's valid. List of attributes: " + self.class.attribute_map.keys.inspect
      end
      h[k.to_sym] = v
    }

    if attributes.key?(:'int_arr')
      if (value = attributes[:'int_arr']).is_a?(Array)
        self.int_arr = value
      end
    end

    if attributes.key?(:'pet_arr')
      if (value = attributes[:'pet_arr']).is_a?(Array)
        self.pet_arr = value
      end
    end

    if attributes.key?(:'int_map')
      if (value = attributes[:'int_map']).is_a?(Hash)
        self.int_map = value
      end
    end

    if attributes.key?(:'pet_map')
      if (value = attributes[:'pet_map']).is_a?(Hash)
        self.pet_map = value
      end
    end

    if attributes.key?(:'int_arr_map')
      if (value = attributes[:'int_arr_map']).is_a?(Hash)
        self.int_arr_map = value
      end
    end

    if attributes.key?(:'pet_arr_map')
      if (value = attributes[:'pet_arr_map']).is_a?(Hash)
        self.pet_arr_map = value
      end
    end

    if attributes.key?(:'boolean_true_arr')
      if (value = attributes[:'boolean_true_arr']).is_a?(Array)
        self.boolean_true_arr = value
      end
    end

    if attributes.key?(:'boolean_false_arr')
      if (value = attributes[:'boolean_false_arr']).is_a?(Array)
        self.boolean_false_arr = value
      end
    end
  end
end

describe 'BaseObject' do
  describe 'boolean values' do
    let(:obj) { Petstore::Cat.new(declawed: false) }

    it 'should have values set' do
      expect(obj.declawed).not_to be_nil
      expect(obj.declawed).to eq(false)
    end
  end

  describe 'array and map properties' do
    let(:data) do
      { int_arr: [123, 456],
       pet_arr: [{ name: 'Kitty', photoUrls: ['www.photo-url.test'] }],
       int_map: { 'int' => 123 },
       pet_map: { 'pet' => { name: 'Kitty', photoUrls: ['www.photo-url.test'] } },
       int_arr_map: { 'int_arr' => [123, 456] },
       pet_arr_map: { 'pet_arr' => [{ name: 'Kitty', photoUrls: ['www.photo-url.test'] }] },
       boolean_true_arr:  [true, "true", "TruE", 1, "y", "yes", "1", "t", "T"],
       boolean_false_arr: [false, "", 0, "0", "f", nil, "null", "\ntrue\n"],
      }
    end

    it 'works for #build_from_hash' do
      obj = ArrayMapObject.build_from_hash(data)

      expect(obj.int_arr).to match_array([123, 456])

      expect(obj.pet_arr).to be_instance_of(Array)
      expect(obj.pet_arr.size).to eq(1)

      pet = obj.pet_arr.first
      expect(pet).to be_instance_of(Petstore::Pet)
      expect(pet.name).to eq('Kitty')

      expect(obj.int_map).to be_instance_of(Hash)
      expect(obj.int_map).to eq('int' => 123)

      expect(obj.pet_map).to be_instance_of(Hash)
      pet = obj.pet_map['pet']
      expect(pet).to be_instance_of(Petstore::Pet)
      expect(pet.name).to eq('Kitty')

      expect(obj.int_arr_map).to be_instance_of(Hash)
      arr = obj.int_arr_map['int_arr']
      expect(arr).to match_array([123, 456])

      expect(obj.pet_arr_map).to be_instance_of(Hash)
      arr = obj.pet_arr_map['pet_arr']
      expect(arr).to be_instance_of(Array)
      expect(arr.size).to eq(1)
      pet = arr.first
      expect(pet).to be_instance_of(Petstore::Pet)
      expect(pet.name).to eq('Kitty')

      expect(obj.boolean_true_arr).to be_instance_of(Array)
      obj.boolean_true_arr.each do |b|
        expect(b).to eq(true)
      end

      expect(obj.boolean_false_arr).to be_instance_of(Array)
      obj.boolean_false_arr.each do |b|
        expect(b).to eq(false)
      end
    end

    it 'works for #to_hash' do
      obj = ArrayMapObject.build_from_hash(data)
      expect_data = data.dup
      expect_data[:boolean_true_arr].map! { true }
      expect_data[:boolean_false_arr].map! { false }
      expect(obj.to_hash).to eq(expect_data)
    end
  end
end
