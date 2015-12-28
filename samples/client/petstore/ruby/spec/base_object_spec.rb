require 'spec_helper'

class ArrayMapObject < Petstore::Category
  attr_accessor :int_arr, :pet_arr, :int_map, :pet_map, :int_arr_map, :pet_arr_map

  def self.attribute_map
    {
      :int_arr => :int_arr,
      :pet_arr => :pet_arr,
      :int_map => :int_map,
      :pet_map => :pet_map,
      :int_arr_map => :int_arr_map,
      :pet_arr_map => :pet_arr_map
    }
  end

  def self.swagger_types
    {
      :int_arr => :'Array<Integer>',
      :pet_arr => :'Array<Pet>',
      :int_map => :'Hash<String, Integer>',
      :pet_map => :'Hash<String, Pet>',
      :int_arr_map => :'Hash<String, Array<Integer>>',
      :pet_arr_map => :'Hash<String, Array<Pet>>'
    }
  end
end


describe 'BaseObject' do
  describe 'array and map properties' do
    let(:obj) { ArrayMapObject.new }

    let(:data) do
      {int_arr: [123, 456],
       pet_arr: [{name: 'Kitty'}],
       int_map: {'int' => 123},
       pet_map: {'pet' => {name: 'Kitty'}},
       int_arr_map: {'int_arr' => [123, 456]},
       pet_arr_map: {'pet_arr' => [{name: 'Kitty'}]}
      }
    end

    it 'works for #build_from_hash' do
      obj.build_from_hash(data)

      obj.int_arr.should == [123, 456]

      obj.pet_arr.should be_a(Array)
      obj.pet_arr.size.should == 1
      pet = obj.pet_arr.first
      pet.should be_a(Petstore::Pet)
      pet.name.should == 'Kitty'

      obj.int_map.should be_a(Hash)
      obj.int_map.should == {'int' => 123}

      obj.pet_map.should be_a(Hash)
      pet = obj.pet_map['pet']
      pet.should be_a(Petstore::Pet)
      pet.name.should == 'Kitty'

      obj.int_arr_map.should be_a(Hash)
      arr = obj.int_arr_map['int_arr']
      arr.should == [123, 456]

      obj.pet_arr_map.should be_a(Hash)
      arr = obj.pet_arr_map['pet_arr']
      arr.should be_a(Array)
      arr.size.should == 1
      pet = arr.first
      pet.should be_a(Petstore::Pet)
      pet.name.should == 'Kitty'
    end

    it 'works for #to_hash' do
      obj.build_from_hash(data)
      obj.to_hash.should == data
    end
  end
end
