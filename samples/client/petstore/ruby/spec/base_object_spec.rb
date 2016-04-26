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

  def self.swagger_types
    {
      :int_arr => :'Array<Integer>',
      :pet_arr => :'Array<Pet>',
      :int_map => :'Hash<String, Integer>',
      :pet_map => :'Hash<String, Pet>',
      :int_arr_map => :'Hash<String, Array<Integer>>',
      :pet_arr_map => :'Hash<String, Array<Pet>>',
      :boolean_true_arr  => :'Array<BOOLEAN>',
      :boolean_false_arr => :'Array<BOOLEAN>',
    }
  end
end

describe 'BaseObject' do
  describe 'boolean values' do
    let(:obj) { Petstore::Cat.new({declawed: false}) }

    it 'should have values set' do
      obj.declawed.should_not eq nil
      obj.declawed.should eq false
    end
  end

  describe 'array and map properties' do
    let(:obj) { ArrayMapObject.new }

    let(:data) do
      {int_arr: [123, 456],
       pet_arr: [{name: 'Kitty'}],
       int_map: {'int' => 123},
       pet_map: {'pet' => {name: 'Kitty'}},
       int_arr_map: {'int_arr' => [123, 456]},
       pet_arr_map: {'pet_arr' => [{name: 'Kitty'}]},
       boolean_true_arr:  [true, "true", "TruE", 1, "y", "yes", "1", "t", "T"],
       boolean_false_arr: [false, "", 0, "0", "f", nil, "null"],
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

      obj.boolean_true_arr.should be_a(Array)
      obj.boolean_true_arr.each do |b|
        b.should eq true
      end

      obj.boolean_false_arr.should be_a(Array)
      obj.boolean_false_arr.each do |b|
        b.should eq false
      end
    end

    it 'works for #to_hash' do
      obj.build_from_hash(data)
      expect_data = data.dup
      expect_data[:boolean_true_arr].map! {true}
      expect_data[:boolean_false_arr].map! {false}
      obj.to_hash.should == expect_data
    end
  end
end
