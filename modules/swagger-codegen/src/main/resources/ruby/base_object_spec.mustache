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
      expect(obj.declawed).not_to be_nil
      expect(obj.declawed).to eq(false)
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

      expect(obj.int_arr).to match_array([123, 456])

      expect(obj.pet_arr).to be_instance_of(Array)
      expect(obj.pet_arr).to be_instance_of(1)

      pet = obj.pet_arr.first
      expect(pet).to be_instance_of(Petstore::Pet)
      expect(pet.name).to eq('Kitty')

      expect(obj.int_map).to be_instance_of(Hash)
      expect(obj.int_map).to eq({'int' => 123})

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
      obj.build_from_hash(data)
      expect_data = data.dup
      expect_data[:boolean_true_arr].map! {true}
      expect_data[:boolean_false_arr].map! {false}
      expect(obj.to_hash).to eq(expect_data)
    end
  end
end
