# To run this profiling:
#   gem install ruby-prof
#   ruby -Ilib petstore_profiling.rb

require 'petstore'
require 'ruby-prof'

class PetstoreProfiling
  attr_accessor :total, :new_pet_id, :output_file

  def initialize
    @total = 5
    @new_pet_id = 50002
    @output_file = './petstore_profiling.output'
  end

  def call_apis
    pet_api = Petstore::PetApi.new

    ### ADD PET ###
    pet = Petstore::Pet.new
    pet.id = new_pet_id
    pet.name = "profiler"
    pet.status = "available"
    pet.photo_urls = ["http://profiler.com"]
    # new tag
    tag= Petstore::Tag.new
    tag.id = new_pet_id # use the same id as pet
    tag.name = "profile tag 1"
    # new category
    category = Petstore::Category.new
    category.id = new_pet_id # use the same id as pet
    category.name = "profile category 1"

    pet.tags = [tag]
    pet.category = category

    # add a new pet (model)
    pet_api.add_pet(body: pet)

    ### GET PET ###
    pet = pet_api.get_pet_by_id(new_pet_id)

    ### UPDATE PET WITH FORM ###
    pet_api.update_pet_with_form(new_pet_id, name: 'new profiler', status: 'sold')

    ### DELETE PET ###
    pet_api.delete_pet(new_pet_id)
  rescue Petstore::ApiError => e
    puts "Caught error: #{e.message}"
    puts "HTTP response headers: #{e.response_headers}"
    puts "HTTP response body: #{e.response_body}"
    puts "HTTP status code: #{e.code}"
  end

  def run
    puts "Running profiling... (total: #{@total})"

    RubyProf.start
    @total.times { call_apis }
    result = RubyProf.stop

    printer = RubyProf::FlatPrinter.new(result)
    File.open(@output_file, 'w') do |file|
      printer.print(file)
    end

    puts "Profiling results written to #{@output_file}"
  end
end

if __FILE__ == $0
  profiling = PetstoreProfiling.new
  profiling.run
end
