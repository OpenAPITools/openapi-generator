## Installation

### Build a gem

You can build the generated client into a gem:

```shell
gem build petstore.gemspec
```

Then you can either install the gem:

```shell
gem install ./petstore-1.0.0.gem
```

or publish the gem to a gem server like [RubyGems](https://rubygems.org/).

Finally add this to your Gemfile:

    gem 'petstore', '~> 1.0.0'

### Host as a git repository

You can also choose to host the generated client as a git repository, e.g. on github:
https://github.com/xhh/swagger-petstore-ruby

Then you can reference it in Gemfile:

    gem 'petstore', :git => 'https://github.com/xhh/swagger-petstore-ruby.git'

### Use without installation

You can also use the client directly like this:

```shell
ruby -Ilib script.rb
```

## Getting Started

```ruby
require 'petstore'

Petstore.configure do |config|
  config.api_key['api_key'] = 'special-key'
  config.host = 'petstore.swagger.io'
  config.base_path = '/v2'
  # enable debugging (default is disabled)
  config.debugging = true
end

pet_api = Petstore::PetApi.new
pet = pet_api.get_pet_by_id(5)
puts pet.to_body
```
