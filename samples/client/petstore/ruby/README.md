## Installation

### Build a gem

You can build the generated client into a gem:

```shell
gem build swagger_client.gemspec
```

Then you can either install the gem:

```shell
gem install ./swagger_client-1.0.0.gem
```

or publish the gem to a gem server like [RubyGems](https://rubygems.org/).

Finally add this to your Gemfile:

    gem 'swagger_client', '~> 1.0.0'

### Host as a git repository

You can also choose to host the generated client as a git repository, e.g. on github:
https://github.com/xhh/swagger-petstore-ruby

Then you can reference it in Gemfile:

    gem 'swagger_client', :git => 'https://github.com/xhh/swagger-petstore-ruby.git'

### Use without installation

You can also use the client directly like this:

```shell
ruby -Ilib script.rb
```

## Configuration

```ruby
require 'swagger_client'

SwaggerClient::Swagger.configure do |config|
  config.api_key = 'special-key'
  config.host = 'petstore.swagger.io'
  config.base_path = '/v2'
end
```

## Getting Started

```ruby
pet = SwaggerClient::PetApi.get_pet_by_id(5)
puts pet.to_body
```
