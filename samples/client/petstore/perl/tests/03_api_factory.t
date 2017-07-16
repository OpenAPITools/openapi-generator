use Test::More tests => 19;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;

use_ok('WWW::SwaggerClient::ApiFactory');

my $api_factory = WWW::SwaggerClient::ApiFactory->new('base_url' => 'http://testing');
my $pet_api = $api_factory->get_api('Pet');
isa_ok($pet_api, 'WWW::SwaggerClient::PetApi');
is $pet_api->{api_client}{config}{base_url}, 'http://testing', 'get the proper base URL from api client';

$api_factory = WWW::SwaggerClient::ApiFactory->new;
$pet_api = $api_factory->get_api('Pet');

# reset the base_url - no direct access because an application shouldn't be changing 
# its base URL halfway through
$pet_api->{api_client}{config}{base_url} = 'http://petstore.swagger.io/v2';
is $pet_api->{api_client}{config}{base_url}, 'http://petstore.swagger.io/v2', 'get the default base URL from api client';

# test accessor methods
my $pet_id = 10008;
# note - we don't need to 'use' these modules because they've already been loaded by ApiFactory
my ($category, $tag, $pet);
lives_ok { $category = WWW::SwaggerClient::Object::Category->new('id' => '22', 'name' => 'perl') } 'Category.pm loaded OK';
lives_ok { $tag =  WWW::SwaggerClient::Object::Tag->new('id' => '11', 'name' => 'just kidding') } 'Tag.pm loaded OK';
lives_ok { $pet =  WWW::SwaggerClient::Object::Pet->new('id' => $pet_id, 'name' => 'perl test',
      "photoUrls" => ['123', 'oop'], 'tags' => [$tag], 'status' => 'pending', 'category' => $category) } 'Pet.pm loaded OK';

is $pet->id, '10008', 'got the proper pet id';
is $pet->name, 'perl test', 'got the proper pet name';
is $pet->category->id, '22', 'got the proper category id';
is $pet->category->name, 'perl', 'got the proper category name';
is $pet->tags->[0]->name, 'just kidding', 'got the proper tag name';
is $pet->tags->[0]->id, '11', 'got the proper tag id';


my $add_pet = $pet_api->add_pet(body => $pet);
my $get_pet = $pet_api->get_pet_by_id(pet_id => $pet_id);

is $get_pet->id, '10008', 'stored and retrieved: got the proper pet id';
is $get_pet->name, 'perl test', 'stored and retrieved: got the proper pet name';
is $get_pet->category->id, '22', 'stored and retrieved: got the proper category id';
is $get_pet->category->name, 'perl', 'stored and retrieved: got the proper category name';
is $get_pet->tags->[0]->name, 'just kidding', 'stored and retrieved: got the proper tag name';
is $get_pet->tags->[0]->id, '11', 'stored and retrieved: got the proper tag id';

