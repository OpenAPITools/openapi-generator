use Test::More tests => 13;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;

use_ok('WWW::SwaggerClient::ApiFactory');

my $api_factory = WWW::SwaggerClient::ApiFactory->new('base_url' => 'http://testing');
my $pet_api = $api_factory->get_api('Pet');
isa_ok($pet_api, 'WWW::SwaggerClient::PetApi');
is $pet_api->{api_client}->{base_url}, 'http://testing', 'get the proper base URL from api client';

$api_factory = WWW::SwaggerClient::ApiFactory->new;
$pet_api = $api_factory->get_api('Pet');

is $pet_api->{api_client}->{base_url}, 'http://petstore.swagger.io/v2', 'get the default base URL from api client';

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
