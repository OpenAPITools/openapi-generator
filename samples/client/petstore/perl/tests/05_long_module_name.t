use Test::More;
use Test::Exception;

use lib 'deep_module_test/lib';
use strict;
use warnings;

if (! -d 'deep_module_test/lib' ) {
    plan skip_all => 'bin/perl-petstore.sh needs to be run first';
}
else {
    plan tests => 38;
}

use_ok('Something::Deep::PetApi');
use_ok('Something::Deep::ApiClient');
use_ok('Something::Deep::Object::Pet');
use_ok('Something::Deep::Object::Tag');
use_ok('Something::Deep::Object::Category');

my $api_client = Something::Deep::ApiClient->instance('base_url' => 'http://testing');
my $pet_api = Something::Deep::PetApi->new('api_client' => $api_client);
is $pet_api->{api_client}->{base_url}, 'http://testing', 'get the proper base URL from api client';

my $api = Something::Deep::PetApi->new();

is $api->{api_client}->{base_url}, 'http://testing', 'we still get the original base URL from api client, because it\'s a singleton';

# reset the base_url - no direct access because an application shouldn't be changing 
# its base URL halfway through
$api->{api_client}->{base_url} = 'http://petstore.swagger.io/v2';

is $api->{api_client}->{base_url}, 'http://petstore.swagger.io/v2', 'get the default base URL from api client';

# test select_header_content_type
is $api->{api_client}->select_header_content_type('application/xml', 'Application/JSON'), 'application/json', 'get the proper content type application/json but not application/xml';
is $api->{api_client}->select_header_content_type('application/xml'), 'application/xml', 'get the proper content type application/json'; 
is $api->{api_client}->select_header_content_type(''), 'application/json', 'get the proper content type application/json (default)'; 

# test select_header_accept
is $api->{api_client}->select_header_accept('application/xml', 'Application/JSON'), 'application/json', 'get the proper accept application/json but not application/xml';
is $api->{api_client}->select_header_content_type('application/xml'), 'application/xml', 'get the proper accept application/json'; 
is $api->{api_client}->select_header_accept(''), undef, 'get the proper accept "undef" (default)';

my $pet_id = 10008;

my $category =  Something::Deep::Object::Category->new('id' => '22', 'name' => 'perl');
my $tag =  Something::Deep::Object::Tag->new('id' => '11', 'name' => 'just kidding');
my $pet =  Something::Deep::Object::Pet->new('id' => $pet_id, 'name' => 'perl test',
      "photoUrls" => ['123', 'oop'], 'tags' => [$tag], 'status' => 'pending', 'category' => $category);

isa_ok($api, 'Something::Deep::PetApi');
isa_ok($category, 'Something::Deep::Object::Category');
isa_ok($tag, 'Something::Deep::Object::Tag');
isa_ok($pet, 'Something::Deep::Object::Pet');

my $pet_hash = $pet->to_hash;

is $pet_hash->{category}->{id}, '22', 'get the proper category id';
is $pet_hash->{category}->{name}, 'perl', 'get the proper category name';
is $pet_hash->{tags}[0]->{name}, 'just kidding', 'get the proper tag name';
is $pet_hash->{tags}[0]->{id}, '11', 'get the proper tag id';

my $add_pet = $api->add_pet(pet => $pet);

my $get_pet = $api->get_pet_by_id(pet_id => $pet_id);
my $get_pet_hash = $get_pet->to_hash;
is $get_pet_hash->{name}, 'perl test', 'get the proper pet name from get_pet_by_id';
is $get_pet_hash->{id}, '10008', 'get the proper pet id from get_pet_by_id';
is $get_pet_hash->{category}->{name}, 'perl', 'get the proper category name from get_pet_by_id';
is $get_pet_hash->{category}->{id}, '22', 'get the proper category id from get_pet_by_id';
is $get_pet_hash->{category}->{name}, 'perl', 'get the proper category from get_pet_by_id';
is $get_pet_hash->{tags}[0]->{name}, 'just kidding', 'get the proper tag from get_pet_by_id';
is $get_pet_hash->{tags}[0]->{id}, '11', 'get the proper tag id from get_pet_by_id';
is $get_pet_hash->{photoUrls}->[0], '123', 'get the proper photoUrl from get_pet_by_id';
is $get_pet_hash->{photoUrls}->[1], 'oop', 'get the proper photoUrl from get_pet_by_id';


my $update_pet_with_form = $api->update_pet_with_form(pet_id => $pet_id, name => 'test_name', status => 'sold');
is $update_pet_with_form, undef, 'get the null response from update_pet_wth_form';

my $get_pet_after_update = $api->get_pet_by_id(pet_id => $pet_id);
is $get_pet_after_update->{status}, 'sold', 'get the updated status after update_pet_with_form';

my $upload_pet = $api->upload_file(pet_id => $pet_id, additional_metadata => 'testabc', file => 'test.pl');
is $upload_pet, undef, 'get the null response from upload_pet';

my $delete_pet = $api->delete_pet(pet_id => $pet_id);
is $delete_pet, undef, 'get the null response from delete_pet';
throws_ok{$api->get_pet_by_id(pet_id => $pet_id)} qr/API Exception\(404\): Not Found/, "throw 404 error about pet not found after delete";
#is $get_pet_after_delete->{status}, undef, 'get the updated status after update_pet_with_form';

my $pets;
lives_ok {$pets = $api->find_pets_by_status(status => [qw(sold available)])} 'array query param processed correctly';
isa_ok($pets->[0], 'Something::Deep::Object::Pet');

