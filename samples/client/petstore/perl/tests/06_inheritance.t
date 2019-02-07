use Test::More tests => 37;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;

use_ok('WWW::OpenAPIClient::Object::Adult');
use_ok('WWW::OpenAPIClient::Object::Person');
use_ok('WWW::OpenAPIClient::Object::Human');

my $pet_id = 10008;

my $adult =  WWW::OpenAPIClient::Object::Adult->new('duplicated_optional' => 22,
                                                       'duplicated_required' => 11,
                                                       'children' => [],
                                                       'adult_required' => 1);
my $child =  WWW::OpenAPIClient::Object::Child->new('age' => 11, 'first_name' => 'William');
my $human =  WWW::OpenAPIClient::Object::Human->new('___type' => $pet_id, 'body' => 'perl test');

isa_ok($adult, 'WWW::OpenAPIClient::Object::Adult');
isa_ok($child, 'WWW::OpenAPIClient::Object::Child');
isa_ok($human, 'WWW::OpenAPIClient::Object::Human');

#my $pet_hash = $pet->to_hash;
#
#is $pet_hash->{category}->{id}, '22', 'get the proper category id';
#is $pet_hash->{category}->{name}, 'perl', 'get the proper category name';
#is $pet_hash->{tags}[0]->{name}, 'just kidding', 'get the proper tag name';
#is $pet_hash->{tags}[0]->{id}, '11', 'get the proper tag id';
#
#my $add_pet = $api->add_pet(pet => $pet);
#
#my $get_pet = $api->get_pet_by_id(pet_id => $pet_id);
#my $get_pet_hash = $get_pet->to_hash;
#is $get_pet_hash->{name}, 'perl test', 'get the proper pet name from get_pet_by_id';
#is $get_pet_hash->{id}, '10008', 'get the proper pet id from get_pet_by_id';
#is $get_pet_hash->{category}->{name}, 'perl', 'get the proper category name from get_pet_by_id';
#is $get_pet_hash->{category}->{id}, '22', 'get the proper category id from get_pet_by_id';
#is $get_pet_hash->{category}->{name}, 'perl', 'get the proper category from get_pet_by_id';
#is $get_pet_hash->{tags}[0]->{name}, 'just kidding', 'get the proper tag from get_pet_by_id';
#is $get_pet_hash->{tags}[0]->{id}, '11', 'get the proper tag id from get_pet_by_id';
#is $get_pet_hash->{photoUrls}->[0], '123', 'get the proper photoUrl from get_pet_by_id';
#is $get_pet_hash->{photoUrls}->[1], 'oop', 'get the proper photoUrl from get_pet_by_id';
#
#
#my $update_pet_with_form = $api->update_pet_with_form(pet_id => $pet_id, name => 'test_name', status => 'sold');
#is $update_pet_with_form, undef, 'get the null response from update_pet_wth_form';
#
#my $get_pet_after_update = $api->get_pet_by_id(pet_id => $pet_id);
#is $get_pet_after_update->{status}, 'sold', 'get the updated status after update_pet_with_form';
#
#my $upload_pet = $api->upload_file(pet_id => $pet_id, additional_metadata => 'testabc', file => 'test.pl');
#isa_ok($upload_pet, 'WWW::OpenAPIClient::Object::ApiResponse');
#
#my $delete_pet = $api->delete_pet(pet_id => $pet_id);
#is $delete_pet, undef, 'get the null response from delete_pet';
#throws_ok{$api->get_pet_by_id(pet_id => $pet_id)} qr/API Exception\(404\): Not Found/, "throw 404 error about pet not found after delete";
##is $get_pet_after_delete->{status}, undef, 'get the updated status after update_pet_with_form';
#
#my $pets;
#lives_ok {$pets = $api->find_pets_by_status(status => [qw(sold available)])} 'array query param processed correctly';
#isa_ok($pets->[0], 'WWW::OpenAPIClient::Object::Pet');
#
