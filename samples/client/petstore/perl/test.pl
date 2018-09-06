#!/usr/bin/perl
#
#
use lib 'lib';
use strict;
use warnings;
use WWW::OpenAPIClient::PetApi;
use WWW::OpenAPIClient::StoreApi;
use WWW::OpenAPIClient::ApiClient;
use WWW::OpenAPIClient::Configuration;
use WWW::OpenAPIClient::Object::Pet;
use WWW::OpenAPIClient::Object::Tag;
use WWW::OpenAPIClient::Object::Category;
use JSON;
use Data::Dumper;
use DateTime;

my $api = WWW::OpenAPIClient::PetApi->new(
    http_user_agent => 'Perl-Swagger-Test',
    api_key         => { api_key => 'ZZZZZZZZZZZZZZ' },
    api_key_prefix  => { api_key => 'Bearer' },

    username => 'username',
    password => 'password',
);

# exception handling
#eval {
#    print "\nget_pet_by_id:".Dumper $api->get_pet_by_id(pet_id => 9999);
#};
#if ($@) {
#    print "Exception when calling: $@\n";
#}

my $pet_id = 10008;

my $category =
  WWW::OpenAPIClient::Object::Category->new( 'id' => '2', 'name' => 'perl' );
my $tag =
  WWW::OpenAPIClient::Object::Tag->new( 'id' => '1', 'name' => 'just kidding' );
my $pet = WWW::OpenAPIClient::Object::Pet->new(
    'id'        => $pet_id,
    'name'      => 'perl test',
    "photoUrls" => [ '123', 'oop' ],
    'tags'      => [$tag],
    'status'    => 'pending',
    'category'  => $category
);

print "\npet(object)=" . Dumper $pet;
my $json = JSON->new->convert_blessed;

my $new_pet = WWW::OpenAPIClient::Object::Pet->new();
$new_pet = $new_pet->from_hash( $pet->to_hash );
print "new_pet(hash):" . Dumper( $new_pet->to_hash );

print "\nTest Petstore endpoints\n";
print "\nupload_file:"
  . Dumper $api->upload_file(
    pet_id              => $pet_id,
    additional_metadata => 'testabc',
    file                => './test.pl'
  );
print "\nadd_pet:" . Dumper $api->add_pet( body => $pet );
print "\nget_pet_by_id:" . Dumper $api->get_pet_by_id( pet_id => $pet_id );
print "\nupdate_pet_with_form:"
  . Dumper $api->update_pet_with_form(
    pet_id => $pet_id,
    name   => 'test_name',
    status => 'test status'
  );
print "\ndelete_pet:" . Dumper $api->delete_pet( pet_id => $pet_id );

my $store_api = WWW::OpenAPIClient::StoreApi->new();
print "\nget_inventory:" . Dumper $store_api->get_inventory();

my $pet_json = <<JSON;
{
    "pet": { 
        "id": 0,
        "category": {
            "id": 0,
            "name": "string"
        },
        "name": "doggie",
        "photoUrls": [
            "string"
        ],
        "tags": [
            {
                "id": 0,
                "name": "tag string"
            }
        ],
        "status": "available"
    }
}
JSON

print "\napi_client->deserialize:"
  . Dumper( $api->{api_client}->deserialize( "HASH[string,Pet]", $pet_json ) );
