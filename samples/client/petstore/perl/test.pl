#!/usr/bin/perl
#
#
use lib 'lib';
use strict;
use warnings;
use WWW::SwaggerClient::PetApi;
use WWW::SwaggerClient::StoreApi;
use WWW::SwaggerClient::ApiClient;
use WWW::SwaggerClient::Configuration;
use WWW::SwaggerClient::Object::Pet;
use WWW::SwaggerClient::Object::Tag;
use WWW::SwaggerClient::Object::Category;
use JSON;
use Data::Dumper;
use DateTime;

$WWW::SwaggerClient::Configuration::http_user_agent = 'Perl-Swagger-Test';
$WWW::SwaggerClient::Configuration::api_key->{'api_key'} = 'ZZZZZZZZZZZZZZ';
$WWW::SwaggerClient::Configuration::api_key_prefix->{'api_key'} = 'Bearer';

$WWW::SwaggerClient::Configuration::username = 'username';
$WWW::SwaggerClient::Configuration::password = 'password';


my $api = WWW::SwaggerClient::PetApi->new();

# exception handling
#eval {
#    print "\nget_pet_by_id:".Dumper $api->get_pet_by_id(pet_id => 9999);
#};
#if ($@) {
#    print "Exception when calling: $@\n";
#}

my $pet_id = 10008;

my $category =  WWW::SwaggerClient::Object::Category->new('id' => '2', 'name' => 'perl');
my $tag =  WWW::SwaggerClient::Object::Tag->new('id' => '1', 'name' => 'just kidding'); 
my $pet =  WWW::SwaggerClient::Object::Pet->new('id' => $pet_id, 'name' => 'perl test', 
    "photoUrls" => ['123', 'oop'], 'tags' => [$tag], 'status' => 'pending', 'category' => $category);

print "\npet(object)=".Dumper $pet;
my $json = JSON->new->convert_blessed;

my $new_pet = WWW::SwaggerClient::Object::Pet->new();
$new_pet = $new_pet->from_hash($pet->to_hash);
print "new_pet(hash):".Dumper($new_pet->to_hash);

print "\nTest Petstore endpoints\n";
print "\nupload_file:".Dumper $api->upload_file(pet_id => $pet_id, additional_metadata => 'testabc', file => './test.pl');
print "\nadd_pet:".Dumper $api->add_pet(body => $pet);
print "\nget_pet_by_id:".Dumper $api->get_pet_by_id(pet_id => $pet_id);
print "\nupdate_pet_with_form:".Dumper $api->update_pet_with_form(pet_id => $pet_id, name => 'test_name', status => 'test status');
print "\ndelete_pet:".Dumper $api->delete_pet(pet_id => $pet_id);

my $store_api = WWW::SwaggerClient::StoreApi->new();
print "\nget_inventory:".Dumper $store_api->get_inventory();

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

print "\napi_client->deserialize:".Dumper($api->{api_client}->deserialize("HASH[string,Pet]", $pet_json));
