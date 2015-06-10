use Test::More tests => 22;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;

use JSON;

use_ok('WWW::SwaggerClient::StoreApi');
use_ok('WWW::SwaggerClient::ApiClient');
use_ok('WWW::SwaggerClient::Object::Pet');
use_ok('WWW::SwaggerClient::Object::Tag');
use_ok('WWW::SwaggerClient::Object::Category');

my $api_client = WWW::SwaggerClient::ApiClient->new();
my $store_api = WWW::SwaggerClient::StoreApi->new('api_client' => $api_client);

is $store_api->{api_client}->{base_url}, 'http://petstore.swagger.io/v2', 'get the default base URL from api client';

my $get_inventory_response = $store_api->get_inventory();

like ($get_inventory_response->{pending}, qr/^\d+$/, "pending is numeric");
like ($get_inventory_response->{sold}, qr/^\d+$/, "sold is numeric");

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

is ref(decode_json $pet_json), "HASH", "the decoded json string is a hash";
is ref $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}, "WWW::SwaggerClient::Object::Pet", "get Pet object from hash";
is $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{name}, "doggie", "get the name of the Pet object";
is $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{category}->{name}, "string", "get the category name of the Pet object";
is ref $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{category}, "WWW::SwaggerClient::Object::Category", "get the Category the Pet object";
is ref $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{tags}[0], "WWW::SwaggerClient::Object::Tag", "get the Tag of the Pet object";
is $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{tags}[0]->{name}, "tag string", "get the Tag name of the Pet object";

my $array_json = <<JSON;
[
    { 
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
]
JSON

is ref(decode_json $array_json), "ARRAY", "the decoded json string is an array";
is ref $api_client->deserialize("ARRAY[Pet]", $array_json)->[0], "WWW::SwaggerClient::Object::Pet", "get Pet object from hash";
is $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{name}, "doggie", "get the name of the Pet object";
is $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{category}->{name}, "string", "get the category name of the Pet object";
is ref $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{category}, "WWW::SwaggerClient::Object::Category", "get the Category the Pet object";
is ref $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{tags}->[0], "WWW::SwaggerClient::Object::Tag", "get the Tag[0] the Pet object";
is $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{tags}->[0]->{name}, "tag string", "get the tag name the Pet object";


