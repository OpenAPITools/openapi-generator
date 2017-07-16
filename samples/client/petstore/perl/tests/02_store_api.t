use Test::More tests => 41;
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
use_ok('WWW::SwaggerClient::Object::User');


my $api_client = WWW::SwaggerClient::ApiClient->new();
my $store_api = WWW::SwaggerClient::StoreApi->new($api_client);

is $store_api->{api_client}{config}{base_url}, 'http://petstore.swagger.io:80/v2', 'get the default base URL from api client';

my $get_inventory_response = $store_api->get_inventory();

# comment out pending check as sometimes there's no object with pending status
#like ($get_inventory_response->{pending}, qr/^\d+$/, "pending is numeric");
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
is $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{photo_urls}->[0], "string", "get the photoUrl from the Pet object";

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
is $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{photo_urls}->[0], "string", "get the photoUrl from the Pet object";

my $pet_json_nopet = <<JSON;
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
JSON

is ref(decode_json $pet_json_nopet), "HASH", "the decoded json string is a hash";
is ref $api_client->deserialize("Pet", $pet_json_nopet), "WWW::SwaggerClient::Object::Pet", "get Pet object via from_hash()";
is $api_client->deserialize("Pet", $pet_json_nopet)->{name}, "doggie", "get the name of the Pet object";
is $api_client->deserialize("Pet", $pet_json_nopet)->{category}->{name}, "string", "get the category name of the Pet object";
is ref $api_client->deserialize("Pet", $pet_json_nopet)->{category}, "WWW::SwaggerClient::Object::Category", "get the Category the Pet object";
is ref $api_client->deserialize("Pet", $pet_json_nopet)->{tags}->[0], "WWW::SwaggerClient::Object::Tag", "get the Tag[0] the Pet object";
is $api_client->deserialize("Pet", $pet_json_nopet)->{tags}->[0]->{name}, "tag string", "get the tag name the Pet object";
is $api_client->deserialize("Pet", $pet_json_nopet)->{photo_urls}->[0], "string", "get the photoUrl from the Pet object";


my %userdata = (
	id => 4000, 
	username => "tony",
    firstName => "Tony",
    lastName => "Tiger",
    email => 'tony@fail.com',
    password => "XXXXXXXXXXX",
    phone => "408-867-5309",
    userStatus => 1,
    );
      
my $user = WWW::SwaggerClient::Object::User->new->from_hash(\%userdata);
is ref $user, 'WWW::SwaggerClient::Object::User', "built a User object via from_hash()";
is $user->{id}, $userdata{id}, "got the id of the User object";
is $user->{username}, $userdata{username}, "got the username of the User object";
is $user->{first_name}, $userdata{firstName}, "got the firstName of the User object";
is $user->{last_name}, $userdata{lastName}, "got the lastName of the User object";
is $user->{email}, $userdata{email}, "got the email of the User object";
is $user->{password}, $userdata{password}, "got the password of the User object";
is $user->{phone}, $userdata{phone}, "got the phone of the User object";
is $user->{user_status}, $userdata{userStatus}, "got the userStatus of the User object";


