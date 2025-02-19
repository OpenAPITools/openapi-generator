use Test::More tests => 57;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;

use JSON;

use_ok('WWW::OpenAPIClient::StoreApi');
use_ok('WWW::OpenAPIClient::ApiClient');
use_ok('WWW::OpenAPIClient::Object::Pet');
use_ok('WWW::OpenAPIClient::Object::Tag');
use_ok('WWW::OpenAPIClient::Object::Category');
use_ok('WWW::OpenAPIClient::Object::User');
use_ok('WWW::OpenAPIClient::Object::Order');


my $api_client = WWW::OpenAPIClient::ApiClient->new();
my $store_api = WWW::OpenAPIClient::StoreApi->new($api_client);

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
is ref $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}, "WWW::OpenAPIClient::Object::Pet", "get Pet object from hash";
is $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{name}, "doggie", "get the name of the Pet object";
is $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{category}->{name}, "string", "get the category name of the Pet object";
is ref $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{category}, "WWW::OpenAPIClient::Object::Category", "get the Category the Pet object";
is ref $api_client->deserialize("HASH[string,Pet]", $pet_json)->{pet}->{tags}[0], "WWW::OpenAPIClient::Object::Tag", "get the Tag of the Pet object";
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
is ref $api_client->deserialize("ARRAY[Pet]", $array_json)->[0], "WWW::OpenAPIClient::Object::Pet", "get Pet object from hash";
is $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{name}, "doggie", "get the name of the Pet object";
is $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{category}->{name}, "string", "get the category name of the Pet object";
is ref $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{category}, "WWW::OpenAPIClient::Object::Category", "get the Category the Pet object";
is ref $api_client->deserialize("ARRAY[Pet]", $array_json)->[0]->{tags}->[0], "WWW::OpenAPIClient::Object::Tag", "get the Tag[0] the Pet object";
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
is ref $api_client->deserialize("Pet", $pet_json_nopet), "WWW::OpenAPIClient::Object::Pet", "get Pet object via from_hash()";
is $api_client->deserialize("Pet", $pet_json_nopet)->{name}, "doggie", "get the name of the Pet object";
is $api_client->deserialize("Pet", $pet_json_nopet)->{category}->{name}, "string", "get the category name of the Pet object";
is ref $api_client->deserialize("Pet", $pet_json_nopet)->{category}, "WWW::OpenAPIClient::Object::Category", "get the Category the Pet object";
is ref $api_client->deserialize("Pet", $pet_json_nopet)->{tags}->[0], "WWW::OpenAPIClient::Object::Tag", "get the Tag[0] the Pet object";
is $api_client->deserialize("Pet", $pet_json_nopet)->{tags}->[0]->{name}, "tag string", "get the tag name the Pet object";
is $api_client->deserialize("Pet", $pet_json_nopet)->{photo_urls}->[0], "string", "get the photoUrl from the Pet object";

my %userdata = (
	id => "4000",
	username => "tony",
    firstName => "Tony",
    lastName => "Tiger",
    email => 'tony@fail.com',
    password => "XXXXXXXXXXX",
    phone => "408-867-5309",
    userStatus => 1,
    );

my $user = WWW::OpenAPIClient::Object::User->new->from_hash(\%userdata);
is ref $user, 'WWW::OpenAPIClient::Object::User', "built a User object via from_hash()";
is $user->{id}, $userdata{id}, "got the id of the User object";
is $user->{username}, $userdata{username}, "got the username of the User object";
is $user->{first_name}, $userdata{firstName}, "got the firstName of the User object";
is $user->{last_name}, $userdata{lastName}, "got the lastName of the User object";
is $user->{email}, $userdata{email}, "got the email of the User object";
is $user->{password}, $userdata{password}, "got the password of the User object";
is $user->{phone}, $userdata{phone}, "got the phone of the User object";
is $user->{user_status}, $userdata{userStatus}, "got the userStatus of the User object";

my $user_to_json = JSON->new->convert_blessed->encode($user);
like $user_to_json, qr/4000/, '$userdata{id} is string. But, json id is a number';
unlike $user_to_json, qr/"4000"/, '$userdata{id} is string. But, json id is a number';


my %order_data = (
    id       => '123',
    petId    => '456',
    quantity => 789,
    shipDate => '2020-11-06T09:20:48Z',
    status   => 101112, # status type is string, but this data is number 
    complete => 0, # status type is boolean, but this data is number
);
my $order = WWW::OpenAPIClient::Object::Order->new->from_hash(\%order_data);
is ref($order->ship_date), 'DateTime';

my $order_to_json = JSON->new->convert_blessed->encode($order);
like $order_to_json, qr/123/, '$order{id} is string. But, json type is number';
unlike $order_to_json, qr/"123"/, '$order{id} is string. But, json type is number';

like $order_to_json, qr/789/, '$order{quantity} is number';
unlike $order_to_json, qr/"789"/, '$order{quantity} is number';

like $order_to_json, qr/2020-11-06T09:20:48Z/, '$order{shipDate} to date-time format';

like $order_to_json, qr/"101112"/, '$order{status} is number. But json type is string';

like $order_to_json, qr/false/, '$order{complete} is number. But json type is boolean';

my $pet_object = WWW::OpenAPIClient::Object::Pet->new->from_hash({
    tags => [ 
        WWW::OpenAPIClient::Object::Tag->new->from_hash({id => 123, name => 1000}), 
        WWW::OpenAPIClient::Object::Tag->new->from_hash({id => 456, name => 'test2'}),
    ]
});


my $pet_object_to_json = JSON->new->convert_blessed->encode($pet_object);
like $pet_object_to_json, qr/\"id\":123/, '$pet_object->tags->[0]->id';
like $pet_object_to_json, qr/\"name\":\"1000\"/, '$pet_object->tags->[0]->name';
like $pet_object_to_json, qr/\"id\":456/, '$pet_object->tags->[1]->id';
like $pet_object_to_json, qr/\"name\":\"test2\"/, '$pet_object->tags->[1]->name';

my %undef_test_order_data = (
    id     => undef,
    status => undef,
);
my $undef_test_order = WWW::OpenAPIClient::Object::Order->new->from_hash(\%undef_test_order_data);
lives_ok { JSON->new->convert_blessed->encode($undef_test_order) };
