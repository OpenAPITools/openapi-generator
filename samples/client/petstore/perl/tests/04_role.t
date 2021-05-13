use Test::More tests => 37;
use Test::Exception;
use Test::Warnings 'warnings';
use Test::Deep;

use lib 'lib';
use strict;
use warnings;

SKIP: {
	eval "
		package MyApp;
		use Moose;
		with 'WWW::OpenAPIClient::Role';
		sub auth_setup_handler {}
	";
	
#	die $@ if $@;
	skip 'Moose not installed', 37 if $@;

my $api = MyApp->new;

my $pet_id = 10008;
# note - we don't need to 'use' these modules because they've already been loaded by ApiFactory
my ($category, $tag, $pet);
lives_ok { $category = WWW::OpenAPIClient::Object::Category->new('id' => '22', 'name' => 'perl') } 'Category.pm loaded OK';
lives_ok { $tag =  WWW::OpenAPIClient::Object::Tag->new('id' => '11', 'name' => 'just kidding') } 'Tag.pm loaded OK';
lives_ok { $pet =  WWW::OpenAPIClient::Object::Pet->new('id' => $pet_id, 'name' => 'perl test',
      "photoUrls" => ['123', 'oop'], 'tags' => [$tag], 'status' => 'pending', 'category' => $category) } 'Pet.pm loaded OK';

is $pet->id, '10008', 'got the proper pet id';
is $pet->name, 'perl test', 'got the proper pet name';
is $pet->category->id, '22', 'got the proper category id';
is $pet->category->name, 'perl', 'got the proper category name';
is $pet->tags->[0]->name, 'just kidding', 'got the proper tag name';
is $pet->tags->[0]->id, '11', 'got the proper tag id';


my $add_pet = $api->add_pet(pet => $pet);
my $get_pet = $api->get_pet_by_id(pet_id => $pet_id);

is $get_pet->id, '10008', 'stored and retrieved: got the proper pet id';
is $get_pet->name, 'perl test', 'stored and retrieved: got the proper pet name';
is $get_pet->category->id, '22', 'stored and retrieved: got the proper category id';
is $get_pet->category->name, 'perl', 'stored and retrieved: got the proper category name';
is $get_pet->tags->[0]->name, 'just kidding', 'stored and retrieved: got the proper tag name';
is $get_pet->tags->[0]->id, '11', 'stored and retrieved: got the proper tag id';

# documentation tests

# API method docs 
is_deeply(	[sort keys %{$api->pet_api->method_documentation}], 
			[ 'add_pet', 'delete_pet', 'find_pets_by_status', 'find_pets_by_tags', 'get_pet_by_id', 'update_pet', 'update_pet_with_form', 'upload_file', 'upload_file_with_required_file'], 
			"Pet API method_documentation has the correct keys");
is $api->pet_api->method_documentation->{get_pet_by_id}->{params}->{pet_id}->{description}, 
	'ID of pet to return', 'get_pet_by_id parameter pet_id description is correct';
is $api->pet_api->method_documentation->{get_pet_by_id}->{params}->{pet_id}->{required},
	1, 'get_pet_by_id parameter pet_id is required';
is $api->pet_api->method_documentation->{get_pet_by_id}->{params}->{pet_id}->{data_type},
	'int', 'get_pet_by_id parameter pet_id is an int';
is $api->pet_api->method_documentation->{get_pet_by_id}->{returns},
	'Pet', 'get_pet_by_id returns a Pet';
is $api->pet_api->method_documentation->{get_pet_by_id}->{summary},
	'Find pet by ID', 'get_pet_by_id summary is correct';

# object class docs 
my $pet_class_doco = { 'description' => '', required => [], class => 'Pet' };
is_deeply($get_pet->class_documentation, $pet_class_doco, 'Pet object class_documentation is available');
is $get_pet->class_documentation->{description}, '', 'Pet object class_documentation is correct'; # right now it's blank
is $get_pet->class_documentation->{class}, 'Pet', 'Pet object class_documentation returns correct class name';

# object method docs 
is $get_pet->method_documentation->{status}->{description}, 'pet status in the store', 'Pet object method_documentation for status() - description is correct';
is $get_pet->method_documentation->{status}->{format}, '', 'Pet object method_documentation for status() - format is correct';
is $get_pet->method_documentation->{status}->{base_name}, 'status', 'Pet object method_documentation for status() - base_name is correct';
is $get_pet->method_documentation->{status}->{datatype}, 'string', 'Pet object method_documentation for status() - datatype is correct';



# / documentation tests

my $tokens = {
	username => 'UserName',
	password => 'PassWord',
	access_token => 'OAuth_token',
	
	someKey => { token => 'some_key_token',
				   prefix => 'some_key_prefix', 
				   in => 'query',    
				   },
				   
	anotherKey => { token => 'another_key_token',
				  },                   
	};

$api->_cfg->accept_tokens({%$tokens}); # pass a copy
no warnings 'once';
is $api->_cfg->{username}, 'UserName', 'accept_tokens() correctly set the username';
is $api->_cfg->{password}, 'PassWord', 'accept_tokens() correctly set the password';
is $api->_cfg->{access_token}, 'OAuth_token', 'accept_tokens() correctly set the oauth';

my $api_key_href = {
          'anotherKey' => 'another_key_token',
          'someKey' => 'some_key_token'
        };
cmp_deeply( $api->_cfg->{api_key}, $api_key_href, 'accept_tokens() correctly set api_key' );

my $api_key_prefix_href = {
          'someKey' => 'some_key_prefix'
        };
cmp_deeply( $api->_cfg->{api_key_prefix}, $api_key_prefix_href, 'accept_tokens() correctly set api_key_prefix' );

my $api_key_in = {
          'someKey' => 'query',
          'anotherKey' => 'head'
        };
cmp_deeply( $api->_cfg->{api_key_in}, $api_key_in, 'accept_tokens() correctly set api_key_in' );

use warnings 'once';

my $cleared_tokens_cmp = {
          'anotherKey' => {
                            'in' => 'head',
                            'token' => 'another_key_token',
                            'prefix' => undef
                          },
          'access_token' => 'OAuth_token',
          'someKey' => {
                         'token' => 'some_key_token',
                         'in' => 'query',
                         'prefix' => 'some_key_prefix'
                       },
          'username' => 'UserName',
          'password' => 'PassWord'
        };
cmp_deeply( $api->_cfg->clear_tokens, $cleared_tokens_cmp, 'clear_tokens() returns the correct data structure' );

my $bad_token = { bad_token_name => 'bad token value' }; # value should should be hashref
dies_ok { $api->_cfg->accept_tokens($bad_token) } "bad token causes exception";


} # / SKIP
	
