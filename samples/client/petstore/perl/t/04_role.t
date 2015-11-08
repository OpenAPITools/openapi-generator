use Test::More tests => 15;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;

use MooseX::amine;
use Class::Inspector;
use Data::Dumper;

use Log::Any::Adapter ('File', '/home/dave/bookeo_api.log');


SKIP: {
	eval "
		package MyApp;
		use Moose;
		with 'WWW::SwaggerClient::Role';
		
		sub auth_setup_handler {}
	";
	
	skip 'Moose not installed', 15 if $@;


my $api = MyApp->new;

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


my $add_pet = $api->add_pet(body => $pet);
my $get_pet = $api->get_pet_by_id(pet_id => $pet_id);

is $get_pet->id, '10008', 'stored and retrieved: got the proper pet id';
is $get_pet->name, 'perl test', 'stored and retrieved: got the proper pet name';
is $get_pet->category->id, '22', 'stored and retrieved: got the proper category id';
is $get_pet->category->name, 'perl', 'stored and retrieved: got the proper category name';
is $get_pet->tags->[0]->name, 'just kidding', 'stored and retrieved: got the proper tag name';
is $get_pet->tags->[0]->id, '11', 'stored and retrieved: got the proper tag id';

} # / SKIP
	
