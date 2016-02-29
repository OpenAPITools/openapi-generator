use Test::More tests => 2;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;


use_ok('WWW::SwaggerClient::Object::Tag');

my $instance = WWW::SwaggerClient::Object::Tag->new();

isa_ok($instance, 'WWW::SwaggerClient::Object::Tag');

