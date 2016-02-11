use Test::More tests => 2;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;


use_ok('WWW::SwaggerClient::Object::Pet');

my $instance = WWW::SwaggerClient::Object::Pet->new();

isa_ok($instance, 'WWW::SwaggerClient::Object::Pet');

