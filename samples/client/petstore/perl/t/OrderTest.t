use Test::More tests => 2;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;


use_ok('WWW::SwaggerClient::Object::Order');

my $instance = WWW::SwaggerClient::Object::Order->new();

isa_ok($instance, 'WWW::SwaggerClient::Object::Order');

