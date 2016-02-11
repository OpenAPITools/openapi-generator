use Test::More tests => 2;
use Test::Exception;

use lib 'lib';
use strict;
use warnings;


use_ok('WWW::SwaggerClient::Object::Category');

my $instance = WWW::SwaggerClient::Object::Category->new();

isa_ok($instance, 'WWW::SwaggerClient::Object::Category');

