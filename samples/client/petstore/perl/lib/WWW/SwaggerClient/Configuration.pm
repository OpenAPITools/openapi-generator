package WWW::SwaggerClient::Configuration;

use strict;
use warnings;
use utf8;

use Log::Any qw($log);
use Carp;

use constant VERSION => '1.0.0';

# class/static variables
our $api_client;
our $http_timeout = 180;
our $http_user_agent = 'Perl-Swagger';

# authenticaiton setting
our $api_key = {};
our $api_key_prefix = {};
our $username;
our $password;


1;
