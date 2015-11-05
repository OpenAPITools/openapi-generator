package WWW::SwaggerClient::ApiFactory;

use strict;
use warnings;
use utf8;

use Carp;
use Module::Find;

usesub WWW::SwaggerClient::Object;

use WWW::SwaggerClient::ApiClient;

=head1 Name

	WWW::SwaggerClient::ApiFactory - constructs APIs to retrieve SwaggerClient objects

=head1 Synopsis

	package My::Petstore::App;
	
	use WWW::SwaggerClient::ApiFactory;
	
	my $api_factory = WWW::SwaggerClient::ApiFactory->new( base_url => 'http://petstore.swagger.io/v2',
							  								..., # other args for ApiClient constructor
							  								);
							  
	# later...
	my $pet_api = $api_factory->get_api('Pet');  
	
	# $pet_api isa WWW::SwaggerClient::PetApi
	
	my $pet = $pet_api->get_pet_by_id(pet_id => $pet_id);
	
	# object attributes have proper accessors:
	printf "Pet's name is %s", $pet->name;
	
	# change the value stored on the object:
	$pet->name('Dave'); 

=cut

# Load all the API classes and construct a lookup table at startup time
my %_apis = map { $_ =~ /^WWW::SwaggerClient::(.*)$/; $1 => $_ } 
			grep {$_ =~ /Api$/} 
			usesub 'WWW::SwaggerClient';

=head1 new()
	
	All parameters are optional, and are passed to and stored on the api_client object. 
	
	base_url: supply this to change the default base URL taken from the Swagger definition.
	
	auth_setup_handler: a coderef you can supply to set up authentication.
	
		The coderef receives a hashref with keys: api_client, query_params, header_params, auth_settings.
		
		my $api_factory = WWW::SwaggerClient::ApiFactory->new( auth_setup_handler => \&setup_auth );					  								);
	
		sub setup_auth {
			my %p = @_;
			$p{header_params}->{'X-SomeApp-FunkyKeyName'} = 'aaaaabbbbbcccccddddd';
		}
		
=cut	

sub new {
    my ($class, %p) = (shift, @_);
	$p{api_client} = WWW::SwaggerClient::ApiClient->new(%p);			
	return bless \%p, $class;
}

=head1 get_api($which)

	Returns an API object of the requested type. 
	
	$which is a nickname for the class: 
	
		WWW::FooBarClient::BazApi has nickname 'Baz'
		
=cut

sub get_api {
	my ($self, $which) = @_;
	croak "API not specified" unless $which;
	my $api_class = $_apis{"${which}Api"} || croak "No known API for '$which'";
	return $api_class->new(api_client => $self->api_client); 
}

=head1 api_client()

	Returns the api_client object, should you ever need it.
	
=cut

sub api_client { $_[0]->{api_client} }

1;
