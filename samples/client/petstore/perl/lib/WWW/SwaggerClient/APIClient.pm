package WWW::SwaggerClient::APIClient;

use strict;
use warnings;
use utf8;

use LWP::UserAgent;
use HTTP::Headers;
use HTTP::Response;
use HTTP::Request::Common qw(DELETE POST GET HEAD PUT);
use HTTP::Status;
use URI::Query;
use JSON;
use URI::Escape;
use Scalar::Util;
use Log::Any qw($log);
use Carp;
use Switch;
use Module::Runtime qw(use_module);

# class variables
my $ua = LWP::UserAgent->new;
my $http_user_agent = 'Perl-Swagger'; # HTTP user-agent
my $http_timeout; #timeout
my $base_url = "http://petstore.swagger.io/v2";


sub new
{
  my $class = shift;
  my %args = @_;

  return bless \%args, $class;
}

# Set the user agent of the API client
#
# @param string $user_agent The user agent of the API client
#
sub set_user_agent {
  my $user_agent = shift;
  $http_user_agent= $user_agent;
}

# Set timeout
#
# @param integer $seconds Number of seconds before timing out [set to 0 for no timeout]
# 
sub set_timeout {
  my $seconds = shift;
  if (!looks_like_number($seconds)) {
    croak('Timeout variable must be numeric.');
  }
  $http_timeout = $seconds;
}

# make the HTTP request
# @param string $resourcePath path to method endpoint
# @param string $method method to call
# @param array $queryParams parameters to be place in query URL
# @param array $postData parameters to be placed in POST body
# @param array $headerParams parameters to be place in request header
# @return mixed
sub call_api {
  my $self = shift;
  my ($resource_path, $method, $query_params, $post_params, $header_params, $body_data) = @_;

  my $headers = HTTP::Headers->new(%$header_params);

  my $_url = $base_url . $resource_path;

  # build query 
  if (%$query_params) {
    $_url = ($_url . '?' . eval { URI::Query->new($query_params)->stringify });
  }

  # body data
  $body_data = to_json($body_data->to_hash) if defined $body_data && $body_data->can('to_hash'); # model to json string
  my $_body_data = %$post_params ? $post_params : $body_data;

  # Make the HTTP request
  my $_request;
  switch ($method) {
    case 'POST' {
      # multipart
      my $_content_type = lc $header_params->{'Content-Type'} eq 'multipart/form' ? 
          'form-data' : $header_params->{'Content-Type'};

      $_request = POST($_url, Accept => $header_params->{Accept},
        Content_Type => $_content_type, Content => $_body_data);
    }
    case 'PUT' {
      # multipart
      my $_content_type = lc $header_params->{'Content-Type'} eq 'multipart/form' ? 
          'form-data' : $header_params->{'Content-Type'};
      $_request = PUT($_url, Accept => $header_params->{Accept},
        Content_Type => $_content_type, Content => $_body_data);
    }
    case 'GET' {
      $_request = GET($_url, Accept => $header_params->{'Accept'},
        Content_Type => $header_params->{'Content-Type'});
    }
    case 'HEAD' {
      $_request = HEAD($_url, Accept => $header_params->{'Accept'},
        Content_Type => $header_params->{'Content-Type'});
    }
    case 'DELETE' { #TODO support form data
      $_request = DELETE($_url, Accept => $header_params->{'Accept'},
        Content_Type => $header_params->{'Content-Type'}, Content => $_body_data);
    }
    case 'PATCH' { #TODO
    }

  }
 
  $ua->timeout($http_timeout); 
  $ua->agent($http_user_agent);
  
  my $_response = $ua->request($_request);

  unless ($_response->is_success) {
    croak("API Exception(".$_response->code."): ".$_response->message);
  }
     
  return $_response->content;

}


# Build a JSON POST object
sub sanitize_for_serialization
{
#  my $data = shift;
#  if (is_scalar($data) || null === $data) {
#    $sanitized = $data;
#  } else if ($data instanceof \DateTime) {
#    $sanitized = $data->format(\DateTime::ISO8601);
#  } else if (is_array($data)) {
#    foreach ($data as $property => $value) {
#      $data[$property] = $this->sanitizeForSerialization($value);
#    }
#    $sanitized = $data;
#  } else if (is_object($data)) {
#    $values = array();
#    foreach (array_keys($data::$swaggerTypes) as $property) {
#      $values[$data::$attributeMap[$property]] = $this->sanitizeForSerialization($data->$property);
#    }
#    $sanitized = $values;
#  } else {
#    $sanitized = (string)$data;
#  }
#
#  return $sanitized;
}


#  Take value and turn it into a string suitable for inclusion in
#  the path, by url-encoding.
#  @param string $value a string which will be part of the path
#  @return string the serialized object
sub to_path_value {
    my $value = shift;
    return uri_escape(to_string($value));
}


# Take value and turn it into a string suitable for inclusion in
# the query, by imploding comma-separated if it's an object.
# If it's a string, pass through unchanged. It will be url-encoded
# later.
# @param object $object an object to be serialized to a string
# @return string the serialized object
sub to_query_value {
      my $object = shift;
      if (is_array($object)) {
          return implode(',', $object);
      } else {
          return toString($object);
      }
}


# Take value and turn it into a string suitable for inclusion in
# the header. If it's a string, pass through unchanged
# If it's a datetime object, format it in ISO8601
# @param string $value a string which will be part of the header
# @return string the header string
sub to_header_value {
    my $value = shift;
    return to_string($value);
}

# Take value and turn it into a string suitable for inclusion in
# the http body (form parameter). If it's a string, pass through unchanged
# If it's a datetime object, format it in ISO8601
# @param string $value the value of the form parameter
# @return string the form string
sub to_form_value {
    my $value = shift;
    return to_string($value);
}

# Take value and turn it into a string suitable for inclusion in
# the parameter. If it's a string, pass through unchanged
# If it's a datetime object, format it in ISO8601
# @param string $value the value of the parameter
# @return string the header string
sub to_string {
  my $value = shift;
  if (ref($value) eq "DateTime") { # datetime in ISO8601 format
    return $value->datetime();
  }
  else {
    return $value;
  }
}

# Deserialize a JSON string into an object
#  
# @param string $class class name is passed as a string
# @param string $data data of the body
# @return object an instance of $class
sub deserialize
{
  my ($self, $class, $data) = @_;
  $log->debugf("deserializing %s for %s", $data, $class);
  my $_result;

  if (not defined $data) {
    return undef;
  } elsif ( lc(substr($class, 0, 4)) eq 'map[') { #hash
    $_result = \(json_decode $data);
  } elsif ( lc(substr($class, 0, 6)) eq 'array[' ) { # array of data
    return $data if $data eq '[]'; # return if empty array

    my $_sub_class = substr($class, 6, -1);
    my @_json_data = json_decode $data;
    my @_values = ();
    foreach my $_value (@_json_data) {
      push @_values, $self->deserialize($_sub_class, $_value);
    }
    $_result = \@_values;
  } elsif ($class eq 'DateTime') {
    $_result = DateTime->from_epoch(epoch => str2time($data));
  } elsif (grep /^$data$/, ('string', 'int', 'float', 'bool')) { #TODO revise the primitive type
    $_result= $data;
  } else { # model
    my $_instance = use_module("WWW::SwaggerClient::Object::$class")->new;
    $_result = $_instance->from_hash(decode_json $data);
  }

  return $_result;

}

# return 'Accept' based on an array of accept provided
# @param [Array] header_accept_array Array fo 'Accept'
# @return String Accept (e.g. application/json)
sub select_header_accept
{
  my ($self, @header) = @_;

  if (@header == 0 || (@header == 1 && $header[0] eq '')) {
    return undef;
  } elsif (grep(/^application\/json$/i, @header)) {
    return 'application/json';
  } else {
    return join(',', @header);
  }

}

# return the content type based on an array of content-type provided
# @param [Array] content_type_array Array fo content-type
# @return String Content-Type (e.g. application/json)
sub select_header_content_type
{
  my ($self, @header) = @_;

  if (@header == 0 || (@header == 1 && $header[0] eq '')) {
    return 'application/json'; # default to application/json
  } elsif (grep(/^application\/json$/i, @header)) {
    return 'application/json';
  } else {
    return join(',', @header);
  }

}


1;
