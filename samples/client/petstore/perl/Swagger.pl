package WWW::Swagger::Swagger;

use strict;
use warnings;
use utf8;

use LWP::UserAgent;
use HTTP::Headers;
use HTTP::Response;
use HTTP::Status;
use URI::Query;
use JSON;

use Scalar::Util;

# class variables
my $ua = LWP::UserAgent->new;
my $http_user_agent = 'Perl-Swagger'; # HTTP user-agent
my $http_timeout; #timeout
my $base_url;


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
  if ($query_params) {
    $_url = ($_url . '?' . eval { URI::Query->new($query_params)->stringify });
  }

  # body data
  my $_body_data = $post_params ? $post_params : $body_data;

  # Make the HTTP request
  my $_request = HTTP::Request->new(
    $method, $_url, $headers, $_body_data
  );
 
  $ua->timeout($http_timeout); 
  $ua->agent($http_user_agent);
  my $_response = $ua->request($_request);

  if (!$_response->is_success) {
    #TODO croak("Can't connect ot the api ($_response{code}): $_response{message}");
  }
     
  return $_response->content;

}


# Build a JSON POST object
#sub sanitize_for_serialization
#{
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
#}


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
# @param object $object object or primitive to be deserialized
# @param string $class class name is passed as a string
# @return object an instance of $class
#sub deserialize
#{
#  my ($data, $class) = @_;
#  if (null === $data) {
#    $deserialized = null;
#  } elseif (substr($class, 0, 4) == 'map[') {
#    $inner = substr($class, 4, -1);
#    $values = array();
#    if(strrpos($inner, ",") !== false) {
#      $subClass_array = explode(',', $inner, 2);
#      $subClass = $subClass_array[1];
#      foreach ($data as $key => $value) {
#        $values[] = array($key => self::deserialize($value, $subClass));
#      }        
#    }
#    $deserialized = $values;
#  } elseif (strcasecmp(substr($class, 0, 6),'array[') == 0) {
#    $subClass = substr($class, 6, -1);
#    $values = array();
#    foreach ($data as $key => $value) {
#      $values[] = self::deserialize($value, $subClass);
#    }
#    $deserialized = $values;
#  } elseif ($class == 'DateTime') {
#    $deserialized = new \DateTime($data);
#  } elseif (in_array($class, array('string', 'int', 'float', 'bool'))) {
#    settype($data, $class);
#    $deserialized = $data;
#  } else {
#    $instance = new $class();
#    foreach ($instance::$swaggerTypes as $property => $type) {
#      if (isset($data->$property)) {
#        $original_property_name = $instance::$attributeMap[$property];
#        $instance->$property = self::deserialize($data->$original_property_name, $type);
#      }
#    }
#    $deserialized = $instance;
#  }
#
#  return $deserialized;
#}

1;
