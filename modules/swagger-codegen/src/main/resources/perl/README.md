# NAME

WWW::{{moduleName}}::Role - a Moose role for the Perl Swagger Codegen project

## A note on Moose

This role is the only component of the library that uses Moose. See 
WWW::{{moduleName}}::ApiFactory for non-Moosey usage. 

# SYNOPSIS

The Perl Swagger Codegen project builds a library of Perl modules to interact with 
a web service defined by a Swagger specification. See below for how to build the 
library.

This module provides an interface to the generated library. All the classes, 
objects, and methods (well, not quite \*all\*, see below) are flattened into this 
role. 

        package MyApp;
        use Moose;
        has [qw(username password)] => ( is => 'ro', required => 1, isa => 'Str' );
        with 'WWW::{{moduleName}}::Role';
        sub auth_setup_handler {...}
        
        package main;
        
        my $api = MyApp->new({username => $username, password => $password});
        
        my $pet = $api->get_pet_by_id(pet_id => $pet_id);
        

Notice that you need to provide the code to accept the parameters passed in to `new()`
(by setting up attributes via the `has` keyword). They should be used by 
`auth_setup_handler()` to configure authentication (see below). 

## Structure of the library

The library consists of a set of API classes, one for each endpoint. These APIs
implement the method calls available on each endpoint. 

Additionally, there is a set of "object" classes, which represent the objects 
returned by and sent to the methods on the endpoints. 

An API factory class is provided, which builds instances of each endpoint API. 

This Moose role flattens all the methods from the endpoint APIs onto the consuming 
class. It also provides methods to retrieve the endpoint API objects, and the API 
factory object, should you need it. 

For documentation of all these methods, see AUTOMATIC DOCUMENTATION below.

# METHODS

## `auth_setup_handler()`

This method is NOT provided - you must write it yourself. Its task is to configure 
authentication for each request. 

The method is called on your `$api` object and passed the following parameters:

- `header_params`

    A hashref that will become the request headers. You can insert auth 
    parameters.

- `query_params`

    A hashref that will be encoded into the request URL. You can insert auth 
    parameters.

- `auth_settings`

    TODO.

- `api_client`

    A reference to the `WWW::{{moduleName}}::ApiClient` object that is responsible 
    for communicating with the server. 

For example: 

        sub auth_setup_handler {
                my ($self, %p) = @_;
                $p{header_params}->{'X-TargetApp-apiKey'} = $api_key;
                $p{header_params}->{'X-TargetApp-secretKey'} = $secret_key;
        }

## base\_url

The generated code has the `base_url` already set as a default value. This method 
returns (and optionally sets) the current value of `base_url`.

## api\_factory

Returns an API factory object. You probably won't need to call this directly. 

        $self->api_factory('Pet'); # returns a WWW::{{moduleName}}::PetApi instance
        
        $self->pet_api;            # the same

# MISSING METHODS

Most of the methods on the API are delegated to individual sub-API objects (e.g. 
Pet API, Store API, User API etc). Where different sub-APIs use the same method 
name (e.g. `new()`), these methods can't be delegated. So you need to call 
`$api->pet_api->new()`. 

In principle, every API is susceptible to the presence of a few, random, undelegatable 
method names. In practice, because of the way method names are constructed, it's 
unlikely in general that any methods will be undelegatable, except for: 

        new()
        class_documentation()
        method_documentation()

To call these methods, you need to get a handle on the relevant object, either 
by calling `$api->foo_api` or by retrieving an object, e.g. 
`$api->get_pet_by_id(pet_id => $pet_id)`.

# BUILDING YOUR LIBRARY

See the homepage `https://github.com/swagger-api/swagger-codegen` for full details. 
But briefly, clone the git repository, build the codegen codebase, set up your build 
config file, then run the API build script. You will need git, Java 7 and Apache 
maven 3.0.3 or better already installed.

The config file should specify the project name for the generated library: 

        {"moduleName":"MyProjectName"}

Your library files will be built under `WWW::MyProjectName`.

          $ git clone https://github.com/swagger-api/swagger-codegen.git
          $ cd swagger-codegen
          $ mvn package
          $ java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
    -i [URL or file path to JSON swagger API spec] \
    -l perl \
    -c /path/to/config/file.json \
    -o /path/to/output/folder

Bang, all done. Run the `autodoc` script in the `bin` directory to see the API 
you just built. 

# AUTOMATIC DOCUMENTATION

You can print out a summary of the generated API by running the included 
`autodoc` script in the `bin` directory of your generated library.

# DOCUMENTATION FROM THE SWAGGER SPEC

Additional documentation for each class and method may be provided by the Swagger 
spec. If so, this is available via the `class_documentation()` and 
`method_documentation()` methods on each generated API and class: 

        my $cdoc = $api->pet_api->class_documentation;                   
        my $cmdoc = $api->pet_api->method_documentation->{$method_name}; 
        
        my $odoc = $api->get_pet_by_id->(pet_id => $pet_id)->class_documentation;                  
        my $omdoc = $api->get_pet_by_id->(pet_id => $pet_id)->method_documentation->{method_name}; 
        

Each of these calls returns a hashref with various useful pieces of information. 	
