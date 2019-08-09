-module(petstore_inline_object_1).

-export([encode/1]).

-export_type([petstore_inline_object_1/0]).

-type petstore_inline_object_1() ::
    #{ 'additionalMetadata' => binary(),
       'file' => binary()
     }.

encode(#{ 'additionalMetadata' := AdditionalMetadata,
          'file' := File
        }) ->
    #{ 'additionalMetadata' => AdditionalMetadata,
       'file' => File
     }.
