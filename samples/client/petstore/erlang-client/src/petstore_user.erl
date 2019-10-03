-module(petstore_user).

-export([encode/1]).

-export_type([petstore_user/0]).

-type petstore_user() ::
    #{ 'id' => integer(),
       'username' => binary(),
       'firstName' => binary(),
       'lastName' => binary(),
       'email' => binary(),
       'password' => binary(),
       'phone' => binary(),
       'userStatus' => integer()
     }.

encode(#{ 'id' := Id,
          'username' := Username,
          'firstName' := FirstName,
          'lastName' := LastName,
          'email' := Email,
          'password' := Password,
          'phone' := Phone,
          'userStatus' := UserStatus
        }) ->
    #{ 'id' => Id,
       'username' => Username,
       'firstName' => FirstName,
       'lastName' => LastName,
       'email' => Email,
       'password' => Password,
       'phone' => Phone,
       'userStatus' => UserStatus
     }.
