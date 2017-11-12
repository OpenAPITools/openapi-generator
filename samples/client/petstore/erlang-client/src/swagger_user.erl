-module(swagger_user).

-export_type([swagger_user/0,
              encode/1,
              decode/1]).

-type swagger_user() ::
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
     }
