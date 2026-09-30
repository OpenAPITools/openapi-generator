require "json"

module Petstore
  module Api
  class User
    def initialize(@conn : Connection); end

    # Create user This can only be done by the logged in user.
    def create(user : Petstore::User) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/user",
        body: user,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # Creates list of users with given input array 
    def create_with_array(user : Array(Petstore::User)) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/user/createWithArray",
        body: user,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # Creates list of users with given input array 
    def create_with_list(user : Array(Petstore::User)) : Response(Nil)
      @conn.request(Nil,
        method: :POST,
        path: "/user/createWithList",
        body: user,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end

    # Delete user This can only be done by the logged in user.
    def delete(username : String) : Response(Nil)
      @conn.request(Nil,
        method: :DELETE,
        path: "/user/{username}".sub("{username}", Petstore.enc(username)),
        accept: %w[],
        auth: %w[])
    end

    # Get user by user name 
    def get(username : String) : Response(Petstore::User)
      @conn.request(Petstore::User,
        method: :GET,
        path: "/user/{username}".sub("{username}", Petstore.enc(username)),
        accept: %w[application/xml application/json],
        auth: %w[])
    end

    # Logs user into the system 
    def login(*, username : String? = nil, password : String? = nil) : Response(String)
      @conn.request(String,
        method: :GET,
        path: "/user/login",
        query: { "username" => username, "password" => password },
        accept: %w[application/xml application/json],
        auth: %w[])
    end

    # Logs out current logged in user session 
    def logout() : Response(Nil)
      @conn.request(Nil,
        method: :GET,
        path: "/user/logout",
        accept: %w[],
        auth: %w[])
    end

    # Updated user This can only be done by the logged in user.
    def update(username : String, user : Petstore::User) : Response(Nil)
      @conn.request(Nil,
        method: :PUT,
        path: "/user/{username}".sub("{username}", Petstore.enc(username)),
        body: user,
        accept: %w[],
        content_type: %w[application/json],
        auth: %w[])
    end
  end
  end

end
