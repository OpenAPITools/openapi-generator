# frozen_string_literal: true

module Petstore
  module Api
    class User
      def initialize(connection)
        @connection = connection
      end

      def create(user:)
        raise ArgumentError, 'user is required' if user.nil?

        @connection.call(
          :POST,
          '/user',
          type: nil,
          auth: ['api_key'],
          body: user
        )
      end

      def create_with_array(user:)
        raise ArgumentError, 'user is required' if user.nil?

        @connection.call(
          :POST,
          '/user/createWithArray',
          type: nil,
          auth: ['api_key'],
          body: user
        )
      end

      def create_with_list(user:)
        raise ArgumentError, 'user is required' if user.nil?

        @connection.call(
          :POST,
          '/user/createWithList',
          type: nil,
          auth: ['api_key'],
          body: user
        )
      end

      def delete(username:)
        raise ArgumentError, 'username is required' if username.nil?

        @connection.call(
          :DELETE,
          '/user/{username}'
            .gsub('{username}', ERB::Util.url_encode(username.to_s)),
          type: nil,
          auth: ['api_key']
        )
      end

      def get(username:)
        raise ArgumentError, 'username is required' if username.nil?

        @connection.call(
          :GET,
          '/user/{username}'
            .gsub('{username}', ERB::Util.url_encode(username.to_s)),
          type: Petstore::Models::User,
          auth: []
        )
      end

      def login(username:, password:)
        raise ArgumentError, 'username is required' if username.nil?
        raise ArgumentError, 'password is required' if password.nil?

        @connection.call(
          :GET,
          '/user/login',
          type: nil,
          auth: [],
          query: { 'username' => username, 'password' => password }
        )
      end

      def logout
        @connection.call(
          :GET,
          '/user/logout',
          type: nil,
          auth: ['api_key']
        )
      end

      def update(username:, user:)
        raise ArgumentError, 'username is required' if username.nil?
        raise ArgumentError, 'user is required' if user.nil?

        @connection.call(
          :PUT,
          '/user/{username}'
            .gsub('{username}', ERB::Util.url_encode(username.to_s)),
          type: nil,
          auth: ['api_key'],
          body: user
        )
      end
    end
  end
end
