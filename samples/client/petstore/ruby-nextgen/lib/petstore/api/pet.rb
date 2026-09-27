# frozen_string_literal: true

module Petstore
  module Api
    class Pet
      def initialize(connection)
        @connection = connection
      end

      def bulk_update(pet:)
        raise ArgumentError, 'pet is required' if pet.nil?

        @connection.call(
          :PUT,
          '/pet',
          type: Petstore::Models::Pet,
          auth: ['petstore_auth'],
          body: pet
        )
      end

      def create(pet:)
        raise ArgumentError, 'pet is required' if pet.nil?

        @connection.call(
          :POST,
          '/pet',
          type: Petstore::Models::Pet,
          auth: ['petstore_auth'],
          body: pet
        )
      end

      def create_post(pet_id:, name: nil, status: nil)
        raise ArgumentError, 'pet_id is required' if pet_id.nil?

        @connection.call(
          :POST,
          '/pet/{petId}'
            .gsub('{petId}', ERB::Util.url_encode(pet_id.to_s)),
          type: nil,
          auth: ['petstore_auth'],
          form: { 'name' => name, 'status' => status }
        )
      end

      def delete(pet_id:, api_key: nil)
        raise ArgumentError, 'pet_id is required' if pet_id.nil?

        @connection.call(
          :DELETE,
          '/pet/{petId}'
            .gsub('{petId}', ERB::Util.url_encode(pet_id.to_s)),
          type: nil,
          auth: ['petstore_auth'],
          headers: { 'api_key' => api_key }
        )
      end

      def find_by_status(status:)
        raise ArgumentError, 'status is required' if status.nil?

        @connection.call(
          :GET,
          '/pet/findByStatus',
          type: [Petstore::Models::Pet],
          auth: ['petstore_auth'],
          query: { 'status' => status }
        )
      end

      def find_by_tags(tags:)
        raise ArgumentError, 'tags is required' if tags.nil?

        @connection.call(
          :GET,
          '/pet/findByTags',
          type: [Petstore::Models::Pet],
          auth: ['petstore_auth'],
          query: { 'tags' => tags }
        )
      end

      def get(pet_id:)
        raise ArgumentError, 'pet_id is required' if pet_id.nil?

        @connection.call(
          :GET,
          '/pet/{petId}'
            .gsub('{petId}', ERB::Util.url_encode(pet_id.to_s)),
          type: Petstore::Models::Pet,
          auth: ['api_key']
        )
      end

      def upload_image(pet_id:, additional_metadata: nil, file: nil)
        raise ArgumentError, 'pet_id is required' if pet_id.nil?

        @connection.call(
          :POST,
          '/pet/{petId}/uploadImage'
            .gsub('{petId}', ERB::Util.url_encode(pet_id.to_s)),
          type: Petstore::Models::ApiResponse,
          auth: ['petstore_auth'],
          form: { 'additionalMetadata' => additional_metadata, 'file' => file }
        )
      end
    end
  end
end
