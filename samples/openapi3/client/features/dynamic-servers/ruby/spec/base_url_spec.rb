require 'spec_helper'

describe DynamicServers::Configuration do
  describe '#base_url' do
    it 'should allow variables override' do
      [
        ["https://custom-petstore.swagger.io:8080/v2", {}],
        [
          "https://custom-petstore.swagger.io:80/v2",
          {
              "server_index": 0,
              "server_variables": {"port": "80"}  # global override
          }
        ],[
          "https://custom-petstore.swagger.io:8080/v2",
          {
              "server_index": 0,
              "server_variables": {"port": "80"},
              "server_operation_variables": {"UsageApi.custom_server": {"port": "8080"}}  # operation override
          }
        ],[
          "https://third.example.com/global-prefix",
          {
              "server_index": 2,
              "server_variables": {"prefix": "global-prefix"}  # global override
          }
        ],[
          "https://third.example.com/local-prefix",
          {
              "server_index": 1,
              "server_variables": {"prefix": "global-prefix"},
              "server_operation_index": {"UsageApi.custom_server": 2},
              "server_operation_variables": {"UsageApi.custom_server": {"prefix": "local-prefix"}}  # operation override
          }
        ]
      ].each do |expected_url, params|
        c = DynamicServers::Configuration.new
        c.server_index = params[:server_index] if params.key?(:server_index)
        c.server_variables = params[:server_variables] if params.key?(:server_variables)
        c.server_operation_index = params[:server_operation_index] if params.key?(:server_operation_index)
        c.server_operation_variables = params[:server_operation_variables] if params.key?(:server_operation_variables)

        expect(c.base_url(:"UsageApi.custom_server")).to eq(expected_url)
      end
    end

    it 'should respect default settings' do
      [
        ["http://petstore.swagger.io:80/v2", {}],
        [
            "http://dev-petstore.swagger.io:8080/v2",
            {
                "server_index": 0,
                "server_variables": {"server": "dev-petstore", "port": "8080"}
            }
          ],
        ["https://localhost:8080/v1", {"server_index": 1}],
        [
            "https://localhost:8080/v3",
            {"server_index": 1, "server_variables": {"version": "v3"}}
        ]
      ].each do|expected_url, params|
        c = DynamicServers::Configuration.new
        c.server_index = params[:server_index] if params.key?(:server_index)
        c.server_variables = params[:server_variables] if params.key?(:server_variables)

        expect(c.base_url).to eq(expected_url)
      end
    end
  end
end
