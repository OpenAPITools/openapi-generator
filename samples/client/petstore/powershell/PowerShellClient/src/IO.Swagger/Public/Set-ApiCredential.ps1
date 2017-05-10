<#
.Synopsis
    Set PetStore Authorization data.

.Description
    Set PetStore Authorization data.

.Parameter ApiKey
    API key.

.Parameter ApiKeyPrefix
    API Key prefix.

.Parameter AccessToken
    Access Token.

.Example
    Set-ApiCredential -ApiKey 'foo'

.Example
    Set-ApiCredential -ApiKey 'foo' -ApiPrefix 'Bearer'

.Example
    Set-ApiCredential -ApiKey 'foo' -ApiPrefix 'Bearer'

.Example
    Set-ApiCredential -AccessToken 'YOUR_ACCESS_TOKEN'
#>
function Set-ApiCredential {
    [CmdletBinding(DefaultParameterSetName = 'ApiKey')]
    Param (
        [Parameter(Position = 0, ParameterSetName = 'ApiKey')]
        [string]
        ${ApiKey},

        [Parameter(Position = 1, ParameterSetName = 'ApiKey')]
        [string]
        ${ApiKeyPrefix},

        [Parameter(Position = 2, ParameterSetName = 'AccessToken')]
        [string]
        ${AccessToken}
    )

    End {
        if (${ApiKey}) {
            ([IO.Swagger.Client.Configuration]::Default).ApiKey.Add(
                'api_key',
                ${ApiKey}
            )
        }

        if ($ApiKeyPrefix) {
            ([IO.Swagger.Client.Configuration]::Default).ApiKeyPrefix.Add(
                'api_key',
                ${ApiKeyPrefix}
            )
        }

        if (${AccessToken}) {
            ([IO.Swagger.Client.Configuration]::Default).AccessToken = ${AccessToken}
        }
   }
}