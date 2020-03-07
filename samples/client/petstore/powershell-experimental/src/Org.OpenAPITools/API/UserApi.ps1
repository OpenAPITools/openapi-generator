function Invoke-UserApiCreateUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.User]
        ${body}
    )

    Process {
        'Calling method: UserApi-CreateUser' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.CreateUser(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

function Invoke-UserApiCreateUsersWithArrayInput {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.User[]]
        ${body}
    )

    Process {
        'Calling method: UserApi-CreateUsersWithArrayInput' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.CreateUsersWithArrayInput(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

function Invoke-UserApiCreateUsersWithListInput {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.User[]]
        ${body}
    )

    Process {
        'Calling method: UserApi-CreateUsersWithListInput' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.CreateUsersWithListInput(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

function Invoke-UserApiDeleteUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${username}
    )

    Process {
        'Calling method: UserApi-DeleteUser' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $LocalVarUri.replace('{username}', $username)
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'DELETE' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.DeleteUser(
        #
        #    
        #    ${username}
        #    
        #
        #)
    }
}

function Invoke-UserApiGetUserByName {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${username}
    )

    Process {
        'Calling method: UserApi-GetUserByName' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $LocalVarUri.replace('{username}', $username)
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.GetUserByName(
        #
        #    
        #    ${username}
        #    
        #
        #)
    }
}

function Invoke-UserApiLoginUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${username},
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${password}
    )

    Process {
        'Calling method: UserApi-LoginUser' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri

        if (!$username) {
            throw "Error! $username is required."
        }

        $LocalVarQueryParameters['username'] = $username

        if (!$password) {
            throw "Error! $password is required."
        }

        $LocalVarQueryParameters['password'] = $password



        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.LoginUser(
        #
        #    
        #    ${username},
        #    
        #
        #    
        #    ${password}
        #    
        #
        #)
    }
}

function Invoke-UserApiLogoutUser {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: UserApi-LogoutUser' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.LogoutUser(
        #
        #)
    }
}

function Invoke-UserApiUpdateUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${username},
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.User]
        ${body}
    )

    Process {
        'Calling method: UserApi-UpdateUser' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $LocalVarUri.replace('{username}', $username)
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'PUT' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:UserApi.UpdateUser(
        #
        #    
        #    ${username},
        #    
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

