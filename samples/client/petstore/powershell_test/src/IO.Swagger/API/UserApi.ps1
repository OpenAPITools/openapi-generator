function Invoke-UserApiCreateUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.User]
        ${body}
    )

    Process {
        'Calling method: UserApi-CreateUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.CreateUser(
            ${body}
        )
    }
}

function Invoke-UserApiCreateUsersWithArrayInput {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.User[]]
        ${body}
    )

    Process {
        'Calling method: UserApi-CreateUsersWithArrayInput' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.CreateUsersWithArrayInput(
            ${body}
        )
    }
}

function Invoke-UserApiCreateUsersWithListInput {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.User[]]
        ${body}
    )

    Process {
        'Calling method: UserApi-CreateUsersWithListInput' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.CreateUsersWithListInput(
            ${body}
        )
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
        'Calling method: UserApi-DeleteUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.DeleteUser(
            ${username}
        )
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
        'Calling method: UserApi-GetUserByName' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.GetUserByName(
            ${username}
        )
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
        'Calling method: UserApi-LoginUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.LoginUser(
            ${username},
            ${password}
        )
    }
}

function Invoke-UserApiLogoutUser {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: UserApi-LogoutUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.LogoutUser(
        )
    }
}

function Invoke-UserApiUpdateUser {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${username},
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.User]
        ${body}
    )

    Process {
        'Calling method: UserApi-UpdateUser' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UserApi.UpdateUser(
            ${username},
            ${body}
        )
    }
}

