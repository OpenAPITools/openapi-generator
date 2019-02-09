function New-User {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${id},
        [Parameter(Position = 1, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${username},
        [Parameter(Position = 2, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${firstName},
        [Parameter(Position = 3, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${lastName},
        [Parameter(Position = 4, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${email},
        [Parameter(Position = 5, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${password},
        [Parameter(Position = 6, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${phone},
        [Parameter(Position = 7, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int32]]
        ${userStatus}
    )

    Process {
        'Creating object: Org.OpenAPITools.Model.User' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        New-Object -TypeName Org.OpenAPITools.Model.User -ArgumentList @(
            ${id},
            ${username},
            ${firstName},
            ${lastName},
            ${email},
            ${password},
            ${phone},
            ${userStatus}
        )
    }
}
