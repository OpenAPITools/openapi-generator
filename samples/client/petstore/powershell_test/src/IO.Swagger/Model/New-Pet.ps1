function New-Pet {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${id},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[IO.Swagger.Model.Category]]
        ${category},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${name},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String[]]
        ${photoUrls},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[IO.Swagger.Model.Tag[]]]
        ${tags},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${status}
    )

    Process {
        'Creating object: IO.Swagger.Model.Pet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        New-Object -TypeName IO.Swagger.Model.Pet -ArgumentList @(
            ${id},
            ${category},
            ${name},
            ${photoUrls},
            ${tags},
            ${status}
        )
    }
}
