function New-Order {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${id},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${petId},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int32]]
        ${quantity},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[System.DateTime]]
        ${shipDate},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${status},
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Boolean]]
        ${complete}
    )

    Process {
        'Creating object: IO.Swagger.Model.Order' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        New-Object -TypeName IO.Swagger.Model.Order -ArgumentList @(
            ${id},
            ${petId},
            ${quantity},
            ${shipDate},
            ${status},
            ${complete}
        )
    }
}
