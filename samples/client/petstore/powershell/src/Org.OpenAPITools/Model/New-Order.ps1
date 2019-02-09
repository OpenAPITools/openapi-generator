function New-Order {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${id},
        [Parameter(Position = 1, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int64]]
        ${petId},
        [Parameter(Position = 2, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Int32]]
        ${quantity},
        [Parameter(Position = 3, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[System.DateTime]]
        ${shipDate},
        [Parameter(Position = 4, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${status},
        [Parameter(Position = 5, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[Boolean]]
        ${complete}
    )

    Process {
        'Creating object: Org.OpenAPITools.Model.Order' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        New-Object -TypeName Org.OpenAPITools.Model.Order -ArgumentList @(
            ${id},
            ${petId},
            ${quantity},
            ${shipDate},
            ${status},
            ${complete}
        )
    }
}
