function New-InlineObject1 {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipelineByPropertyName = $true)]
        [String]
        ${additionalMetadata},
        [Parameter(Position = 1, ValueFromPipelineByPropertyName = $true)]
        [System.Nullable[String]]
        ${file}
    )

    Process {
        'Creating object: Org.OpenAPITools.Model.InlineObject1' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        New-Object -TypeName Org.OpenAPITools.Model.InlineObject1 -ArgumentList @(
            ${additionalMetadata},
            ${file}
        )
    }
}
