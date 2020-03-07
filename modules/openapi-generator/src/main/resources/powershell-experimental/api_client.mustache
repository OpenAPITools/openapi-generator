function Invoke-OpenAPIAPIClient {
  
    [CmdletBinding()]
    Param(
        [Parameter(Mandatory)]
        [string]$Uri,
        [Parameter(Mandatory)]
        [string]$Accept,
        [Parameter(Mandatory)]
        [string]$ContentType,
        [Parameter(Mandatory)]
        [string]$HeaderParameters,
        [Parameter(Mandatory)]
        [string]$FormParameters,
        [Parameter(Mandatory)]
        [string]$QueryParameters,
        [Parameter(Mandatory)]
        [string]$Body,
        [Parameter(Mandatory)]
        [string]$Method
    )

    $PSBoundParameters | Out-DebugParameter | Write-Host

    $RestMethod_Params = @{}
    $Configuraiton = Get-OpenAPIConfiguration

    #$RestMethod_Params['Uri'] = Write-Host "$($Configuration["BaseUrl"])$($Uri)"
    #Write-Host $RestMethod_Params['Uri']
    $RestMethod_Params['Uri'] = $Uri
    $RestMethod_Params['Method'] = $Method
    if (!$HeaderParameters -and $HeaderParameters.Count -ne 0) {
        $RestMethod_Params['Headers'] = $HeaderParameters
    }
    if (!$Accept -and $Accept.Count -ne 0) {
        $RestMethod_Params['Accept'] = $Accept
    }
    if (!$ContentType -and $ContenType.Count -ne 0) {
        $RestMethod_Params['Accept'] = $ContentType
    }
    if (!$Body) {
        $RestMethod_Params['Body'] = $Body
    }

    Invoke-RestMethod @RestMethod_Params
    #Invoke-RestMethod -Uri $Uri -ContentType 'multipart/form-data' -Method $Method -Headers $Headers -Body $Fields;
}
