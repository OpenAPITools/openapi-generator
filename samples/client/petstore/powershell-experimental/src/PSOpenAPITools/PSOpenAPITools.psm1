#region Import functions

# store the API client's configuration 
$Script:Configuration = [System.Collections.HashTable]@{}

$Script:CmdletBindingParameters = @('Verbose','Debug','ErrorAction','WarningAction','InformationAction','ErrorVariable','WarningVariable','InformationVariable','OutVariable','OutBuffer','PipelineVariable')

'API', 'Model', 'Client', 'Private' | Get-ChildItem -Path {
    Join-Path $PSScriptRoot $_
} -Filter '*.ps1' | ForEach-Object {
    Write-Host "Importing file: $($_.BaseName)"
    try {
        . $_.FullName
    } catch {
        Write-Error -Message "Failed to import function $($_.Fullname): $_"
    }
}

#endregion


#region Initialize APIs

#'Creating object: PSOpenAPITools.Api.PetApi' | Write-Verbose
#$Script:PetApi= New-Object -TypeName PSOpenAPITools.Api.PetApi -ArgumentList @($null)

#'Creating object: PSOpenAPITools.Api.StoreApi' | Write-Verbose
#$Script:StoreApi= New-Object -TypeName PSOpenAPITools.Api.StoreApi -ArgumentList @($null)

#'Creating object: PSOpenAPITools.Api.UserApi' | Write-Verbose
#$Script:UserApi= New-Object -TypeName PSOpenAPITools.Api.UserApi -ArgumentList @($null)


#endregion
