#region Import functions

# store the API client's configuration 
$Script:Configuration = [System.Collections.HashTable]@{}

$Script:CmdletBindingParameters = @('Verbose','Debug','ErrorAction','WarningAction','InformationAction','ErrorVariable','WarningVariable','InformationVariable','OutVariable','OutBuffer','PipelineVariable')

'Api', 'Model', 'Client', 'Private' | Get-ChildItem -Path {
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

#TODO remove the following
#region Initialize APIs

#'Creating object: PSPetstore.Api.PSPetApi' | Write-Verbose
#$Script:PSPetApi= New-Object -TypeName PSPetstore.Api.PSPetApi -ArgumentList @($null)

#'Creating object: PSPetstore.Api.PSStoreApi' | Write-Verbose
#$Script:PSStoreApi= New-Object -TypeName PSPetstore.Api.PSStoreApi -ArgumentList @($null)

#'Creating object: PSPetstore.Api.PSUserApi' | Write-Verbose
#$Script:PSUserApi= New-Object -TypeName PSPetstore.Api.PSUserApi -ArgumentList @($null)


#endregion
