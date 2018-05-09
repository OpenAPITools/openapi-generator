#region Import functions

'API', 'Model', 'Private' | Get-ChildItem -Path {
    Join-Path $PSScriptRoot $_
} -Filter '*.ps1' | ForEach-Object {
    Write-Verbose "Importing file: $($_.BaseName)"
    try {
        . $_.FullName
    } catch {
        Write-Verbose "Can't import function!"
    }
}

#endregion


#region Initialize APIs

'Creating object: Org.OpenAPITools.Api.PetApi' | Write-Verbose
$Script:PetApi= New-Object -TypeName Org.OpenAPITools.Api.PetApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.StoreApi' | Write-Verbose
$Script:StoreApi= New-Object -TypeName Org.OpenAPITools.Api.StoreApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.UserApi' | Write-Verbose
$Script:UserApi= New-Object -TypeName Org.OpenAPITools.Api.UserApi -ArgumentList @($null)


#endregion
