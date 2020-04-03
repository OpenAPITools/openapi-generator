#Remove-Module -FullyQualifiedName @{ModuleName = "Org.OpenAPITools"; ModuleVersion = "0.0.1"}
#Remove-Module -FullyQualifiedName @{ModuleName = "PSOpenAPITools"; ModuleVersion = "0.0.1"}
Remove-Module -FullyQualifiedName @{ModuleName = "PSPetstore"; ModuleVersion = "0.1.2"}
#Remove-Module -FullyQualifiedName @{ModuleName = "PSPetstore"; ModuleVersion = "0.0"}

Import-Module -Name '.\src\PSPetstore\PSPetstore.psd1' -Verbose
#Import-Module -Name '.\src\PSPetstore\PSPetstore.psd1' -Verbose
#Import-Module -Name '.\src\PSOpenAPITools'
#Import-Module -Name '.\src\Org.OpenAPITools'
#Import-Module -Name './src/Org.OpenAPITools'

#$DebugPreference = 'Continue'

$body = (Initialize-PSUser -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)

$Id = 38369

#$result = Update-PSPetWithForm 
try {
    Set-PSConfigurationApiKey -Id "api_key" -ApiKey "zzZZZZZZZZZZZZZ"
    $result = Get-PSPetById -petId $Id -Verbose #-testHeader "testing only" -testQuery "testing something here"
} catch {
    Write-Host ("Exception occured when calling '': {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}

#$result | Write-Host

#$result | Select-Object -Property "photoUrls" | ConvertTo-Json | Write-Host
#Write-Host "result =" + $result.photoUrls

#$pet = Initialize-Pet -Id 10129 -Name 'foo' -Category (
#    Initialize-Category -Id 2 -Name 'bar'
#) -PhotoUrls @(
#    'http://example.com/foo',
#    'http://example.com/bar'
#) -Tags (
#    Initialize-Tag -Id 3 -Name 'baz'
#) -Status Available
#
#Write-Host $pet
#$Result = Invoke-PetApiAddPet -Body $pet

#$Result = Invoke-PetApiUpdatePetWithForm -petId $Id -Name "PowerShell Update" -Status "Pending"

#$file = Get-Item "./plus.gif"
##$Result = Invoke-PetApiUploadFile -petId $Id -additionalMetadata "Additional data" -File $file
#
#Set-PSConfiguration -Username "test_username" -Password "test_password"
#
#$conf = Get-PSConfiguration
#
#$conf | ConvertTo-Json | Write-Host

