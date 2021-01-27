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
#try {
    $pet = Initialize-PSPet -Id $Id -Name 'foo' -Category (
        Initialize-PSCategory -Id $Id -Name 'bar'
    ) -PhotoUrls @(
        'http://example.com/foo',
        'http://example.com/bar'
    ) -Tags (
        Initialize-PSTag -Id 3 -Name 'baz'
    ) -Status Available
    
    #Write-Host $pet
    #$Result = Add-PSPet -Pet $pet
    $Result = Add-PSPet -Pet $null
    Set-PSConfigurationApiKey -Id "api_key" -ApiKey "zzZZZZZZZZZZZZZ"
    $Result2 = Get-PSPetById -petId ($Id) -Verbose -WithHttpInfo #-testHeader "testing only" -testQuery "testing something here"
    Write-Host $Result2["Headers"]["Content-Type"]
    #$Result3 = Get-PSPetById -petId ($Id) -Verbose -WithHttpInfo -ReturnType "application/xml" #-testHeader "testing only" -testQuery "testing something here"
    #Write-Host $Result3["Headers"]["Content-Type"]
    #Write-Host $Result3["Response"]
#} catch {
#    Write-Host ("Exception occured when calling '': {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
#    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
#}

#$Result = Add-PSPet -Pet $pet -ReturnType "application/xml"
#Write-Host "Before exit $($Result2.GetType())"

#$result | Write-Host

#$result | Select-Object -Property "photoUrls" | ConvertTo-Json | Write-Host
#Write-Host "result =" + $result.photoUrls


#$pet2 = Initialize-PSPet -Id 20129 -Name '2foo' -Category (
#    Initialize-PSCategory -Id 20129 -Name '2bar'
#) -PhotoUrls @(
#    'http://example.com/2foo',
#    'http://example.com/2bar'
#) -Tags (
#    Initialize-PSTag -Id 3 -Name 'baz'
#) -Status Available
#
##Write-Host $pet
#$Result = Add-PSPet -Pet $pet2
#
#$Result = Find-PSPetsByTags 'baz'
#Write-Host $Result.GetType().Name
#Write-Host $Result

#$Result = Invoke-PetApiUpdatePetWithForm -petId $Id -Name "PowerShell Update" -Status "Pending"

#$file = Get-Item "./plus.gif"
##$Result = Invoke-PetApiUploadFile -petId $Id -additionalMetadata "Additional data" -File $file
#
#Set-PSConfiguration -Username "test_username" -Password "test_password"
#
#$conf = Get-PSConfiguration
#
#$conf | ConvertTo-Json | Write-Host

