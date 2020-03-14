#Remove-Module -FullyQualifiedName @{ModuleName = "Org.OpenAPITools"; ModuleVersion = "0.0.1"}
Remove-Module -FullyQualifiedName @{ModuleName = "PSOpenAPITools"; ModuleVersion = "0.0.1"}

Import-Module -Name '.\src\PSOpenAPITools'
#Import-Module -Name '.\src\Org.OpenAPITools'
#Import-Module -Name './src/Org.OpenAPITools'

$body = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)

$Id = 38369

$result = Invoke-PetApiGetPetById -petId $Id -testHeader "testing only" -testQuery "testing something here"

#$result | Select-Object -Property "photoUrls" | ConvertTo-Json | Write-Host
#Write-Host "result =" + $result.photoUrls

#$pet = New-Pet -Id 10129 -Name 'foo' -Category (
#    New-Category -Id 2 -Name 'bar'
#) -PhotoUrls @(
#    'http://example.com/foo',
#    'http://example.com/bar'
#) -Tags (
#    New-Tag -Id 3 -Name 'baz'
#) -Status Available
#
#Write-Host $pet
#$Result = Invoke-PetApiAddPet -Body $pet

#$Result = Invoke-PetApiUpdatePetWithForm -petId $Id -Name "PowerShell Update" -Status "Pending"

$file = Get-Item "./plus.gif"
#$Result = Invoke-PetApiUploadFile -petId $Id -additionalMetadata "Additional data" -File $file
