#Remove-Module -FullyQualifiedName @{ModuleName = "Org.OpenAPITools"; ModuleVersion = "0.0.1"}
Remove-Module -FullyQualifiedName @{ModuleName = "PSOpenAPITools"; ModuleVersion = "0.0.1"}

Import-Module -Name '.\src\PSOpenAPITools'
#Import-Module -Name '.\src\Org.OpenAPITools'
#Import-Module -Name './src/Org.OpenAPITools'

#$result = Invoke-PetApiGetPetById -petId 2

#$result | Select-Object -Property "photoUrls" | ConvertTo-Json | Write-Host
#Write-Host "result =" + $result.photoUrls

$pet = New-Pet -Id 10129 -Name 'foo' -Category (
    New-Category -Id 2 -Name 'bar'
) -PhotoUrls @(
    'http://example.com/foo',
    'http://example.com/bar'
) -Tags (
    New-Tag -Id 3 -Name 'baz'
) -Status Available

Write-Host $pet
$Result = Invoke-PetApiAddPet -Body $pet


