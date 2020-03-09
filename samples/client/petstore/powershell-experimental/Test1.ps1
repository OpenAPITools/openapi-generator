Remove-Module -FullyQualifiedName @{ModuleName = "Org.OpenAPITools"; ModuleVersion = "0.0.1"}

Import-Module -Name '.\src\Org.OpenAPITools'
#Import-Module -Name './src/Org.OpenAPITools'

Invoke-PetApiGetPetById -petId 2


New-Pet -Id 1 -Name 'foo' -Category (
    New-Category -Id 2 -Name 'bar'
) -PhotoUrls @(
    'http://example.com/foo',
    'http://example.com/bar'
) -Tags (
    New-Tag -Id 3 -Name 'baz'
) -Status Available

