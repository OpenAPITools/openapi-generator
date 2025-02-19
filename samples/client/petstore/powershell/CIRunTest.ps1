# assuming the current directory is already set correctly
#cd samples\client\petstore\powershell\
$ErrorActionPreference = "Stop"

.\Build.ps1

Import-Module -Name '.\src\PSPetstore'

$Result = Invoke-Pester -PassThru

if ($Result.FailedCount -gt 0) {
    $host.SetShouldExit($Result.FailedCount)
    exit $Result.FailedCount
}
