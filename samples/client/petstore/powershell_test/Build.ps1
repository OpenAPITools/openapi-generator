function Get-FunctionsToExport {
    [CmdletBinding()]
    Param (
        [Parameter(Mandatory = $true, ValueFromPipelineByPropertyName = $true)]
        [ValidateNotNullOrEmpty()]
        [Alias('FullName')]
        $Path
    )

    Process {
        $Token = $null
        $ParserErr = $null

        $Ast = [System.Management.Automation.Language.Parser]::ParseFile(
            $Path,
            [ref]$Token,
            [ref]$ParserErr
        )

        if ($ParserErr) {
            throw $ParserErr
        } else {
            foreach ($name in 'Begin', 'Process', 'End') {
	            foreach ($Statement in $Ast."${name}Block".Statements) {
		            if (
                        [String]::IsNullOrWhiteSpace($Statement.Name) -or
                        $Statement.Extent.ToString() -notmatch
                        ('function\W+{0}' -f $Statement.Name)
                    ) {
			            continue
		            }

		            $Statement.Name
	            }
            }
        }
    }
}

$ScriptDir = Split-Path $script:MyInvocation.MyCommand.Path
$ClientPath = ("$ScriptDir\..\..\petstore\csharp\SwaggerClient" | Resolve-Path).ProviderPath
$FunctionPath = 'API', 'Model' | ForEach-Object {Join-Path "$ScriptDir\src\IO.Swagger\" $_}
$BinPath = "$ScriptDir\src\IO.Swagger\Bin"

Start-Process -FilePath "$ClientPath\build.bat" -WorkingDirectory $ClientPath -Wait -NoNewWindow

if (!(Test-Path "$ScriptDir\src\IO.Swagger\Bin" -PathType Container)) {
    New-Item "$ScriptDir\src\IO.Swagger\Bin" -ItemType Directory > $null
}

Copy-Item "$ClientPath\bin\*.dll" $BinPath

$Manifest = @{
    Path = "$ScriptDir\src\IO.Swagger\IO.Swagger.psd1"

    Author = 'Swagger Codegen Team'
    CompanyName = 'swagger.io'
    Description = 'IO.Swagger - the PowerShell module for Swagger Petstore'

    RootModule = 'IO.Swagger.psm1'
    Guid = 'a27b908d-2a20-467f-bc32-af6f3a654ac5' # Has to be static, otherwise each new build will be considered different module

    PowerShellVersion = '3.0'

    RequiredAssemblies = Get-ChildItem "$BinPath\*.dll" | ForEach-Object {
        Join-Path $_.Directory.Name $_.Name
    }

    FunctionsToExport = $FunctionPath | Get-ChildItem -Filter *.ps1 | Get-FunctionsToExport

    VariablesToExport = @()
    AliasesToExport = @()
    CmdletsToExport = @()

    # Should we use prefix to prevent command name collisions?
    # https://www.sapien.com/blog/2016/02/15/use-prefixes-to-prevent-command-name-collision/
    #
    # Kirk Munro recommends against it:
    # https://www.sapien.com/blog/2016/02/15/use-prefixes-to-prevent-command-name-collision/#comment-20820
    #
    # If not, we'd need to generate functions name with prefix.
    #
    # DefaultCommandPrefix = 'PetStore'
}

New-ModuleManifest @Manifest
