function Invoke-PetApiAddPet {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.Pet]
        ${body}
    )

    Process {
        'Calling method: PetApi-AddPet' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri '/pet' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.AddPet(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

function Invoke-PetApiDeletePet {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Int64]
        ${petId},
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [String]
        ${apiKey}
    )

    Process {
        'Calling method: PetApi-DeletePet' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'DELETE' `
                                -Uri '/pet/{petId}' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.DeletePet(
        #
        #    
        #    ${petId},
        #    
        #
        #    
        #    ${apiKey}
        #    
        #
        #)
    }
}

function Invoke-PetApiFindPetsByStatus {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String[]]
        ${status}
    )

    Process {
        'Calling method: PetApi-FindPetsByStatus' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri '/pet/findByStatus' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.FindPetsByStatus(
        #
        #    
        #    ${status}
        #    
        #
        #)
    }
}

function Invoke-PetApiFindPetsByTags {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String[]]
        ${tags}
    )

    Process {
        'Calling method: PetApi-FindPetsByTags' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri '/pet/findByTags' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.FindPetsByTags(
        #
        #    
        #    ${tags}
        #    
        #
        #)
    }
}

function Invoke-PetApiGetPetById {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Int64]
        ${petId}
    )

    Process {
        'Calling method: PetApi-GetPetById' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri '/pet/{petId}' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.GetPetById(
        #
        #    
        #    ${petId}
        #    
        #
        #)
    }
}

function Invoke-PetApiUpdatePet {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.Pet]
        ${body}
    )

    Process {
        'Calling method: PetApi-UpdatePet' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'PUT' `
                                -Uri '/pet' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.UpdatePet(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

function Invoke-PetApiUpdatePetWithForm {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Int64]
        ${petId},
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [String]
        ${name},
        [Parameter(Position = 2, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [String]
        ${status}
    )

    Process {
        'Calling method: PetApi-UpdatePetWithForm' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri '/pet/{petId}' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.UpdatePetWithForm(
        #
        #    
        #    ${petId},
        #    
        #
        #    
        #    ${name},
        #    
        #
        #    
        #    ${status}
        #    
        #
        #)
    }
}

function Invoke-PetApiUploadFile {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Int64]
        ${petId},
        [Parameter(Position = 1, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [String]
        ${additionalMetadata},
        [Parameter(Position = 2, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $false)]
        [String]
        ${file}
    )

    Process {
        'Calling method: PetApi-UploadFile' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri '/pet/{petId}/uploadImage' `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:PetApi.UploadFile(
        #
        #    
        #    ${petId},
        #    
        #
        #    
        #    ${additionalMetadata},
        #    
        #
        #    
        #    ${file}
        #    
        #
        #)
    }
}

