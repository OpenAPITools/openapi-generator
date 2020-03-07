function Invoke-StoreApiDeleteOrder {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${orderId}
    )

    Process {
        'Calling method: StoreApi-DeleteOrder' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $LocalVarUri.replace('{orderId}', $orderId)
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'DELETE' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:StoreApi.DeleteOrder(
        #
        #    
        #    ${orderId}
        #    
        #
        #)
    }
}

function Invoke-StoreApiGetInventory {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: StoreApi-GetInventory' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:StoreApi.GetInventory(
        #
        #)
    }
}

function Invoke-StoreApiGetOrderById {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Int64]
        ${orderId}
    )

    Process {
        'Calling method: StoreApi-GetOrderById' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $LocalVarUri.replace('{orderId}', $orderId)
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:StoreApi.GetOrderById(
        #
        #    
        #    ${orderId}
        #    
        #
        #)
    }
}

function Invoke-StoreApiPlaceOrder {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [Org.OpenAPITools.Model.Order]
        ${body}
    )

    Process {
        'Calling method: StoreApi-PlaceOrder' | Write-Host
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $LocalVarAccepts = @{}
        $LocalVarContentTypes = @{}
        $LocalVarHeaders = @{}
        $LocalVarQueryParameters = @{}
        $LocalVarFormParameters = @{}
        $LocalVarBodyParameters = @{}
        $LocalVarPathParameters = @{}

        $Configuraiton = Get-OpenAPIConfiguration

        $LocalVarUri = 'http://petstore.swagger.io/v2'
        $LocalVarUri = $Configuration["BaseUrl"] + $LocalVarUri



        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri $LocalVarUri `
                                -Accept $LocalVarAccepts `
                                -ContentType $LocalVarContentTypes `
                                -Body $LocalVarBodyParameters `
                                -HeaderParameters LocalVarHeaderParameters `
                                -QueryParameters LocalVarQueryParameters `
                                -FormParameters LocalVarFormParameters

        #$Script:StoreApi.PlaceOrder(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

