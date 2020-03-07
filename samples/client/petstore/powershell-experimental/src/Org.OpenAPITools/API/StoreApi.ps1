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

        Invoke-OpenAPIAPIClient -Method 'DELETE' `
                                -Uri '/store/order/{orderId}' `
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

        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri '/store/inventory' `
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

        Invoke-OpenAPIAPIClient -Method 'GET' `
                                -Uri '/store/order/{orderId}' `
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

        Invoke-OpenAPIAPIClient -Method 'POST' `
                                -Uri '/store/order' `
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

