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

        Invoke-OpenAPIAPIClient -Method 'DELETE' -ResourceUrl '/store/order/{orderId}'

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

        Invoke-OpenAPIAPIClient -Method 'GET' -ResourceUrl '/store/inventory'

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

        Invoke-OpenAPIAPIClient -Method 'GET' -ResourceUrl '/store/order/{orderId}'

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

        Invoke-OpenAPIAPIClient -Method 'POST' -ResourceUrl '/store/order'

        #$Script:StoreApi.PlaceOrder(
        #
        #    
        #    ${body}
        #    
        #
        #)
    }
}

