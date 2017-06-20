function Invoke-StoreApiDeleteOrder {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${orderId}
    )

    Process {
        'Calling method: StoreApi-DeleteOrder' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.DeleteOrder(
            ${orderId}
        )
    }
}

function Invoke-StoreApiGetInventory {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: StoreApi-GetInventory' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.GetInventory(
        )
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
        'Calling method: StoreApi-GetOrderById' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.GetOrderById(
            ${orderId}
        )
    }
}

function Invoke-StoreApiPlaceOrder {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [IO.Swagger.Model.Order]
        ${body}
    )

    Process {
        'Calling method: StoreApi-PlaceOrder' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:StoreApi.PlaceOrder(
            ${body}
        )
    }
}

