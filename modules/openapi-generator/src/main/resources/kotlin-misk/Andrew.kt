package com.squareup.cash.gaia.fdx.action

import com.squareup.cash.gaia.AdminApiAccess
import com.squareup.protos.cash.fdx.model.v1.AccountContact
import com.squareup.protos.cash.fdx.model.v1.AccountPaymentNetworkList
import com.squareup.protos.cash.fdx.model.v1.AccountWithDetails
import com.squareup.protos.cash.fdx.model.v1.Accounts
import com.squareup.protos.cash.fdx.model.v1.AssetTransferNetworkList
import com.squareup.protos.cash.fdx.model.v1.ResultType
import com.squareup.protos.cash.fdx.model.v1.Statements
import com.squareup.protos.cash.fdx.model.v1.Transactions
import io.swagger.v3.oas.annotations.Parameter
import io.swagger.v3.oas.annotations.media.Schema
import javax.inject.Inject
import javax.inject.Singleton
import javax.validation.Valid
import javax.validation.constraints.Size
import misk.web.Description
import misk.web.Get
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestContentType
import misk.web.ResponseContentType
import misk.web.actions.WebAction
import misk.web.interceptors.LogRequestResponse
import misk.web.mediatype.MediaTypes

/**
 * Spring commented out.
@RestController
@Validated
@RequestMapping("\${api.base-path:/fdx/v6}")
 */
@Singleton
class AccountsApiAction @Inject constructor(
) : WebAction {


    /**
     * Spring commented out.
    @Operation(
    summary = "Retrieve account details",
    operationId = "getAccount",
    description = """Retrieve full details about the account identified by `{accountId}` parameter""",
    responses = [
    ApiResponse(
    responseCode = "200",
    description = "This can be one of LoanAccount, DepositAccount, LineOfCreditAccount, InvestmentAccount, InsuranceAccount, AnnuityAccount, CommercialAccount, or DigitalWallet",
    content = [Content(schema = Schema(implementation = AccountWithDetails::class))]
    ),
    ApiResponse(
    responseCode = "404",
    description = "Account with id not found",
    content = [Content(schema = Schema(implementation = Error::class))]
    ),
    ApiResponse(
    responseCode = "500",
    description = "",
    content = [Content(schema = Schema(implementation = Error::class))]
    ),
    ApiResponse(
    responseCode = "501",
    description = "",
    content = [Content(schema = Schema(implementation = Error::class))]
    ),
    ApiResponse(
    responseCode = "503",
    description = "",
    content = [Content(schema = Schema(implementation = Error::class))]
    )]
    )
    @RequestMapping(
    method = [RequestMethod.GET],
    value = ["/accounts/{accountId}"],
    produces = ["application/json"]
    )
    fun getAccount(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String): ResponseEntity<AccountWithDetails> {
    return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
    }
     */
    @Get("/accounts/{accountId}")
    @Description("Retrieve full details about the account identified by `{accountId}` parameter")
    @AdminApiAccess
    @RequestContentType(MediaTypes.APPLICATION_JSON)
    @ResponseContentType(MediaTypes.APPLICATION_JSON)
    @LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
    @Suppress("unused")
    fun getAccount(@PathParam("accountId") accountId: String): AccountWithDetails {
        TODO()
    }
}

/**
 * Spring doc
@Operation(
summary = "Get asset transfer details for this account",
operationId = "getAccountAssetTransferNetworks",
description = """...""",
responses = [
ApiResponse(responseCode = "200", description = "Information required to facilitate asset transfer from this account", content = [Content(schema = Schema(implementation = AssetTransferNetworkList::class))]) ]
)
@RequestMapping(
method = [RequestMethod.GET],
value = ["/accounts/{accountId}/asset-transfer-networks"],
produces = ["application/json"]
)
fun getAccountAssetTransferNetworks(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String): ResponseEntity<AssetTransferNetworkList> {
return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
}
 */
@Get("/accounts/{accountId}/asset-transfer-networks")
@Description("Get asset transfer details for this account")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(MediaTypes.APPLICATION_JSON)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun getAccountAssetTransferNetworks(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId") accountId: String
): AssetTransferNetworkList {
    TODO()
}

/*
  @Operation(
    summary = "Get an account's contact information",
    operationId = "getAccountContact",
    description = """Get contact information on the account""",
    responses = [
      ApiResponse(responseCode = "200", description = "Details used to verify an account", content = [Content(schema = Schema(implementation = AccountContact::class))]),
      ApiResponse(responseCode = "404", description = "Account with id not found", content = [Content(schema = Schema(implementation = Error::class))]),
      ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
      ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
      ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
  )
  @RequestMapping(
    method = [RequestMethod.GET],
    value = ["/accounts/{accountId}/contact"],
    produces = ["application/json"]
  )
  fun getAccountContact(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String): ResponseEntity<AccountContact> {
    return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
  }
 */
@Get("/accounts/{accountId}/contact")
@Description("Get contact information on the account")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(MediaTypes.APPLICATION_JSON)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun getAccountContact(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId")
    accountId: String
): AccountContact {
    TODO()
}

/*
@Operation(
  summary = "Get payment networks supported by the account",
  operationId = "getAccountPaymentNetworks",
  description = """Get payment networks supported by the account""",
  responses = [
    ApiResponse(responseCode = "200", description = "Information required to execute a payment transaction against this account", content = [Content(schema = Schema(implementation = AccountPaymentNetworkList::class))]),
    ApiResponse(responseCode = "404", description = "Account with id not found", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
)
@RequestMapping(
  method = [RequestMethod.GET],
  value = ["/accounts/{accountId}/payment-networks"],
  produces = ["application/json"]
)
fun getAccountPaymentNetworks(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String,@Parameter(description = "Opaque cursor used by the provider to send the next set of records") @Valid @RequestParam(value = "offset", required = false) offset: kotlin.String?,@Parameter(description = "Number of elements that the consumer wishes to receive. Providers should implement reasonable default/maximum/minimum values based on their internal architecture and update their documentation accordingly") @Valid @RequestParam(value = "limit", required = false) limit: kotlin.Int?): ResponseEntity<AccountPaymentNetworkList> {
  return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
}
 */
@Get("/accounts/{accountId}/payment-networks")
@Description("Get payment networks supported by the account")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(MediaTypes.APPLICATION_JSON)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun getAccountPaymentNetworks(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId")
    accountId: String,

    @Parameter(description = "Opaque cursor used by the provider to send the next set of records", required = false)
    @Valid
    @QueryParam(value = "offset")
    offset: kotlin.String?,

    @Parameter(description = "Number of elements that the consumer wishes to receive. Providers should implement reasonable default/maximum/minimum values based on their internal architecture and update their documentation accordingly", required = false)
    @Valid
    @QueryParam(value = "limit")
    limit: Int?
): AccountPaymentNetworkList {
    TODO()
}

/*
@Operation(
  summary = "Get an account statement",
  operationId = "getAccountStatement",
  description = """Gets an account statement image file. Use [HTTP Accept request-header](https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) to specify desired content types. See ContentTypes definition for typical values""",
  responses = [
    ApiResponse(responseCode = "200", description = "An image of an account statement", content = [Content(schema = Schema(implementation = org.springframework.core.io.Resource::class))]),
    ApiResponse(responseCode = "302", description = "Statement is available at specified location. URL is returned via the `Location` HTTP header"),
    ApiResponse(responseCode = "400", description = "Statement is processing and is not yet available", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "404", description = "When account is present with no statements in it", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "406", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
)
@RequestMapping(
  method = [RequestMethod.GET],
  value = ["/accounts/{accountId}/statements/{statementId}"],
  produces = ["application/pdf", "image/gif", "image/jpeg", "image/tiff", "image/png", "application/json"]
)
fun getAccountStatement(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String,@Parameter(description = "Statement Identifier", required = true) @PathVariable("statementId") statementId: kotlin.String): ResponseEntity<org.springframework.core.io.Resource> {
  return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
}
*/
@Get("/accounts/{accountId}/statements/{statementId}")
@Description("""Gets an account statement image file. Use [HTTP Accept request-header](https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) to specify desired content types. See ContentTypes definition for typical values""")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(
    MediaTypes.IMAGE_GIF,
    MediaTypes.IMAGE_JPEG,
    MediaTypes.IMAGE_PNG,
    MediaTypes.APPLICATION_JSON
)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun getAccountStatement(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId")
    accountId: String,

    @Parameter(description = "Statement Identifier", required = true)
    @QueryParam("statementId")
    statementId: String
): ByteArray { // TODO how do we return a file etc.
    TODO()
}

/*
@Operation(
  summary = "Get account transaction image",
  operationId = "getAccountTransactionImages",
  description = """Get account transaction image""",
  responses = [
    ApiResponse(responseCode = "200", description = "An image of transaction (such as a scanned check)", content = [Content(schema = Schema(implementation = org.springframework.core.io.Resource::class))]),
    ApiResponse(responseCode = "404", description = "Account or image with id not found", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "406", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
)
@RequestMapping(
  method = [RequestMethod.GET],
  value = ["/accounts/{accountId}/transaction-images/{imageId}"],
  produces = ["application/pdf", "image/gif", "image/jpeg", "image/tiff", "image/png", "application/json"]
)
fun getAccountTransactionImages(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String,@Parameter(description = "Image Identifier", required = true) @PathVariable("imageId") imageId: kotlin.String): ResponseEntity<org.springframework.core.io.Resource> {
  return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
}

 */
@Get("/accounts/{accountId}/transaction-images/{imageId}")
@Description("Get account transaction image")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(
    MediaTypes.IMAGE_GIF,
    MediaTypes.IMAGE_JPEG,
    MediaTypes.IMAGE_PNG,
    MediaTypes.APPLICATION_JSON
)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun getAccountTransactionImages(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId")
    accountId: kotlin.String,

    @Parameter(description = "Image Identifier", required = true)
    @PathParam("imageId")
    imageId: String
): ByteArray {
    TODO()
}

/*
@Operation(
  summary = "Search for statements",
  operationId = "searchForAccountStatements",
  description = """Get account statements. Example: GET /accounts/{accountId}/statements?startTime=value1&endTime=value2""",
  responses = [
    ApiResponse(responseCode = "200", description = "Paginated list of available statements", content = [Content(schema = Schema(implementation = Statements::class))]),
    ApiResponse(responseCode = "400", description = "Start or end date value is not in the ISO 8601 format", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "404", description = "Account with id not found", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
)
@RequestMapping(
  method = [RequestMethod.GET],
  value = ["/accounts/{accountId}/statements"],
  produces = ["application/json"]
)
fun searchForAccountStatements(@Parameter(description = "Account Identifier", required = true) @PathVariable("accountId") accountId: kotlin.String,@Size(max=10) @Parameter(description = "Start time for use in retrieval of elements (ISO 8601)") @Valid @RequestParam(value = "startTime", required = false) @org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE) startTime: java.time.LocalDate?,@Size(max=10) @Parameter(description = "End time for use in retrieval of elements (ISO 8601)") @Valid @RequestParam(value = "endTime", required = false) @org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE) endTime: java.time.LocalDate?,@Parameter(description = "Opaque cursor used by the provider to send the next set of records") @Valid @RequestParam(value = "offset", required = false) offset: kotlin.String?,@Parameter(description = "Number of elements that the consumer wishes to receive. Providers should implement reasonable default/maximum/minimum values based on their internal architecture and update their documentation accordingly") @Valid @RequestParam(value = "limit", required = false) limit: kotlin.Int?): ResponseEntity<Statements> {
  return ResponseEntity(HttpStatus.NOT_IMPLEMENTED)
}
*/
@Get("/accounts/{accountId}/statements")
@Description("""Get account statements. Example: GET /accounts/{accountId}/statements?startTime=value1&endTime=value2""")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(MediaTypes.APPLICATION_JSON)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun searchForAccountStatements(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId")
    accountId: String,

    @Size(max=10)
    @Parameter(description = "Start time for use in retrieval of elements (ISO 8601)", required = false)
    @Valid
    @QueryParam(value = "startTime")
    //@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)
    startTime: java.time.LocalDate?,

    @Size(max=10)
    @Parameter(description = "End time for use in retrieval of elements (ISO 8601)", required = false)
    @Valid
    @QueryParam(value = "endTime")
    //@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)
    endTime: java.time.LocalDate?,

    @Parameter(description = "Opaque cursor used by the provider to send the next set of records", required = false)
    @Valid
    @QueryParam(value = "offset")
    offset: String?,

    @Parameter(description = "Number of elements that the consumer wishes to receive. Providers should implement reasonable default/maximum/minimum values based on their internal architecture and update their documentation accordingly", required = false)
    @Valid
    @QueryParam(value = "limit")
    limit: Int?): Statements {
    TODO()
}

/*
@Operation(
  summary = "Search for account transactions",
  operationId = "searchForAccountTransactions",
  description = """Search for account transactions. Example: /accounts/{accountId}/transactions?startTime=value1&endTime=value2""",
  responses = [
    ApiResponse(responseCode = "200", description = "Paginated collection of transactions, which can be one of DepositTransaction, LoanTransaction, LineOfCreditTransaction, InvestmentTransaction, InsuranceTransaction, CommercialTransaction, or DigitalWalletTransaction", content = [Content(schema = Schema(implementation = Transactions::class))]),
    ApiResponse(responseCode = "400", description = "Start or end date value is not in the ISO 8601 format", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "404", description = "Account with id not found", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
)
@RequestMapping(
  method = [RequestMethod.GET],
  value = ["/accounts/{accountId}/transactions"],
  produces = ["application/json"]
)
 */
@Get("/accounts/{accountId}/transactions")
@Description("""Search for account transactions. Example: /accounts/{accountId}/transactions?startTime=value1&endTime=value2""")
@AdminApiAccess
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(MediaTypes.APPLICATION_JSON)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun searchForAccountTransactions(
    @Parameter(description = "Account Identifier", required = true)
    @PathParam("accountId")
    accountId:String,

    @Size(max=10)
    @Parameter(description = "Start time for use in retrieval of elements (ISO 8601)", required = false)
    @Valid
    @QueryParam(value = "startTime")
    //@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)
    startTime: java.time.LocalDate?,

    @Size(max=10)
    @Parameter(description = "End time for use in retrieval of elements (ISO 8601)", required = false)
    @Valid
    @QueryParam(value = "endTime")
    //@org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE)
    endTime: java.time.LocalDate?,

    @Parameter(description = "Opaque cursor used by the provider to send the next set of records", required = false)
    @Valid
    @QueryParam(value = "offset")
    offset: String?,

    @Parameter(description = "Number of elements that the consumer wishes to receive. Providers should implement reasonable default/maximum/minimum values based on their internal architecture and update their documentation accordingly", required = false)
    @Valid
    @QueryParam(value = "limit")
    limit: Int?
): Transactions {
    TODO()
}

/*
@Operation(
  summary = "Search for accounts",
  operationId = "searchForAccounts",
  description = """Return information for all of the customer's consented accounts or just those accounts identified in the `accountIds` request parameter. Use `ResultTypeQuery` parameter value of `lightweight` to retrieve minimal descriptive information and the `accountId` for each account. The `accountId` can then be used in the `getAccount` operation's path `/accounts/{accountId}` to retrieve full details about each account""",
  responses = [
    ApiResponse(responseCode = "200", description = "Array of accounts (DepositAccount, LoanAccount, LineOfCreditAccount, InvestmentAccount, InsuranceAccount, AnnuityAccount, CommercialAccount, or DigitalWallet)", content = [Content(schema = Schema(implementation = Accounts::class))]),
    ApiResponse(responseCode = "400", description = "Start or end date value is not in the ISO 8601 format", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "404", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "500", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "501", description = "", content = [Content(schema = Schema(implementation = Error::class))]),
    ApiResponse(responseCode = "503", description = "", content = [Content(schema = Schema(implementation = Error::class))]) ]
)
@RequestMapping(
  method = [RequestMethod.GET],
  value = ["/accounts"],
  produces = ["application/json"]
)
*/
@AdminApiAccess
@Description("""Return information for all of the customer's consented accounts or just those accounts identified in the `accountIds` request parameter. Use `ResultTypeQuery` parameter value of `lightweight` to retrieve minimal descriptive information and the `accountId` for each account. The `accountId` can then be used in the `getAccount` operation's path `/accounts/{accountId}` to retrieve full details about each account""${'"'},""")
@Get("/accounts")
@RequestContentType(MediaTypes.APPLICATION_JSON)
@ResponseContentType(MediaTypes.APPLICATION_JSON)
@LogRequestResponse(bodySampling = 1.0, errorBodySampling = 1.0)
@Suppress("unused")
fun searchForAccounts(
    @Parameter(description = "Comma separated list of account ids", required = false)
    @Valid
    @QueryParam(value = "accountIds")
    accountIds: List<String>?,

    @Parameter(description = "Start time for use in retrieval of transactions", required = false)
    @Valid
    @QueryParam(value = "startTime")
    startTime: List<java.time.LocalDate>?,

    @Parameter(description = "End time for use in retrieval of transactions", required = false)
    @Valid
    @QueryParam(value = "endTime")
    endTime: List<java.time.LocalDate>?,

    @Parameter(description = "Flag to indicate if you want a lightweight array of metadata (AccountDescriptor or Tax or Operations) or full item details (Account or a Tax subclass or Availability details). If set to 'lightweight', should only return the fields associated with the metadata entity. This field is not required, defaults to lightweight",
        schema = Schema(
            allowableValues = ["details", "lightweight"],
            defaultValue = "lightweight"
        )
    )
    @Valid
    @QueryParam(value = "resultType")
    resultType: ResultType,

    @Parameter(description = "Opaque cursor used by the provider to send the next set of records", required = false)
    @Valid
    @QueryParam(value = "offset")
    offset: String?,

    @Parameter(description = "Number of elements that the consumer wishes to receive. Providers should implement reasonable default/maximum/minimum values based on their internal architecture and update their documentation accordingly", required = false)
    @Valid
    @QueryParam(value = "limit")
    limit: Int?
): Accounts {
    TODO()
}


