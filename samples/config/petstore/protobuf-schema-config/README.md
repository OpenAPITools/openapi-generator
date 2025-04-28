# Documentation for CDR Banking API

    <a id="documentation-for-api-endpoints"></a>
    ## Documentation for API Endpoints

    All URIs are relative to *https://mtls.dh.example.com/cds-au/v1*

    Class | Method | HTTP request | Description
    ------------ | ------------- | ------------- | -------------
    *BankingAccountBalancesApi* | [**getBankingBalance**](Apis/docs/BankingAccountBalancesApi.md#getbankingbalance) | **Get** /banking/accounts/{accountId}/balance | Get Account Balance
    *BankingAccountBalancesApi* | [**listBankingBalancesBulk**](Apis/docs/BankingAccountBalancesApi.md#listbankingbalancesbulk) | **Get** /banking/accounts/balances | Get Bulk Balances
    *BankingAccountBalancesApi* | [**listBankingBalancesSpecificAccounts**](Apis/docs/BankingAccountBalancesApi.md#listbankingbalancesspecificaccounts) | **Post** /banking/accounts/balances | Get Balances For Specific Accounts
    *BankingAccountDirectDebitsApi* | [**listDirectDebits**](Apis/docs/BankingAccountDirectDebitsApi.md#listdirectdebits) | **Get** /banking/accounts/{accountId}/direct-debits | Get Direct Debits For Account
    *BankingAccountDirectDebitsApi* | [**listDirectDebitsBulk**](Apis/docs/BankingAccountDirectDebitsApi.md#listdirectdebitsbulk) | **Get** /banking/accounts/direct-debits | Get Bulk Direct Debits
    *BankingAccountDirectDebitsApi* | [**listDirectDebitsSpecificAccounts**](Apis/docs/BankingAccountDirectDebitsApi.md#listdirectdebitsspecificaccounts) | **Post** /banking/accounts/direct-debits | Get Direct Debits For Specific Accounts
    *BankingAccountScheduledPaymentsApi* | [**listScheduledPayments**](Apis/docs/BankingAccountScheduledPaymentsApi.md#listscheduledpayments) | **Get** /banking/accounts/{accountId}/payments/scheduled | Get Scheduled Payments for Account
    *BankingAccountScheduledPaymentsApi* | [**listScheduledPaymentsBulk**](Apis/docs/BankingAccountScheduledPaymentsApi.md#listscheduledpaymentsbulk) | **Get** /banking/payments/scheduled | Get Scheduled Payments Bulk
    *BankingAccountScheduledPaymentsApi* | [**listScheduledPaymentsSpecificAccounts**](Apis/docs/BankingAccountScheduledPaymentsApi.md#listscheduledpaymentsspecificaccounts) | **Post** /banking/payments/scheduled | Get Scheduled Payments For Specific Accounts
    *BankingAccountTransactionsApi* | [**getBankingTransactionDetail**](Apis/docs/BankingAccountTransactionsApi.md#getbankingtransactiondetail) | **Get** /banking/accounts/{accountId}/transactions/{transactionId} | Get Transaction Detail
    *BankingAccountTransactionsApi* | [**listBankingTransactions**](Apis/docs/BankingAccountTransactionsApi.md#listbankingtransactions) | **Get** /banking/accounts/{accountId}/transactions | Get Transactions For Account
    *BankingAccountsApi* | [**getBankingAccountDetail**](Apis/docs/BankingAccountsApi.md#getbankingaccountdetail) | **Get** /banking/accounts/{accountId} | Get Account Detail
    *BankingAccountsApi* | [**listBankingAccounts**](Apis/docs/BankingAccountsApi.md#listbankingaccounts) | **Get** /banking/accounts | Get Accounts
    *BankingPayeesApi* | [**getBankingPayeeDetail**](Apis/docs/BankingPayeesApi.md#getbankingpayeedetail) | **Get** /banking/payees/{payeeId} | Get Payee Detail
    *BankingPayeesApi* | [**listBankingPayees**](Apis/docs/BankingPayeesApi.md#listbankingpayees) | **Get** /banking/payees | Get Payees
    *BankingProductsApi* | [**getBankingProductDetail**](Apis/docs/BankingProductsApi.md#getbankingproductdetail) | **Get** /banking/products/{productId} | Get Product Detail
    *BankingProductsApi* | [**listBankingProducts**](Apis/docs/BankingProductsApi.md#listbankingproducts) | **Get** /banking/products | Get Products
    

    <a id="documentation-for-models"></a>
    ## Documentation for Models

         - [petstore.models.BankingAccountDetailV5](Models/docs/BankingAccountDetailV5.md)
         - [petstore.models.BankingAccountDetailV5AllOfFeaturesInner](Models/docs/BankingAccountDetailV5AllOfFeaturesInner.md)
         - [petstore.models.BankingAccountInstalments](Models/docs/BankingAccountInstalments.md)
         - [petstore.models.BankingAccountV3](Models/docs/BankingAccountV3.md)
         - [petstore.models.BankingAuthorisedEntity](Models/docs/BankingAuthorisedEntity.md)
         - [petstore.models.BankingBalance](Models/docs/BankingBalance.md)
         - [petstore.models.BankingBalancePurse](Models/docs/BankingBalancePurse.md)
         - [petstore.models.BankingBillerPayee](Models/docs/BankingBillerPayee.md)
         - [petstore.models.BankingCreditCardAccount](Models/docs/BankingCreditCardAccount.md)
         - [petstore.models.BankingDigitalWalletPayee](Models/docs/BankingDigitalWalletPayee.md)
         - [petstore.models.BankingDirectDebit](Models/docs/BankingDirectDebit.md)
         - [petstore.models.BankingDomesticPayee](Models/docs/BankingDomesticPayee.md)
         - [petstore.models.BankingDomesticPayeeAccount](Models/docs/BankingDomesticPayeeAccount.md)
         - [petstore.models.BankingDomesticPayeeCard](Models/docs/BankingDomesticPayeeCard.md)
         - [petstore.models.BankingDomesticPayeePayId](Models/docs/BankingDomesticPayeePayId.md)
         - [petstore.models.BankingFeeAmount](Models/docs/BankingFeeAmount.md)
         - [petstore.models.BankingFeeDiscountAmount](Models/docs/BankingFeeDiscountAmount.md)
         - [petstore.models.BankingFeeDiscountRange](Models/docs/BankingFeeDiscountRange.md)
         - [petstore.models.BankingFeeDiscountRate](Models/docs/BankingFeeDiscountRate.md)
         - [petstore.models.BankingFeeRange](Models/docs/BankingFeeRange.md)
         - [petstore.models.BankingFeeRate](Models/docs/BankingFeeRate.md)
         - [petstore.models.BankingInstalmentPlanSchedule](Models/docs/BankingInstalmentPlanSchedule.md)
         - [petstore.models.BankingInstalmentPlans](Models/docs/BankingInstalmentPlans.md)
         - [petstore.models.BankingInternationalPayee](Models/docs/BankingInternationalPayee.md)
         - [petstore.models.BankingInternationalPayeeBankDetails](Models/docs/BankingInternationalPayeeBankDetails.md)
         - [petstore.models.BankingInternationalPayeeBankDetailsBankAddress](Models/docs/BankingInternationalPayeeBankDetailsBankAddress.md)
         - [petstore.models.BankingInternationalPayeeBeneficiaryDetails](Models/docs/BankingInternationalPayeeBeneficiaryDetails.md)
         - [petstore.models.BankingLoanAccountV3](Models/docs/BankingLoanAccountV3.md)
         - [petstore.models.BankingPayeeDetailV2](Models/docs/BankingPayeeDetailV2.md)
         - [petstore.models.BankingPayeeV2](Models/docs/BankingPayeeV2.md)
         - [petstore.models.BankingProductAdditionalInformationV2](Models/docs/BankingProductAdditionalInformationV2.md)
         - [petstore.models.BankingProductAdditionalInformationV2AdditionalInformationUris](Models/docs/BankingProductAdditionalInformationV2AdditionalInformationUris.md)
         - [petstore.models.BankingProductBundle](Models/docs/BankingProductBundle.md)
         - [petstore.models.BankingProductCardArt](Models/docs/BankingProductCardArt.md)
         - [petstore.models.BankingProductCategoryV2](Models/docs/BankingProductCategoryV2.md)
         - [petstore.models.BankingProductConstraintV3](Models/docs/BankingProductConstraintV3.md)
         - [petstore.models.BankingProductDepositRateV2](Models/docs/BankingProductDepositRateV2.md)
         - [petstore.models.BankingProductDetailV7](Models/docs/BankingProductDetailV7.md)
         - [petstore.models.BankingProductDiscountEligibility](Models/docs/BankingProductDiscountEligibility.md)
         - [petstore.models.BankingProductDiscountV2](Models/docs/BankingProductDiscountV2.md)
         - [petstore.models.BankingProductEligibilityV2](Models/docs/BankingProductEligibilityV2.md)
         - [petstore.models.BankingProductFeatureV4](Models/docs/BankingProductFeatureV4.md)
         - [petstore.models.BankingProductFeeV2](Models/docs/BankingProductFeeV2.md)
         - [petstore.models.BankingProductInstalments](Models/docs/BankingProductInstalments.md)
         - [petstore.models.BankingProductLendingRateV3](Models/docs/BankingProductLendingRateV3.md)
         - [petstore.models.BankingProductRateConditionV2](Models/docs/BankingProductRateConditionV2.md)
         - [petstore.models.BankingProductRateTierV4](Models/docs/BankingProductRateTierV4.md)
         - [petstore.models.BankingProductV6](Models/docs/BankingProductV6.md)
         - [petstore.models.BankingScheduledPaymentFrom](Models/docs/BankingScheduledPaymentFrom.md)
         - [petstore.models.BankingScheduledPaymentInterval](Models/docs/BankingScheduledPaymentInterval.md)
         - [petstore.models.BankingScheduledPaymentRecurrence](Models/docs/BankingScheduledPaymentRecurrence.md)
         - [petstore.models.BankingScheduledPaymentRecurrenceEventBased](Models/docs/BankingScheduledPaymentRecurrenceEventBased.md)
         - [petstore.models.BankingScheduledPaymentRecurrenceIntervalSchedule](Models/docs/BankingScheduledPaymentRecurrenceIntervalSchedule.md)
         - [petstore.models.BankingScheduledPaymentRecurrenceLastWeekday](Models/docs/BankingScheduledPaymentRecurrenceLastWeekday.md)
         - [petstore.models.BankingScheduledPaymentRecurrenceOnceOff](Models/docs/BankingScheduledPaymentRecurrenceOnceOff.md)
         - [petstore.models.BankingScheduledPaymentSetV2](Models/docs/BankingScheduledPaymentSetV2.md)
         - [petstore.models.BankingScheduledPaymentToV2](Models/docs/BankingScheduledPaymentToV2.md)
         - [petstore.models.BankingScheduledPaymentV2](Models/docs/BankingScheduledPaymentV2.md)
         - [petstore.models.BankingTermDepositAccount](Models/docs/BankingTermDepositAccount.md)
         - [petstore.models.BankingTransaction](Models/docs/BankingTransaction.md)
         - [petstore.models.BankingTransactionDetailV2](Models/docs/BankingTransactionDetailV2.md)
         - [petstore.models.BankingTransactionDetailV2AllOfExtendedData](Models/docs/BankingTransactionDetailV2AllOfExtendedData.md)
         - [petstore.models.BankingTransactionDetailV2AllOfExtendedDataNppPayload](Models/docs/BankingTransactionDetailV2AllOfExtendedDataNppPayload.md)
         - [petstore.models.CommonPAFAddress](Models/docs/CommonPAFAddress.md)
         - [petstore.models.CommonPhysicalAddress](Models/docs/CommonPhysicalAddress.md)
         - [petstore.models.CommonSimpleAddress](Models/docs/CommonSimpleAddress.md)
         - [petstore.models.ErrorV2](Models/docs/ErrorV2.md)
         - [petstore.models.ErrorV2Meta](Models/docs/ErrorV2Meta.md)
         - [petstore.models.Links](Models/docs/Links.md)
         - [petstore.models.LinksPaginated](Models/docs/LinksPaginated.md)
         - [petstore.models.MetaPaginated](Models/docs/MetaPaginated.md)
         - [petstore.models.MetaPaginatedTransaction](Models/docs/MetaPaginatedTransaction.md)
         - [petstore.models.NppPaymentService](Models/docs/NppPaymentService.md)
         - [petstore.models.RequestAccountIdListV1](Models/docs/RequestAccountIdListV1.md)
         - [petstore.models.RequestAccountIdListV1Data](Models/docs/RequestAccountIdListV1Data.md)
         - [petstore.models.ResponseBankingAccountByIdV5](Models/docs/ResponseBankingAccountByIdV5.md)
         - [petstore.models.ResponseBankingAccountListV3](Models/docs/ResponseBankingAccountListV3.md)
         - [petstore.models.ResponseBankingAccountListV3Data](Models/docs/ResponseBankingAccountListV3Data.md)
         - [petstore.models.ResponseBankingAccountsBalanceById](Models/docs/ResponseBankingAccountsBalanceById.md)
         - [petstore.models.ResponseBankingAccountsBalanceList](Models/docs/ResponseBankingAccountsBalanceList.md)
         - [petstore.models.ResponseBankingAccountsBalanceListData](Models/docs/ResponseBankingAccountsBalanceListData.md)
         - [petstore.models.ResponseBankingDirectDebitAuthorisationList](Models/docs/ResponseBankingDirectDebitAuthorisationList.md)
         - [petstore.models.ResponseBankingDirectDebitAuthorisationListData](Models/docs/ResponseBankingDirectDebitAuthorisationListData.md)
         - [petstore.models.ResponseBankingPayeeByIdV2](Models/docs/ResponseBankingPayeeByIdV2.md)
         - [petstore.models.ResponseBankingPayeeListV2](Models/docs/ResponseBankingPayeeListV2.md)
         - [petstore.models.ResponseBankingPayeeListV2Data](Models/docs/ResponseBankingPayeeListV2Data.md)
         - [petstore.models.ResponseBankingProductByIdV7](Models/docs/ResponseBankingProductByIdV7.md)
         - [petstore.models.ResponseBankingProductListV4](Models/docs/ResponseBankingProductListV4.md)
         - [petstore.models.ResponseBankingProductListV4Data](Models/docs/ResponseBankingProductListV4Data.md)
         - [petstore.models.ResponseBankingScheduledPaymentsListV2](Models/docs/ResponseBankingScheduledPaymentsListV2.md)
         - [petstore.models.ResponseBankingScheduledPaymentsListV2Data](Models/docs/ResponseBankingScheduledPaymentsListV2Data.md)
         - [petstore.models.ResponseBankingTransactionByIdV2](Models/docs/ResponseBankingTransactionByIdV2.md)
         - [petstore.models.ResponseBankingTransactionList](Models/docs/ResponseBankingTransactionList.md)
         - [petstore.models.ResponseBankingTransactionListData](Models/docs/ResponseBankingTransactionListData.md)
         - [petstore.models.ResponseErrorListV2](Models/docs/ResponseErrorListV2.md)
        

<a id="documentation-for-authorization"></a>
## Documentation for Authorization

Endpoints do not require authorization.

