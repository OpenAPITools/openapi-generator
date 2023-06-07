# Adyen Checkout API - Jetbrains API Client

## OpenAPI File description

Adyen Checkout API provides a simple and flexible way to initiate and authorise online payments. You can use the same integration for payments made with cards (including 3D Secure), mobile wallets, and local payment methods (for example, iDEAL and Sofort).  This API reference provides information on available endpoints and how to interact with them. To learn more about the API, visit [online payments documentation](https://docs.adyen.com/online-payments).  ## Authentication Each request to Checkout API must be signed with an API key. For this, [get your API key](https://docs.adyen.com/development-resources/api-credentials#generate-api-key) from your Customer Area, and set this key to the &#x60;X-API-Key&#x60; header value, for example:  &#x60;&#x60;&#x60; curl -H \&quot;Content-Type: application/json\&quot; \\ -H \&quot;X-API-Key: YOUR_API_KEY\&quot; \\ ... &#x60;&#x60;&#x60; ## Versioning Checkout API supports [versioning](https://docs.adyen.com/development-resources/versioning) using a version suffix in the endpoint URL. This suffix has the following format: \&quot;vXX\&quot;, where XX is the version number.  For example: &#x60;&#x60;&#x60; https://checkout-test.adyen.com/v70/payments &#x60;&#x60;&#x60;  ## Going live  To access the live endpoints, you need an API key from your live Customer Area.  The live endpoint URLs contain a prefix which is unique to your company account, for example: &#x60;&#x60;&#x60; https://{PREFIX}-checkout-live.adyenpayments.com/checkout/v70/payments &#x60;&#x60;&#x60;  Get your &#x60;{PREFIX}&#x60; from your live Customer Area under **Developers** &gt; **API URLs** &gt; **Prefix**.  When preparing to do live transactions with Checkout API, follow the [go-live checklist](https://docs.adyen.com/online-payments/go-live-checklist) to make sure you&#39;ve got all the required configuration in place.  ## Release notes Have a look at the [release notes](https://docs.adyen.com/online-payments/release-notes?integration_type&#x3D;api&amp;version&#x3D;70) to find out what changed in this version!

* API basepath : [https://checkout-test.adyen.com/v70](https://checkout-test.adyen.com/v70)
* Version : 70

## Documentation for API Endpoints

All URIs are relative to *https://checkout-test.adyen.com/v70*, but will link to the `.http` file that contains the endpoint definition

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*ClassicCheckoutSDKApi* | [**postPaymentSession**](Apis/ClassicCheckoutSDKApi.http#postpaymentsession) | **POST** /paymentSession | Create a payment session
*ClassicCheckoutSDKApi* | [**postPaymentsResult**](Apis/ClassicCheckoutSDKApi.http#postpaymentsresult) | **POST** /payments/result | Verify a payment result
*ModificationsApi* | [**postCancels**](Apis/ModificationsApi.http#postcancels) | **POST** /cancels | Cancel an authorised payment
*ModificationsApi* | [**postPaymentsPaymentPspReferenceAmountUpdates**](Apis/ModificationsApi.http#postpaymentspaymentpspreferenceamountupdates) | **POST** /payments/{paymentPspReference}/amountUpdates | Update an authorised amount
*ModificationsApi* | [**postPaymentsPaymentPspReferenceCancels**](Apis/ModificationsApi.http#postpaymentspaymentpspreferencecancels) | **POST** /payments/{paymentPspReference}/cancels | Cancel an authorised payment
*ModificationsApi* | [**postPaymentsPaymentPspReferenceCaptures**](Apis/ModificationsApi.http#postpaymentspaymentpspreferencecaptures) | **POST** /payments/{paymentPspReference}/captures | Capture an authorised payment
*ModificationsApi* | [**postPaymentsPaymentPspReferenceRefunds**](Apis/ModificationsApi.http#postpaymentspaymentpspreferencerefunds) | **POST** /payments/{paymentPspReference}/refunds | Refund a captured payment
*ModificationsApi* | [**postPaymentsPaymentPspReferenceReversals**](Apis/ModificationsApi.http#postpaymentspaymentpspreferencereversals) | **POST** /payments/{paymentPspReference}/reversals | Refund or cancel a payment
*OrdersApi* | [**postOrders**](Apis/OrdersApi.http#postorders) | **POST** /orders | Create an order
*OrdersApi* | [**postOrdersCancel**](Apis/OrdersApi.http#postorderscancel) | **POST** /orders/cancel | Cancel an order
*OrdersApi* | [**postPaymentMethodsBalance**](Apis/OrdersApi.http#postpaymentmethodsbalance) | **POST** /paymentMethods/balance | Get the balance of a gift card
*PaymentLinksApi* | [**getPaymentLinksLinkId**](Apis/PaymentLinksApi.http#getpaymentlinkslinkid) | **GET** /paymentLinks/{linkId} | Get a payment link
*PaymentLinksApi* | [**patchPaymentLinksLinkId**](Apis/PaymentLinksApi.http#patchpaymentlinkslinkid) | **PATCH** /paymentLinks/{linkId} | Update the status of a payment link
*PaymentLinksApi* | [**postPaymentLinks**](Apis/PaymentLinksApi.http#postpaymentlinks) | **POST** /paymentLinks | Create a payment link
*PaymentsApi* | [**postCardDetails**](Apis/PaymentsApi.http#postcarddetails) | **POST** /cardDetails | Get the list of brands on the card
*PaymentsApi* | [**postDonations**](Apis/PaymentsApi.http#postdonations) | **POST** /donations | Start a transaction for donations
*PaymentsApi* | [**postPaymentMethods**](Apis/PaymentsApi.http#postpaymentmethods) | **POST** /paymentMethods | Get a list of available payment methods
*PaymentsApi* | [**postPayments**](Apis/PaymentsApi.http#postpayments) | **POST** /payments | Start a transaction
*PaymentsApi* | [**postPaymentsDetails**](Apis/PaymentsApi.http#postpaymentsdetails) | **POST** /payments/details | Submit details for a payment
*PaymentsApi* | [**postSessions**](Apis/PaymentsApi.http#postsessions) | **POST** /sessions | Create a payment session
*RecurringApi* | [**deleteStoredPaymentMethodsRecurringId**](Apis/RecurringApi.http#deletestoredpaymentmethodsrecurringid) | **DELETE** /storedPaymentMethods/{recurringId} | Delete a token for stored payment details
*RecurringApi* | [**getStoredPaymentMethods**](Apis/RecurringApi.http#getstoredpaymentmethods) | **GET** /storedPaymentMethods | Get tokens for stored payment details
*UtilityApi* | [**postApplePaySessions**](Apis/UtilityApi.http#postapplepaysessions) | **POST** /applePay/sessions | Get an Apple Pay session
*UtilityApi* | [**postOriginKeys**](Apis/UtilityApi.http#postoriginkeys) | **POST** /originKeys | Create originKey values for domains



_This client was generated by the jetbrains-http-client of OpenAPI Generator_