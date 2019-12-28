#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap, BTreeSet};
use std::marker::PhantomData;
use futures::{Future, future, Stream, stream};
use hyper;
use hyper::{Request, Response, Error, StatusCode, Body, HeaderMap};
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use serde_json;
use std::io;
#[allow(unused_imports)]
use swagger;
use swagger::{ApiError, XSpanIdString, Has, RequestParser};
use swagger::auth::Scopes;
use swagger::context::ContextualPayload;
use url::form_urlencoded;
use multipart::server::Multipart;
use multipart::server::save::SaveResult;
use serde_xml_rs;
use uuid;

use mimetypes;

pub use swagger::auth::Authorization;

use {Api,
     TestSpecialTagsResponse,
     FakeOuterBooleanSerializeResponse,
     FakeOuterCompositeSerializeResponse,
     FakeOuterNumberSerializeResponse,
     FakeOuterStringSerializeResponse,
     TestBodyWithQueryParamsResponse,
     TestClientModelResponse,
     TestEndpointParametersResponse,
     TestEnumParametersResponse,
     TestInlineAdditionalPropertiesResponse,
     TestJsonFormDataResponse,
     TestClassnameResponse,
     AddPetResponse,
     DeletePetResponse,
     FindPetsByStatusResponse,
     FindPetsByTagsResponse,
     GetPetByIdResponse,
     UpdatePetResponse,
     UpdatePetWithFormResponse,
     UploadFileResponse,
     DeleteOrderResponse,
     GetInventoryResponse,
     GetOrderByIdResponse,
     PlaceOrderResponse,
     CreateUserResponse,
     CreateUsersWithArrayInputResponse,
     CreateUsersWithListInputResponse,
     DeleteUserResponse,
     GetUserByNameResponse,
     LoginUserResponse,
     LogoutUserResponse,
     UpdateUserResponse
     };

#[allow(unused_imports)]
use models;

pub mod context;

mod paths {
    extern crate regex;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/v2/another-fake/dummy$",
            r"^/v2/fake$",
            r"^/v2/fake/body-with-query-params$",
            r"^/v2/fake/inline-additionalProperties$",
            r"^/v2/fake/jsonFormData$",
            r"^/v2/fake/outer/boolean$",
            r"^/v2/fake/outer/composite$",
            r"^/v2/fake/outer/number$",
            r"^/v2/fake/outer/string$",
            r"^/v2/fake_classname_test$",
            r"^/v2/pet$",
            r"^/v2/pet/findByStatus$",
            r"^/v2/pet/findByTags$",
            r"^/v2/pet/(?P<petId>[^/?#]*)$",
            r"^/v2/pet/(?P<petId>[^/?#]*)/uploadImage$",
            r"^/v2/store/inventory$",
            r"^/v2/store/order$",
            r"^/v2/store/order/(?P<order_id>[^/?#]*)$",
            r"^/v2/user$",
            r"^/v2/user/createWithArray$",
            r"^/v2/user/createWithList$",
            r"^/v2/user/login$",
            r"^/v2/user/logout$",
            r"^/v2/user/(?P<username>[^/?#]*)$"
        ])
        .expect("Unable to create global regex set");
    }
    pub static ID_ANOTHER_FAKE_DUMMY: usize = 0;
    pub static ID_FAKE: usize = 1;
    pub static ID_FAKE_BODY_WITH_QUERY_PARAMS: usize = 2;
    pub static ID_FAKE_INLINE_ADDITIONALPROPERTIES: usize = 3;
    pub static ID_FAKE_JSONFORMDATA: usize = 4;
    pub static ID_FAKE_OUTER_BOOLEAN: usize = 5;
    pub static ID_FAKE_OUTER_COMPOSITE: usize = 6;
    pub static ID_FAKE_OUTER_NUMBER: usize = 7;
    pub static ID_FAKE_OUTER_STRING: usize = 8;
    pub static ID_FAKE_CLASSNAME_TEST: usize = 9;
    pub static ID_PET: usize = 10;
    pub static ID_PET_FINDBYSTATUS: usize = 11;
    pub static ID_PET_FINDBYTAGS: usize = 12;
    pub static ID_PET_PETID: usize = 13;
    lazy_static! {
        pub static ref REGEX_PET_PETID: regex::Regex =
            regex::Regex::new(r"^/v2/pet/(?P<petId>[^/?#]*)$")
                .expect("Unable to create regex for PET_PETID");
    }
    pub static ID_PET_PETID_UPLOADIMAGE: usize = 14;
    lazy_static! {
        pub static ref REGEX_PET_PETID_UPLOADIMAGE: regex::Regex =
            regex::Regex::new(r"^/v2/pet/(?P<petId>[^/?#]*)/uploadImage$")
                .expect("Unable to create regex for PET_PETID_UPLOADIMAGE");
    }
    pub static ID_STORE_INVENTORY: usize = 15;
    pub static ID_STORE_ORDER: usize = 16;
    pub static ID_STORE_ORDER_ORDER_ID: usize = 17;
    lazy_static! {
        pub static ref REGEX_STORE_ORDER_ORDER_ID: regex::Regex =
            regex::Regex::new(r"^/v2/store/order/(?P<order_id>[^/?#]*)$")
                .expect("Unable to create regex for STORE_ORDER_ORDER_ID");
    }
    pub static ID_USER: usize = 18;
    pub static ID_USER_CREATEWITHARRAY: usize = 19;
    pub static ID_USER_CREATEWITHLIST: usize = 20;
    pub static ID_USER_LOGIN: usize = 21;
    pub static ID_USER_LOGOUT: usize = 22;
    pub static ID_USER_USERNAME: usize = 23;
    lazy_static! {
        pub static ref REGEX_USER_USERNAME: regex::Regex =
            regex::Regex::new(r"^/v2/user/(?P<username>[^/?#]*)$")
                .expect("Unable to create regex for USER_USERNAME");
    }
}

pub struct MakeService<T, RC> {
    api_impl: T,
    marker: PhantomData<RC>,
}

impl<T, RC> MakeService<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString> + Has<Option<Authorization>> + 'static
{
    pub fn new(api_impl: T) -> Self {
        MakeService {
            api_impl,
            marker: PhantomData
        }
    }
}

impl<'a, T, SC, RC> hyper::service::MakeService<&'a SC> for MakeService<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString> + Has<Option<Authorization>> + 'static + Send
{
    type ReqBody = ContextualPayload<Body, RC>;
    type ResBody = Body;
    type Error = Error;
    type Service = Service<T, RC>;
    type Future = future::FutureResult<Self::Service, Self::MakeError>;
    type MakeError = Error;

    fn make_service(&mut self, _ctx: &'a SC) -> Self::Future {
        future::FutureResult::from(Ok(Service::new(
            self.api_impl.clone(),
        )))
    }
}

pub struct Service<T, RC> {
    api_impl: T,
    marker: PhantomData<RC>,
}

impl<T, RC> Service<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString> + Has<Option<Authorization>> + 'static {
    pub fn new(api_impl: T) -> Self {
        Service {
            api_impl: api_impl,
            marker: PhantomData
        }
    }
}

impl<T, C> hyper::service::Service for Service<T, C>
where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString> + Has<Option<Authorization>> + 'static + Send
{
    type ReqBody = ContextualPayload<Body, C>;
    type ResBody = Body;
    type Error = Error;
    type Future = Box<dyn Future<Item = Response<Self::ResBody>, Error = Self::Error> + Send>;

    fn call(&mut self, req: Request<Self::ReqBody>) -> Self::Future {
        let api_impl = self.api_impl.clone();
        let (parts, body) = req.into_parts();
        let (method, uri, headers) = (parts.method, parts.uri, parts.headers);
        let path = paths::GLOBAL_REGEX_SET.matches(uri.path());
        let mut context = body.context;
        let body = body.inner;

        // This match statement is duplicated below in `parse_operation_id()`.
        // Please update both places if changing how this code is autogenerated.
        match &method {

            // TestSpecialTags - PATCH /another-fake/dummy
            &hyper::Method::PATCH if path.matched(paths::ID_ANOTHER_FAKE_DUMMY) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::Client> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.test_special_tags(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestSpecialTagsResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::TEST_SPECIAL_TAGS_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for TEST_SPECIAL_TAGS_SUCCESSFUL_OPERATION"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // FakeOuterBooleanSerialize - POST /fake/outer/boolean
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_BOOLEAN) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::OuterBoolean> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                Box::new(
                                    api_impl.fake_outer_boolean_serialize(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                FakeOuterBooleanSerializeResponse::OutputBoolean

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::FAKE_OUTER_BOOLEAN_SERIALIZE_OUTPUT_BOOLEAN)
                                                            .expect("Unable to create Content-Type header for FAKE_OUTER_BOOLEAN_SERIALIZE_OUTPUT_BOOLEAN"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // FakeOuterCompositeSerialize - POST /fake/outer/composite
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_COMPOSITE) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::OuterComposite> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                Box::new(
                                    api_impl.fake_outer_composite_serialize(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                FakeOuterCompositeSerializeResponse::OutputComposite

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::FAKE_OUTER_COMPOSITE_SERIALIZE_OUTPUT_COMPOSITE)
                                                            .expect("Unable to create Content-Type header for FAKE_OUTER_COMPOSITE_SERIALIZE_OUTPUT_COMPOSITE"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // FakeOuterNumberSerialize - POST /fake/outer/number
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_NUMBER) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::OuterNumber> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                Box::new(
                                    api_impl.fake_outer_number_serialize(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                FakeOuterNumberSerializeResponse::OutputNumber

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::FAKE_OUTER_NUMBER_SERIALIZE_OUTPUT_NUMBER)
                                                            .expect("Unable to create Content-Type header for FAKE_OUTER_NUMBER_SERIALIZE_OUTPUT_NUMBER"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // FakeOuterStringSerialize - POST /fake/outer/string
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_STRING) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::OuterString> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                Box::new(
                                    api_impl.fake_outer_string_serialize(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                FakeOuterStringSerializeResponse::OutputString

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::FAKE_OUTER_STRING_SERIALIZE_OUTPUT_STRING)
                                                            .expect("Unable to create Content-Type header for FAKE_OUTER_STRING_SERIALIZE_OUTPUT_STRING"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // TestBodyWithQueryParams - PUT /fake/body-with-query-params
            &hyper::Method::PUT if path.matched(paths::ID_FAKE_BODY_WITH_QUERY_PARAMS) => {

                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_query = query_params.iter().filter(|e| e.0 == "query").map(|e| e.1.to_owned())
                    .nth(0);
                let param_query = match param_query {
                    Some(param_query) => match param_query.parse::<String>() {
                        Ok(param_query) => param_query,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse query parameter query - doesn't match schema: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid query parameter query"))),
                    },
                    None => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from("Missing required query parameter query"))
                                        .expect("Unable to create Bad Request response for missing qeury parameter query"))),
                };
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::User> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.test_body_with_query_params(
                                            param_query,
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestBodyWithQueryParamsResponse::Success


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // TestClientModel - PATCH /fake
            &hyper::Method::PATCH if path.matched(paths::ID_FAKE) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::Client> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.test_client_model(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestClientModelResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::TEST_CLIENT_MODEL_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for TEST_CLIENT_MODEL_SUCCESSFUL_OPERATION"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // TestEndpointParameters - POST /fake
            &hyper::Method::POST if path.matched(paths::ID_FAKE) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                }
                Box::new({
                        {{
                                // Form parameters
                                let param_integer = 
Some(56);


                                let param_int32 = 
Some(56);


                                let param_int64 = 
Some(789);


                                let param_number = 
8.14;


                                let param_float = 
Some(3.4);


                                let param_double = 
1.2;


                                let param_string = 
Some("string_example".to_string());


                                let param_pattern_without_delimiter = 
"pattern_without_delimiter_example".to_string();


                                let param_byte = 
swagger::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE"));


                                let param_binary = 
Some(swagger::ByteArray(Vec::from("BINARY_DATA_HERE")));


                                let param_date = 
None;


                                let param_date_time = 
None;


                                let param_password = 
Some("password_example".to_string());


                                let param_callback = 
Some("callback_example".to_string());



                                Box::new(
                                    api_impl.test_endpoint_parameters(
                                            param_number,
                                            param_double,
                                            param_pattern_without_delimiter,
                                            param_byte,
                                            param_integer,
                                            param_int32,
                                            param_int64,
                                            param_float,
                                            param_string,
                                            param_binary,
                                            param_date,
                                            param_date_time,
                                            param_password,
                                            param_callback,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestEndpointParametersResponse::InvalidUsernameSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                TestEndpointParametersResponse::UserNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // TestEnumParameters - GET /fake
            &hyper::Method::GET if path.matched(paths::ID_FAKE) => {

                // Header parameters
                let param_enum_header_string_array = headers.get(HeaderName::from_static("enum_header_string_array"));

                let param_enum_header_string_array = param_enum_header_string_array.map(|p| {
                        swagger::IntoHeaderValue::<Vec<String>>::from((*p).clone()).0
                });
                let param_enum_header_string = headers.get(HeaderName::from_static("enum_header_string"));

                let param_enum_header_string = param_enum_header_string.map(|p| {
                        swagger::IntoHeaderValue::<String>::from((*p).clone()).0
                });

                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_enum_query_string_array = query_params.iter().filter(|e| e.0 == "enum_query_string_array").map(|e| e.1.to_owned())
                    .filter_map(|param_enum_query_string_array| param_enum_query_string_array.parse::<String>().ok())
                    .collect::<Vec<_>>();
                let param_enum_query_string_array = if !param_enum_query_string_array.is_empty() {
                    Some(param_enum_query_string_array)
                } else {
                    None
                };
                let param_enum_query_string = query_params.iter().filter(|e| e.0 == "enum_query_string").map(|e| e.1.to_owned())
                    .nth(0);

                let param_enum_query_string = param_enum_query_string.and_then(|param_enum_query_string| param_enum_query_string.parse::<>().ok());
                let param_enum_query_integer = query_params.iter().filter(|e| e.0 == "enum_query_integer").map(|e| e.1.to_owned())
                    .nth(0);

                let param_enum_query_integer = param_enum_query_integer.and_then(|param_enum_query_integer| param_enum_query_integer.parse::<>().ok());
                let param_enum_query_double = query_params.iter().filter(|e| e.0 == "enum_query_double").map(|e| e.1.to_owned())
                    .nth(0);

                let param_enum_query_double = param_enum_query_double.and_then(|param_enum_query_double| param_enum_query_double.parse::<>().ok());
                Box::new({
                        {{
                                // Form parameters
                                let param_enum_form_string = 
Some("enum_form_string_example".to_string());



                                Box::new(
                                    api_impl.test_enum_parameters(
                                            param_enum_header_string_array.as_ref(),
                                            param_enum_header_string,
                                            param_enum_query_string_array.as_ref(),
                                            param_enum_query_string,
                                            param_enum_query_integer,
                                            param_enum_query_double,
                                            param_enum_form_string,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestEnumParametersResponse::InvalidRequest


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                TestEnumParametersResponse::NotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // TestInlineAdditionalProperties - POST /fake/inline-additionalProperties
            &hyper::Method::POST if path.matched(paths::ID_FAKE_INLINE_ADDITIONALPROPERTIES) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_param: Option<HashMap<String, String>> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_param) => param_param,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter param - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter param due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_param = match param_param {
                                    Some(param_param) => param_param,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter param"))
                                                        .expect("Unable to create Bad Request response for missing body parameter param"))),
                                };

                                Box::new(
                                    api_impl.test_inline_additional_properties(
                                            param_param,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestInlineAdditionalPropertiesResponse::SuccessfulOperation


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter param: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter param"))),
                        }
                    })
                ) as Self::Future
            },

            // TestJsonFormData - GET /fake/jsonFormData
            &hyper::Method::GET if path.matched(paths::ID_FAKE_JSONFORMDATA) => {
                Box::new({
                        {{
                                // Form parameters
                                let param_param = 
"param_example".to_string();


                                let param_param2 = 
"param2_example".to_string();



                                Box::new(
                                    api_impl.test_json_form_data(
                                            param_param,
                                            param_param2,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestJsonFormDataResponse::SuccessfulOperation


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // TestClassname - PATCH /fake_classname_test
            &hyper::Method::PATCH if path.matched(paths::ID_FAKE_CLASSNAME_TEST) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                }
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::Client> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.test_classname(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                TestClassnameResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::TEST_CLASSNAME_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for TEST_CLASSNAME_SUCCESSFUL_OPERATION"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // AddPet - POST /pet
            &hyper::Method::POST if path.matched(paths::ID_PET) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::Pet> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.add_pet(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                AddPetResponse::InvalidInput


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(405).expect("Unable to turn 405 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // DeletePet - DELETE /pet/{petId}
            &hyper::Method::DELETE if path.matched(paths::ID_PET_PETID) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_PET_PETID
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE PET_PETID in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID.as_str())
                    );
                let param_pet_id = match percent_encoding::percent_decode(path_params["petId"].as_bytes()).decode_utf8() {
                    Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                        Ok(param_pet_id) => param_pet_id,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter petId: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };

                // Header parameters
                let param_api_key = headers.get(HeaderName::from_static("api_key"));

                let param_api_key = param_api_key.map(|p| {
                        swagger::IntoHeaderValue::<String>::from((*p).clone()).0
                });
                Box::new({
                        {{

                                Box::new(
                                    api_impl.delete_pet(
                                            param_pet_id,
                                            param_api_key,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                DeletePetResponse::InvalidPetValue


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // FindPetsByStatus - GET /pet/findByStatus
            &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYSTATUS) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }

                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_status = query_params.iter().filter(|e| e.0 == "status").map(|e| e.1.to_owned())
                    .filter_map(|param_status| param_status.parse::<String>().ok())
                    .collect::<Vec<_>>();
                Box::new({
                        {{

                                Box::new(
                                    api_impl.find_pets_by_status(
                                            param_status.as_ref(),
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                FindPetsByStatusResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::FIND_PETS_BY_STATUS_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for FIND_PETS_BY_STATUS_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                FindPetsByStatusResponse::InvalidStatusValue


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // FindPetsByTags - GET /pet/findByTags
            &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYTAGS) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }

                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_tags = query_params.iter().filter(|e| e.0 == "tags").map(|e| e.1.to_owned())
                    .filter_map(|param_tags| param_tags.parse::<String>().ok())
                    .collect::<Vec<_>>();
                Box::new({
                        {{

                                Box::new(
                                    api_impl.find_pets_by_tags(
                                            param_tags.as_ref(),
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                FindPetsByTagsResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::FIND_PETS_BY_TAGS_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for FIND_PETS_BY_TAGS_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                FindPetsByTagsResponse::InvalidTagValue


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // GetPetById - GET /pet/{petId}
            &hyper::Method::GET if path.matched(paths::ID_PET_PETID) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                }
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_PET_PETID
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE PET_PETID in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID.as_str())
                    );
                let param_pet_id = match percent_encoding::percent_decode(path_params["petId"].as_bytes()).decode_utf8() {
                    Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                        Ok(param_pet_id) => param_pet_id,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter petId: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                Box::new({
                        {{

                                Box::new(
                                    api_impl.get_pet_by_id(
                                            param_pet_id,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                GetPetByIdResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::GET_PET_BY_ID_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for GET_PET_BY_ID_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                GetPetByIdResponse::InvalidIDSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                GetPetByIdResponse::PetNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // UpdatePet - PUT /pet
            &hyper::Method::PUT if path.matched(paths::ID_PET) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::Pet> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.update_pet(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                UpdatePetResponse::InvalidIDSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                UpdatePetResponse::PetNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                                UpdatePetResponse::ValidationException


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(405).expect("Unable to turn 405 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // UpdatePetWithForm - POST /pet/{petId}
            &hyper::Method::POST if path.matched(paths::ID_PET_PETID) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_PET_PETID
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE PET_PETID in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID.as_str())
                    );
                let param_pet_id = match percent_encoding::percent_decode(path_params["petId"].as_bytes()).decode_utf8() {
                    Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                        Ok(param_pet_id) => param_pet_id,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter petId: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                Box::new({
                        {{
                                // Form parameters
                                let param_name = 
Some("name_example".to_string());


                                let param_status = 
Some("status_example".to_string());



                                Box::new(
                                    api_impl.update_pet_with_form(
                                            param_pet_id,
                                            param_name,
                                            param_status,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                UpdatePetWithFormResponse::InvalidInput


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(405).expect("Unable to turn 405 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // UploadFile - POST /pet/{petId}/uploadImage
            &hyper::Method::POST if path.matched(paths::ID_PET_PETID_UPLOADIMAGE) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: BTreeSet<String> = vec![
                            "write:pets".to_string(), // modify pets in your account
                            "read:pets".to_string(), // read your pets
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Box::new(future::ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            ));
                        }
                    }
                }
                let boundary = match swagger::multipart::boundary(&headers) {
                    Some(boundary) => boundary.to_string(),
                    None => return Box::new(future::ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from("Couldn't find valid multipart body".to_string()))
                                .expect("Unable to create Bad Request response for incorrect boundary"))),
                };
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_PET_PETID_UPLOADIMAGE
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE PET_PETID_UPLOADIMAGE in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID_UPLOADIMAGE.as_str())
                    );
                let param_pet_id = match percent_encoding::percent_decode(path_params["petId"].as_bytes()).decode_utf8() {
                    Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                        Ok(param_pet_id) => param_pet_id,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter petId: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                // Form Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                use std::io::Read;

                                // Read Form Parameters from body
                                let mut entries = match Multipart::with_body(&body.to_vec()[..], boundary).save().temp() {
                                    SaveResult::Full(entries) => {
                                        entries
                                    },
                                    _ => {
                                        return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Unable to process all message parts")))
                                                        .expect("Unable to create Bad Request response due to failure to process all message")))
                                    },
                                };
                
                                let field_additional_metadata = entries.fields.remove("additional_metadata");
                                let param_additional_metadata = match field_additional_metadata {
                                    Some(field) => {
                                        let mut reader = field[0].data.readable().expect("Unable to read field for additional_metadata");
                                    Some({
                                        let mut data = String::new();
                                        reader.read_to_string(&mut data).expect("Reading saved String should never fail");
                                        let additional_metadata_model: String = match serde_json::from_str(&data) {
                                            Ok(model) => model,
                                            Err(e) => {
                                                return Box::new(future::ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("additional_metadata data does not match API definition: {}", e)))
                                                    .expect("Unable to create Bad Request response due to failure to process all message")))
                                            }
                                        };
                                        additional_metadata_model
                                    })
                                    },
                                    None => {
                                            None
                                    }
                                };

                                let field_file = entries.fields.remove("file");
                                let param_file = match field_file {
                                    Some(field) => {
                                        let mut reader = field[0].data.readable().expect("Unable to read field for file");
                                    Some({
                                        let mut data = String::new();
                                        reader.read_to_string(&mut data).expect("Reading saved String should never fail");
                                        let file_model: swagger::ByteArray = match serde_json::from_str(&data) {
                                            Ok(model) => model,
                                            Err(e) => {
                                                return Box::new(future::ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("file data does not match API definition: {}", e)))
                                                    .expect("Unable to create Bad Request response due to failure to process all message")))
                                            }
                                        };
                                        file_model
                                    })
                                    },
                                    None => {
                                            None
                                    }
                                };

                                Box::new(
                                    api_impl.upload_file(
                                            param_pet_id,
                                            param_additional_metadata,
                                            param_file,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                UploadFileResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::UPLOAD_FILE_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for UPLOAD_FILE_SUCCESSFUL_OPERATION"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                                as Self::Future
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read multipart body")))
                                                .expect("Unable to create Bad Request response due to unable read multipart body"))),
                        }
                    })
                )
            },

            // DeleteOrder - DELETE /store/order/{order_id}
            &hyper::Method::DELETE if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_STORE_ORDER_ORDER_ID
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE STORE_ORDER_ORDER_ID in set but failed match against \"{}\"", path, paths::REGEX_STORE_ORDER_ORDER_ID.as_str())
                    );
                let param_order_id = match percent_encoding::percent_decode(path_params["order_id"].as_bytes()).decode_utf8() {
                    Ok(param_order_id) => match param_order_id.parse::<String>() {
                        Ok(param_order_id) => param_order_id,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter order_id: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["order_id"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                Box::new({
                        {{

                                Box::new(
                                    api_impl.delete_order(
                                            param_order_id,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                DeleteOrderResponse::InvalidIDSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                DeleteOrderResponse::OrderNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // GetInventory - GET /store/inventory
            &hyper::Method::GET if path.matched(paths::ID_STORE_INVENTORY) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response"))),
                    };

                }
                Box::new({
                        {{

                                Box::new(
                                    api_impl.get_inventory(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                GetInventoryResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::GET_INVENTORY_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for GET_INVENTORY_SUCCESSFUL_OPERATION"));

                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // GetOrderById - GET /store/order/{order_id}
            &hyper::Method::GET if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_STORE_ORDER_ORDER_ID
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE STORE_ORDER_ORDER_ID in set but failed match against \"{}\"", path, paths::REGEX_STORE_ORDER_ORDER_ID.as_str())
                    );
                let param_order_id = match percent_encoding::percent_decode(path_params["order_id"].as_bytes()).decode_utf8() {
                    Ok(param_order_id) => match param_order_id.parse::<i64>() {
                        Ok(param_order_id) => param_order_id,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter order_id: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["order_id"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                Box::new({
                        {{

                                Box::new(
                                    api_impl.get_order_by_id(
                                            param_order_id,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                GetOrderByIdResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::GET_ORDER_BY_ID_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for GET_ORDER_BY_ID_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                GetOrderByIdResponse::InvalidIDSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                GetOrderByIdResponse::OrderNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // PlaceOrder - POST /store/order
            &hyper::Method::POST if path.matched(paths::ID_STORE_ORDER) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::Order> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.place_order(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                PlaceOrderResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::PLACE_ORDER_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for PLACE_ORDER_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                PlaceOrderResponse::InvalidOrder


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // CreateUser - POST /user
            &hyper::Method::POST if path.matched(paths::ID_USER) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::User> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.create_user(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                CreateUserResponse::SuccessfulOperation


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(0).expect("Unable to turn 0 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // CreateUsersWithArrayInput - POST /user/createWithArray
            &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHARRAY) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<Vec<models::User>> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.create_users_with_array_input(
                                            param_body.as_ref(),
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                CreateUsersWithArrayInputResponse::SuccessfulOperation


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(0).expect("Unable to turn 0 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // CreateUsersWithListInput - POST /user/createWithList
            &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHLIST) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<Vec<models::User>> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.create_users_with_list_input(
                                            param_body.as_ref(),
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                CreateUsersWithListInputResponse::SuccessfulOperation


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(0).expect("Unable to turn 0 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // DeleteUser - DELETE /user/{username}
            &hyper::Method::DELETE if path.matched(paths::ID_USER_USERNAME) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_USER_USERNAME
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE USER_USERNAME in set but failed match against \"{}\"", path, paths::REGEX_USER_USERNAME.as_str())
                    );
                let param_username = match percent_encoding::percent_decode(path_params["username"].as_bytes()).decode_utf8() {
                    Ok(param_username) => match param_username.parse::<String>() {
                        Ok(param_username) => param_username,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter username: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["username"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                Box::new({
                        {{

                                Box::new(
                                    api_impl.delete_user(
                                            param_username,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                DeleteUserResponse::InvalidUsernameSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                DeleteUserResponse::UserNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // GetUserByName - GET /user/{username}
            &hyper::Method::GET if path.matched(paths::ID_USER_USERNAME) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_USER_USERNAME
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE USER_USERNAME in set but failed match against \"{}\"", path, paths::REGEX_USER_USERNAME.as_str())
                    );
                let param_username = match percent_encoding::percent_decode(path_params["username"].as_bytes()).decode_utf8() {
                    Ok(param_username) => match param_username.parse::<String>() {
                        Ok(param_username) => param_username,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter username: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["username"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                Box::new({
                        {{

                                Box::new(
                                    api_impl.get_user_by_name(
                                            param_username,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                GetUserByNameResponse::SuccessfulOperation

                                                    (body)


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::GET_USER_BY_NAME_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for GET_USER_BY_NAME_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                GetUserByNameResponse::InvalidUsernameSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                GetUserByNameResponse::UserNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // LoginUser - GET /user/login
            &hyper::Method::GET if path.matched(paths::ID_USER_LOGIN) => {

                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_username = query_params.iter().filter(|e| e.0 == "username").map(|e| e.1.to_owned())
                    .nth(0);
                let param_username = match param_username {
                    Some(param_username) => match param_username.parse::<String>() {
                        Ok(param_username) => param_username,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse query parameter username - doesn't match schema: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid query parameter username"))),
                    },
                    None => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from("Missing required query parameter username"))
                                        .expect("Unable to create Bad Request response for missing qeury parameter username"))),
                };
                let param_password = query_params.iter().filter(|e| e.0 == "password").map(|e| e.1.to_owned())
                    .nth(0);
                let param_password = match param_password {
                    Some(param_password) => match param_password.parse::<String>() {
                        Ok(param_password) => param_password,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse query parameter password - doesn't match schema: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid query parameter password"))),
                    },
                    None => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from("Missing required query parameter password"))
                                        .expect("Unable to create Bad Request response for missing qeury parameter password"))),
                };
                Box::new({
                        {{

                                Box::new(
                                    api_impl.login_user(
                                            param_username,
                                            param_password,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                LoginUserResponse::SuccessfulOperation

                                                    {
                                                        body,
                                                        x_rate_limit, 

                                                        x_expires_after
                                                    }


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("x-rate-limit"),
                                                        swagger::IntoHeaderValue(x_rate_limit).into()
                                                    );
                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("x-expires-after"),
                                                        swagger::IntoHeaderValue(x_expires_after).into()
                                                    );

                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str(mimetypes::responses::LOGIN_USER_SUCCESSFUL_OPERATION)
                                                            .expect("Unable to create Content-Type header for LOGIN_USER_SUCCESSFUL_OPERATION"));

                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                LoginUserResponse::InvalidUsername


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // LogoutUser - GET /user/logout
            &hyper::Method::GET if path.matched(paths::ID_USER_LOGOUT) => {
                Box::new({
                        {{

                                Box::new(
                                    api_impl.logout_user(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        match result {
                                            Ok(rsp) => match rsp {
                                                LogoutUserResponse::SuccessfulOperation


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(0).expect("Unable to turn 0 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // UpdateUser - PUT /user/{username}
            &hyper::Method::PUT if path.matched(paths::ID_USER_USERNAME) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_USER_USERNAME
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE USER_USERNAME in set but failed match against \"{}\"", path, paths::REGEX_USER_USERNAME.as_str())
                    );
                let param_username = match percent_encoding::percent_decode(path_params["username"].as_bytes()).decode_utf8() {
                    Ok(param_username) => match param_username.parse::<String>() {
                        Ok(param_username) => param_username,
                        Err(e) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter username: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter"))),
                    },
                    Err(_) => return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["username"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode")))
                };
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_body: Option<models::User> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_body) => param_body,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.update_user(
                                            param_username,
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));


                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                UpdateUserResponse::InvalidUserSupplied


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");

                                                },
                                                UpdateUserResponse::UserNotFound


                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            _ => Box::new(future::ok(
                Response::builder().status(StatusCode::NOT_FOUND)
                    .body(Body::empty())
                    .expect("Unable to create Not Found response")
            )) as Self::Future
        }
    }
}

impl<T, C> Clone for Service<T, C> where T: Clone
{
    fn clone(&self) -> Self {
        Service {
            api_impl: self.api_impl.clone(),
            marker: self.marker.clone(),
        }
    }
}

/// Request parser for `Api`.
pub struct ApiRequestParser;
impl<T> RequestParser<T> for ApiRequestParser {
    fn parse_operation_id(request: &Request<T>) -> Result<&'static str, ()> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match request.method() {

            // TestSpecialTags - PATCH /another-fake/dummy
            &hyper::Method::PATCH if path.matched(paths::ID_ANOTHER_FAKE_DUMMY) => Ok("TestSpecialTags"),

            // FakeOuterBooleanSerialize - POST /fake/outer/boolean
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_BOOLEAN) => Ok("FakeOuterBooleanSerialize"),

            // FakeOuterCompositeSerialize - POST /fake/outer/composite
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_COMPOSITE) => Ok("FakeOuterCompositeSerialize"),

            // FakeOuterNumberSerialize - POST /fake/outer/number
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_NUMBER) => Ok("FakeOuterNumberSerialize"),

            // FakeOuterStringSerialize - POST /fake/outer/string
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_STRING) => Ok("FakeOuterStringSerialize"),

            // TestBodyWithQueryParams - PUT /fake/body-with-query-params
            &hyper::Method::PUT if path.matched(paths::ID_FAKE_BODY_WITH_QUERY_PARAMS) => Ok("TestBodyWithQueryParams"),

            // TestClientModel - PATCH /fake
            &hyper::Method::PATCH if path.matched(paths::ID_FAKE) => Ok("TestClientModel"),

            // TestEndpointParameters - POST /fake
            &hyper::Method::POST if path.matched(paths::ID_FAKE) => Ok("TestEndpointParameters"),

            // TestEnumParameters - GET /fake
            &hyper::Method::GET if path.matched(paths::ID_FAKE) => Ok("TestEnumParameters"),

            // TestInlineAdditionalProperties - POST /fake/inline-additionalProperties
            &hyper::Method::POST if path.matched(paths::ID_FAKE_INLINE_ADDITIONALPROPERTIES) => Ok("TestInlineAdditionalProperties"),

            // TestJsonFormData - GET /fake/jsonFormData
            &hyper::Method::GET if path.matched(paths::ID_FAKE_JSONFORMDATA) => Ok("TestJsonFormData"),

            // TestClassname - PATCH /fake_classname_test
            &hyper::Method::PATCH if path.matched(paths::ID_FAKE_CLASSNAME_TEST) => Ok("TestClassname"),

            // AddPet - POST /pet
            &hyper::Method::POST if path.matched(paths::ID_PET) => Ok("AddPet"),

            // DeletePet - DELETE /pet/{petId}
            &hyper::Method::DELETE if path.matched(paths::ID_PET_PETID) => Ok("DeletePet"),

            // FindPetsByStatus - GET /pet/findByStatus
            &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYSTATUS) => Ok("FindPetsByStatus"),

            // FindPetsByTags - GET /pet/findByTags
            &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYTAGS) => Ok("FindPetsByTags"),

            // GetPetById - GET /pet/{petId}
            &hyper::Method::GET if path.matched(paths::ID_PET_PETID) => Ok("GetPetById"),

            // UpdatePet - PUT /pet
            &hyper::Method::PUT if path.matched(paths::ID_PET) => Ok("UpdatePet"),

            // UpdatePetWithForm - POST /pet/{petId}
            &hyper::Method::POST if path.matched(paths::ID_PET_PETID) => Ok("UpdatePetWithForm"),

            // UploadFile - POST /pet/{petId}/uploadImage
            &hyper::Method::POST if path.matched(paths::ID_PET_PETID_UPLOADIMAGE) => Ok("UploadFile"),

            // DeleteOrder - DELETE /store/order/{order_id}
            &hyper::Method::DELETE if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => Ok("DeleteOrder"),

            // GetInventory - GET /store/inventory
            &hyper::Method::GET if path.matched(paths::ID_STORE_INVENTORY) => Ok("GetInventory"),

            // GetOrderById - GET /store/order/{order_id}
            &hyper::Method::GET if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => Ok("GetOrderById"),

            // PlaceOrder - POST /store/order
            &hyper::Method::POST if path.matched(paths::ID_STORE_ORDER) => Ok("PlaceOrder"),

            // CreateUser - POST /user
            &hyper::Method::POST if path.matched(paths::ID_USER) => Ok("CreateUser"),

            // CreateUsersWithArrayInput - POST /user/createWithArray
            &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHARRAY) => Ok("CreateUsersWithArrayInput"),

            // CreateUsersWithListInput - POST /user/createWithList
            &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHLIST) => Ok("CreateUsersWithListInput"),

            // DeleteUser - DELETE /user/{username}
            &hyper::Method::DELETE if path.matched(paths::ID_USER_USERNAME) => Ok("DeleteUser"),

            // GetUserByName - GET /user/{username}
            &hyper::Method::GET if path.matched(paths::ID_USER_USERNAME) => Ok("GetUserByName"),

            // LoginUser - GET /user/login
            &hyper::Method::GET if path.matched(paths::ID_USER_LOGIN) => Ok("LoginUser"),

            // LogoutUser - GET /user/logout
            &hyper::Method::GET if path.matched(paths::ID_USER_LOGOUT) => Ok("LogoutUser"),

            // UpdateUser - PUT /user/{username}
            &hyper::Method::PUT if path.matched(paths::ID_USER_USERNAME) => Ok("UpdateUser"),
            _ => Err(()),
        }
    }
}
