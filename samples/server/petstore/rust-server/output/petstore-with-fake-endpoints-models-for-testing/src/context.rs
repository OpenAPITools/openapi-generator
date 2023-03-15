use futures::future::BoxFuture;
use hyper::header::HeaderName;
use hyper::{Error, Request, Response, StatusCode, service::Service};
use url::form_urlencoded;
use std::default::Default;
use std::io;
use std::marker::PhantomData;
use std::task::{Poll, Context};
use swagger::auth::{AuthData, Authorization, Bearer, Scopes};
use swagger::{EmptyContext, Has, Pop, Push, XSpanIdString};
use crate::Api;

pub struct MakeAddContext<T, A> {
    inner: T,
    marker: PhantomData<A>,
}

impl<T, A, B, C, D> MakeAddContext<T, A>
where
    A: Default + Push<XSpanIdString, Result = B>,
    B: Push<Option<AuthData>, Result = C>,
    C: Push<Option<Authorization>, Result = D>,
{
    pub fn new(inner: T) -> MakeAddContext<T, A> {
        MakeAddContext {
            inner,
            marker: PhantomData,
        }
    }
}

// Make a service that adds context.
impl<Target, T, A, B, C, D> Service<Target> for
    MakeAddContext<T, A>
where
    Target: Send,
    A: Default + Push<XSpanIdString, Result = B> + Send,
    B: Push<Option<AuthData>, Result = C>,
    C: Push<Option<Authorization>, Result = D>,
    D: Send + 'static,
    T: Service<Target> + Send,
    T::Future: Send + 'static
{
    type Error = T::Error;
    type Response = AddContext<T::Response, A, B, C, D>;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, target: Target) -> Self::Future {
        let service = self.inner.call(target);

        Box::pin(async move {
            Ok(AddContext::new(service.await?))
        })
    }
}

/// Middleware to add context data from the request
pub struct AddContext<T, A, B, C, D>
where
    A: Default + Push<XSpanIdString, Result = B>,
    B: Push<Option<AuthData>, Result = C>,
    C: Push<Option<Authorization>, Result = D>
{
    inner: T,
    marker: PhantomData<A>,
}

impl<T, A, B, C, D> AddContext<T, A, B, C, D>
where
    A: Default + Push<XSpanIdString, Result = B>,
    B: Push<Option<AuthData>, Result = C>,
    C: Push<Option<Authorization>, Result = D>,
{
    pub fn new(inner: T) -> Self {
        AddContext {
            inner,
            marker: PhantomData,
        }
    }
}

impl<T, A, B, C, D, ReqBody> Service<Request<ReqBody>> for AddContext<T, A, B, C, D>
    where
        A: Default + Push<XSpanIdString, Result=B>,
        B: Push<Option<AuthData>, Result=C>,
        C: Push<Option<Authorization>, Result=D>,
        D: Send + 'static,
        T: Service<(Request<ReqBody>, D)>
{
    type Error = T::Error;
    type Future = T::Future;
    type Response = T::Response;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }


    fn call(&mut self, request: Request<ReqBody>) -> Self::Future {
        let context = A::default().push(XSpanIdString::get_or_generate(&request));
        let headers = request.headers();

        {
            use swagger::auth::api_key_from_header;

            if let Some(header) = api_key_from_header(&headers, "api_key") {
                let auth_data = AuthData::ApiKey(header);
                let context = context.push(Some(auth_data));
                let context = context.push(None::<Authorization>);

                return self.inner.call((request, context))
            }
        }
        {
            let key = form_urlencoded::parse(request.uri().query().unwrap_or_default().as_bytes())
                .filter(|e| e.0 == "api_key_query")
                .map(|e| e.1.clone().into_owned())
                .nth(0);
            if let Some(key) = key {
                let auth_data = AuthData::ApiKey(key);
                let context = context.push(Some(auth_data));
                let context = context.push(None::<Authorization>);

                return self.inner.call((request, context))
            }
        }
        {
            use swagger::auth::Basic;
            use std::ops::Deref;
            if let Some(basic) = swagger::auth::from_headers::<Basic>(&headers) {
                let auth_data = AuthData::Basic(basic);
                let context = context.push(Some(auth_data));
                let context = context.push(None::<Authorization>);

                return self.inner.call((request, context))
            }
        }
        {
            use swagger::auth::Bearer;
            use std::ops::Deref;
            if let Some(bearer) = swagger::auth::from_headers::<Bearer>(&headers) {
                let auth_data = AuthData::Bearer(bearer);
                let context = context.push(Some(auth_data));
                let context = context.push(None::<Authorization>);

                return self.inner.call((request, context))
            }
        }

        let context = context.push(None::<AuthData>);
        let context = context.push(None::<Authorization>);

        self.inner.call((request, context))
    }
}
