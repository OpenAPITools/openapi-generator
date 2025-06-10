use futures::future::BoxFuture;
use hyper::header::HeaderName;
use hyper::{Error, Request, Response, StatusCode, service::Service};
use url::form_urlencoded;
use std::default::Default;
use std::io;
use std::marker::PhantomData;
use std::task::{Poll, Context};
use swagger::auth::{AuthData, Authorization, Scopes};
use swagger::{EmptyContext, Has, Pop, Push, XSpanIdString};
use crate::Api;
use log::error;

pub struct MakeAddContext<T, A> {
    inner: T,
    marker: PhantomData<A>,
}

impl<T, A, B, C> MakeAddContext<T, A>
where
    A: Default + Push<XSpanIdString, Result = B>,
    B: Push<Option<AuthData>, Result = C>,
    C: Send + 'static,
{
    pub fn new(inner: T) -> MakeAddContext<T, A> {
        MakeAddContext {
            inner,
            marker: PhantomData,
        }
    }
}

impl<T, A> Clone for MakeAddContext<T, A>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            marker: PhantomData,
        }
    }
}

// Make a service that adds context.
impl<Target, T, A, B, C> Service<Target> for
    MakeAddContext<T, A>
where
    Target: Send,
    A: Default + Push<XSpanIdString, Result = B> + Send,
    B: Push<Option<AuthData>, Result = C>,
    C: Send + 'static,
    T: Service<Target> + Send,
    T::Future: Send + 'static
{
    type Error = T::Error;
    type Response = AddContext<T::Response, A>;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn call(&self, target: Target) -> Self::Future {
        let service = self.inner.call(target);

        Box::pin(async move {
            Ok(AddContext::new(service.await?))
        })
    }
}

/// Middleware to add context data from the request
#[derive(Debug, Clone)]
pub struct AddContext<T, A>
{
    inner: T,
    marker: PhantomData<A>,
}

impl<T, A, B, C> AddContext<T, A>
where
    A: Default + Push<XSpanIdString, Result = B>,
    B: Push<Option<AuthData>, Result = C>,
{
    pub fn new(inner: T) -> Self {
        AddContext {
            inner,
            marker: PhantomData,
        }
    }
}

impl<T, A, B, C, ReqBody> Service<Request<ReqBody>> for AddContext<T, A>
    where
        A: Default + Push<XSpanIdString, Result=B>,
        B: Push<Option<AuthData>, Result=C>,
        C: Send + 'static,
        T: Service<(Request<ReqBody>, C)>
{
    type Error = T::Error;
    type Future = T::Future;
    type Response = T::Response;

    fn call(&self, request: Request<ReqBody>) -> Self::Future {
        let context = A::default().push(XSpanIdString::get_or_generate(&request));
        let headers = request.headers();


        let context = context.push(None::<AuthData>);

        self.inner.call((request, context))
    }
}
