use crate::headers::*;
use std::io;
use std::marker::PhantomData;
use std::future::Future;
use std::pin::Pin;
use std::default::Default;
use futures::FutureExt;
use headers::{ContentType, HeaderMapExt};
use hyper;
use hyper::{Request, Response, Error, StatusCode};
use url::form_urlencoded;
use openapi_context::auth::{Authorization, AuthData, Scopes};
use openapi_context::{ContextualPayload, Has, Pop, Push, XSpanId};
use crate::Api;

/// Middleware to extract authentication data from request
pub struct AddContext<T, A>
{
    inner: T,
    marker: PhantomData<A>,
}

impl<T, A, B, C, D> AddContext<T, A>
    where
        A: Default + Push<XSpanId, Result=B>,
        B: Push<Option<AuthData>, Result=C>,
        C: Push<Option<Authorization>, Result=D>,
        T: hyper::service::Service<(Request<hyper::Body>, D), Response = Response<hyper::Body>, Error = Error>,
{
    pub fn new(inner: T) -> AddContext<T, A> {
        AddContext {
            inner,
            marker: PhantomData,
        }
    }
}

impl<T, A, B, C, D> hyper::service::Service<ContextualPayload<C>> for AddContext<T, A>
    where
        A: Default + Push<XSpanId, Result=B>,
        B: Push<Option<AuthData>, Result=C>,
        C: Push<Option<Authorization>, Result=D> + Send + Sync,
        T: hyper::service::Service<(Request<hyper::Body>, D), Response = Response<hyper::Body>, Error = Error>,
{
    type Response = Response<hyper::Body>;
    type Error = Error;
    type Future = T::Future;

    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: ContextualPayload<C>) -> Self::Future {
        let context = A::default().push(XSpanId::get_or_generate(&req.inner));
        let mut req = req.inner;


        let context = context.push(None::<AuthData>);
        let context = context.push(None::<Authorization>);
        return self.inner.call((req, context));
    }
}
