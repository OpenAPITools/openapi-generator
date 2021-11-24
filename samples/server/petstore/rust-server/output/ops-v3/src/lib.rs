#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

use async_trait::async_trait;
use futures::Stream;
use std::error::Error;
use std::task::{Poll, Context};
use swagger::{ApiError, ContextWrapper};
use serde::{Serialize, Deserialize};

type ServiceError = Box<dyn Error + Send + Sync + 'static>;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "0.0.1";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op10GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op11GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op12GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op13GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op14GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op15GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op16GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op17GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op18GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op19GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op1GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op20GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op21GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op22GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op23GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op24GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op25GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op26GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op27GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op28GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op29GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op2GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op30GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op31GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op32GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op33GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op34GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op35GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op36GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op37GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op3GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op4GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op5GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op6GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op7GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op8GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Op9GetResponse {
    /// OK
    OK
}

/// API
#[async_trait]
pub trait Api<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>> {
        Poll::Ready(Ok(()))
    }

    async fn op10_get(
        &self,
        context: &C) -> Result<Op10GetResponse, ApiError>;

    async fn op11_get(
        &self,
        context: &C) -> Result<Op11GetResponse, ApiError>;

    async fn op12_get(
        &self,
        context: &C) -> Result<Op12GetResponse, ApiError>;

    async fn op13_get(
        &self,
        context: &C) -> Result<Op13GetResponse, ApiError>;

    async fn op14_get(
        &self,
        context: &C) -> Result<Op14GetResponse, ApiError>;

    async fn op15_get(
        &self,
        context: &C) -> Result<Op15GetResponse, ApiError>;

    async fn op16_get(
        &self,
        context: &C) -> Result<Op16GetResponse, ApiError>;

    async fn op17_get(
        &self,
        context: &C) -> Result<Op17GetResponse, ApiError>;

    async fn op18_get(
        &self,
        context: &C) -> Result<Op18GetResponse, ApiError>;

    async fn op19_get(
        &self,
        context: &C) -> Result<Op19GetResponse, ApiError>;

    async fn op1_get(
        &self,
        context: &C) -> Result<Op1GetResponse, ApiError>;

    async fn op20_get(
        &self,
        context: &C) -> Result<Op20GetResponse, ApiError>;

    async fn op21_get(
        &self,
        context: &C) -> Result<Op21GetResponse, ApiError>;

    async fn op22_get(
        &self,
        context: &C) -> Result<Op22GetResponse, ApiError>;

    async fn op23_get(
        &self,
        context: &C) -> Result<Op23GetResponse, ApiError>;

    async fn op24_get(
        &self,
        context: &C) -> Result<Op24GetResponse, ApiError>;

    async fn op25_get(
        &self,
        context: &C) -> Result<Op25GetResponse, ApiError>;

    async fn op26_get(
        &self,
        context: &C) -> Result<Op26GetResponse, ApiError>;

    async fn op27_get(
        &self,
        context: &C) -> Result<Op27GetResponse, ApiError>;

    async fn op28_get(
        &self,
        context: &C) -> Result<Op28GetResponse, ApiError>;

    async fn op29_get(
        &self,
        context: &C) -> Result<Op29GetResponse, ApiError>;

    async fn op2_get(
        &self,
        context: &C) -> Result<Op2GetResponse, ApiError>;

    async fn op30_get(
        &self,
        context: &C) -> Result<Op30GetResponse, ApiError>;

    async fn op31_get(
        &self,
        context: &C) -> Result<Op31GetResponse, ApiError>;

    async fn op32_get(
        &self,
        context: &C) -> Result<Op32GetResponse, ApiError>;

    async fn op33_get(
        &self,
        context: &C) -> Result<Op33GetResponse, ApiError>;

    async fn op34_get(
        &self,
        context: &C) -> Result<Op34GetResponse, ApiError>;

    async fn op35_get(
        &self,
        context: &C) -> Result<Op35GetResponse, ApiError>;

    async fn op36_get(
        &self,
        context: &C) -> Result<Op36GetResponse, ApiError>;

    async fn op37_get(
        &self,
        context: &C) -> Result<Op37GetResponse, ApiError>;

    async fn op3_get(
        &self,
        context: &C) -> Result<Op3GetResponse, ApiError>;

    async fn op4_get(
        &self,
        context: &C) -> Result<Op4GetResponse, ApiError>;

    async fn op5_get(
        &self,
        context: &C) -> Result<Op5GetResponse, ApiError>;

    async fn op6_get(
        &self,
        context: &C) -> Result<Op6GetResponse, ApiError>;

    async fn op7_get(
        &self,
        context: &C) -> Result<Op7GetResponse, ApiError>;

    async fn op8_get(
        &self,
        context: &C) -> Result<Op8GetResponse, ApiError>;

    async fn op9_get(
        &self,
        context: &C) -> Result<Op9GetResponse, ApiError>;

}

/// API where `Context` isn't passed on every API call
#[async_trait]
pub trait ApiNoContext<C: Send + Sync> {

    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>>;

    fn context(&self) -> &C;

    async fn op10_get(
        &self,
        ) -> Result<Op10GetResponse, ApiError>;

    async fn op11_get(
        &self,
        ) -> Result<Op11GetResponse, ApiError>;

    async fn op12_get(
        &self,
        ) -> Result<Op12GetResponse, ApiError>;

    async fn op13_get(
        &self,
        ) -> Result<Op13GetResponse, ApiError>;

    async fn op14_get(
        &self,
        ) -> Result<Op14GetResponse, ApiError>;

    async fn op15_get(
        &self,
        ) -> Result<Op15GetResponse, ApiError>;

    async fn op16_get(
        &self,
        ) -> Result<Op16GetResponse, ApiError>;

    async fn op17_get(
        &self,
        ) -> Result<Op17GetResponse, ApiError>;

    async fn op18_get(
        &self,
        ) -> Result<Op18GetResponse, ApiError>;

    async fn op19_get(
        &self,
        ) -> Result<Op19GetResponse, ApiError>;

    async fn op1_get(
        &self,
        ) -> Result<Op1GetResponse, ApiError>;

    async fn op20_get(
        &self,
        ) -> Result<Op20GetResponse, ApiError>;

    async fn op21_get(
        &self,
        ) -> Result<Op21GetResponse, ApiError>;

    async fn op22_get(
        &self,
        ) -> Result<Op22GetResponse, ApiError>;

    async fn op23_get(
        &self,
        ) -> Result<Op23GetResponse, ApiError>;

    async fn op24_get(
        &self,
        ) -> Result<Op24GetResponse, ApiError>;

    async fn op25_get(
        &self,
        ) -> Result<Op25GetResponse, ApiError>;

    async fn op26_get(
        &self,
        ) -> Result<Op26GetResponse, ApiError>;

    async fn op27_get(
        &self,
        ) -> Result<Op27GetResponse, ApiError>;

    async fn op28_get(
        &self,
        ) -> Result<Op28GetResponse, ApiError>;

    async fn op29_get(
        &self,
        ) -> Result<Op29GetResponse, ApiError>;

    async fn op2_get(
        &self,
        ) -> Result<Op2GetResponse, ApiError>;

    async fn op30_get(
        &self,
        ) -> Result<Op30GetResponse, ApiError>;

    async fn op31_get(
        &self,
        ) -> Result<Op31GetResponse, ApiError>;

    async fn op32_get(
        &self,
        ) -> Result<Op32GetResponse, ApiError>;

    async fn op33_get(
        &self,
        ) -> Result<Op33GetResponse, ApiError>;

    async fn op34_get(
        &self,
        ) -> Result<Op34GetResponse, ApiError>;

    async fn op35_get(
        &self,
        ) -> Result<Op35GetResponse, ApiError>;

    async fn op36_get(
        &self,
        ) -> Result<Op36GetResponse, ApiError>;

    async fn op37_get(
        &self,
        ) -> Result<Op37GetResponse, ApiError>;

    async fn op3_get(
        &self,
        ) -> Result<Op3GetResponse, ApiError>;

    async fn op4_get(
        &self,
        ) -> Result<Op4GetResponse, ApiError>;

    async fn op5_get(
        &self,
        ) -> Result<Op5GetResponse, ApiError>;

    async fn op6_get(
        &self,
        ) -> Result<Op6GetResponse, ApiError>;

    async fn op7_get(
        &self,
        ) -> Result<Op7GetResponse, ApiError>;

    async fn op8_get(
        &self,
        ) -> Result<Op8GetResponse, ApiError>;

    async fn op9_get(
        &self,
        ) -> Result<Op9GetResponse, ApiError>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<C: Send + Sync> where Self: Sized
{
    /// Binds this API to a context.
    fn with_context(self: Self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: Api<C> + Send + Sync, C: Clone + Send + Sync> ContextWrapperExt<C> for T {
    fn with_context(self: T, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: Api<C> + Send + Sync, C: Clone + Send + Sync> ApiNoContext<C> for ContextWrapper<T, C> {
    fn poll_ready(&self, cx: &mut Context) -> Poll<Result<(), ServiceError>> {
        self.api().poll_ready(cx)
    }

    fn context(&self) -> &C {
        ContextWrapper::context(self)
    }

    async fn op10_get(
        &self,
        ) -> Result<Op10GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op10_get(&context).await
    }

    async fn op11_get(
        &self,
        ) -> Result<Op11GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op11_get(&context).await
    }

    async fn op12_get(
        &self,
        ) -> Result<Op12GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op12_get(&context).await
    }

    async fn op13_get(
        &self,
        ) -> Result<Op13GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op13_get(&context).await
    }

    async fn op14_get(
        &self,
        ) -> Result<Op14GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op14_get(&context).await
    }

    async fn op15_get(
        &self,
        ) -> Result<Op15GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op15_get(&context).await
    }

    async fn op16_get(
        &self,
        ) -> Result<Op16GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op16_get(&context).await
    }

    async fn op17_get(
        &self,
        ) -> Result<Op17GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op17_get(&context).await
    }

    async fn op18_get(
        &self,
        ) -> Result<Op18GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op18_get(&context).await
    }

    async fn op19_get(
        &self,
        ) -> Result<Op19GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op19_get(&context).await
    }

    async fn op1_get(
        &self,
        ) -> Result<Op1GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op1_get(&context).await
    }

    async fn op20_get(
        &self,
        ) -> Result<Op20GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op20_get(&context).await
    }

    async fn op21_get(
        &self,
        ) -> Result<Op21GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op21_get(&context).await
    }

    async fn op22_get(
        &self,
        ) -> Result<Op22GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op22_get(&context).await
    }

    async fn op23_get(
        &self,
        ) -> Result<Op23GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op23_get(&context).await
    }

    async fn op24_get(
        &self,
        ) -> Result<Op24GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op24_get(&context).await
    }

    async fn op25_get(
        &self,
        ) -> Result<Op25GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op25_get(&context).await
    }

    async fn op26_get(
        &self,
        ) -> Result<Op26GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op26_get(&context).await
    }

    async fn op27_get(
        &self,
        ) -> Result<Op27GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op27_get(&context).await
    }

    async fn op28_get(
        &self,
        ) -> Result<Op28GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op28_get(&context).await
    }

    async fn op29_get(
        &self,
        ) -> Result<Op29GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op29_get(&context).await
    }

    async fn op2_get(
        &self,
        ) -> Result<Op2GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op2_get(&context).await
    }

    async fn op30_get(
        &self,
        ) -> Result<Op30GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op30_get(&context).await
    }

    async fn op31_get(
        &self,
        ) -> Result<Op31GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op31_get(&context).await
    }

    async fn op32_get(
        &self,
        ) -> Result<Op32GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op32_get(&context).await
    }

    async fn op33_get(
        &self,
        ) -> Result<Op33GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op33_get(&context).await
    }

    async fn op34_get(
        &self,
        ) -> Result<Op34GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op34_get(&context).await
    }

    async fn op35_get(
        &self,
        ) -> Result<Op35GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op35_get(&context).await
    }

    async fn op36_get(
        &self,
        ) -> Result<Op36GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op36_get(&context).await
    }

    async fn op37_get(
        &self,
        ) -> Result<Op37GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op37_get(&context).await
    }

    async fn op3_get(
        &self,
        ) -> Result<Op3GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op3_get(&context).await
    }

    async fn op4_get(
        &self,
        ) -> Result<Op4GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op4_get(&context).await
    }

    async fn op5_get(
        &self,
        ) -> Result<Op5GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op5_get(&context).await
    }

    async fn op6_get(
        &self,
        ) -> Result<Op6GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op6_get(&context).await
    }

    async fn op7_get(
        &self,
        ) -> Result<Op7GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op7_get(&context).await
    }

    async fn op8_get(
        &self,
        ) -> Result<Op8GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op8_get(&context).await
    }

    async fn op9_get(
        &self,
        ) -> Result<Op9GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().op9_get(&context).await
    }

}


#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

#[cfg(feature = "server")]
pub mod context;

pub mod models;

#[cfg(any(feature = "client", feature = "server"))]
pub(crate) mod header;
