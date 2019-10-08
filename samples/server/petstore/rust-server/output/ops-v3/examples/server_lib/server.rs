//! Server implementation of ops_v3.

#![allow(unused_imports)]
use async_trait::async_trait;
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;
use openapi_context;
use openapi_context::{Has, XSpanId};

use ops_v3::{Api, ApiError,
                      Op10GetResponse,
                      Op11GetResponse,
                      Op12GetResponse,
                      Op13GetResponse,
                      Op14GetResponse,
                      Op15GetResponse,
                      Op16GetResponse,
                      Op17GetResponse,
                      Op18GetResponse,
                      Op19GetResponse,
                      Op1GetResponse,
                      Op20GetResponse,
                      Op21GetResponse,
                      Op22GetResponse,
                      Op23GetResponse,
                      Op24GetResponse,
                      Op25GetResponse,
                      Op26GetResponse,
                      Op27GetResponse,
                      Op28GetResponse,
                      Op29GetResponse,
                      Op2GetResponse,
                      Op30GetResponse,
                      Op31GetResponse,
                      Op32GetResponse,
                      Op33GetResponse,
                      Op34GetResponse,
                      Op35GetResponse,
                      Op36GetResponse,
                      Op37GetResponse,
                      Op3GetResponse,
                      Op4GetResponse,
                      Op5GetResponse,
                      Op6GetResponse,
                      Op7GetResponse,
                      Op8GetResponse,
                      Op9GetResponse
};

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanId> + Send + Sync {


    async fn op10_get(&mut self, context: &C) -> Result<Op10GetResponse, ApiError> {
        let context = context.clone();
        println!("op10_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op11_get(&mut self, context: &C) -> Result<Op11GetResponse, ApiError> {
        let context = context.clone();
        println!("op11_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op12_get(&mut self, context: &C) -> Result<Op12GetResponse, ApiError> {
        let context = context.clone();
        println!("op12_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op13_get(&mut self, context: &C) -> Result<Op13GetResponse, ApiError> {
        let context = context.clone();
        println!("op13_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op14_get(&mut self, context: &C) -> Result<Op14GetResponse, ApiError> {
        let context = context.clone();
        println!("op14_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op15_get(&mut self, context: &C) -> Result<Op15GetResponse, ApiError> {
        let context = context.clone();
        println!("op15_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op16_get(&mut self, context: &C) -> Result<Op16GetResponse, ApiError> {
        let context = context.clone();
        println!("op16_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op17_get(&mut self, context: &C) -> Result<Op17GetResponse, ApiError> {
        let context = context.clone();
        println!("op17_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op18_get(&mut self, context: &C) -> Result<Op18GetResponse, ApiError> {
        let context = context.clone();
        println!("op18_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op19_get(&mut self, context: &C) -> Result<Op19GetResponse, ApiError> {
        let context = context.clone();
        println!("op19_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op1_get(&mut self, context: &C) -> Result<Op1GetResponse, ApiError> {
        let context = context.clone();
        println!("op1_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op20_get(&mut self, context: &C) -> Result<Op20GetResponse, ApiError> {
        let context = context.clone();
        println!("op20_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op21_get(&mut self, context: &C) -> Result<Op21GetResponse, ApiError> {
        let context = context.clone();
        println!("op21_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op22_get(&mut self, context: &C) -> Result<Op22GetResponse, ApiError> {
        let context = context.clone();
        println!("op22_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op23_get(&mut self, context: &C) -> Result<Op23GetResponse, ApiError> {
        let context = context.clone();
        println!("op23_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op24_get(&mut self, context: &C) -> Result<Op24GetResponse, ApiError> {
        let context = context.clone();
        println!("op24_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op25_get(&mut self, context: &C) -> Result<Op25GetResponse, ApiError> {
        let context = context.clone();
        println!("op25_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op26_get(&mut self, context: &C) -> Result<Op26GetResponse, ApiError> {
        let context = context.clone();
        println!("op26_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op27_get(&mut self, context: &C) -> Result<Op27GetResponse, ApiError> {
        let context = context.clone();
        println!("op27_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op28_get(&mut self, context: &C) -> Result<Op28GetResponse, ApiError> {
        let context = context.clone();
        println!("op28_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op29_get(&mut self, context: &C) -> Result<Op29GetResponse, ApiError> {
        let context = context.clone();
        println!("op29_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op2_get(&mut self, context: &C) -> Result<Op2GetResponse, ApiError> {
        let context = context.clone();
        println!("op2_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op30_get(&mut self, context: &C) -> Result<Op30GetResponse, ApiError> {
        let context = context.clone();
        println!("op30_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op31_get(&mut self, context: &C) -> Result<Op31GetResponse, ApiError> {
        let context = context.clone();
        println!("op31_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op32_get(&mut self, context: &C) -> Result<Op32GetResponse, ApiError> {
        let context = context.clone();
        println!("op32_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op33_get(&mut self, context: &C) -> Result<Op33GetResponse, ApiError> {
        let context = context.clone();
        println!("op33_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op34_get(&mut self, context: &C) -> Result<Op34GetResponse, ApiError> {
        let context = context.clone();
        println!("op34_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op35_get(&mut self, context: &C) -> Result<Op35GetResponse, ApiError> {
        let context = context.clone();
        println!("op35_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op36_get(&mut self, context: &C) -> Result<Op36GetResponse, ApiError> {
        let context = context.clone();
        println!("op36_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op37_get(&mut self, context: &C) -> Result<Op37GetResponse, ApiError> {
        let context = context.clone();
        println!("op37_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op3_get(&mut self, context: &C) -> Result<Op3GetResponse, ApiError> {
        let context = context.clone();
        println!("op3_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op4_get(&mut self, context: &C) -> Result<Op4GetResponse, ApiError> {
        let context = context.clone();
        println!("op4_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op5_get(&mut self, context: &C) -> Result<Op5GetResponse, ApiError> {
        let context = context.clone();
        println!("op5_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op6_get(&mut self, context: &C) -> Result<Op6GetResponse, ApiError> {
        let context = context.clone();
        println!("op6_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op7_get(&mut self, context: &C) -> Result<Op7GetResponse, ApiError> {
        let context = context.clone();
        println!("op7_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op8_get(&mut self, context: &C) -> Result<Op8GetResponse, ApiError> {
        let context = context.clone();
        println!("op8_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn op9_get(&mut self, context: &C) -> Result<Op9GetResponse, ApiError> {
        let context = context.clone();
        println!("op9_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

}
