use futures::{Stream, StreamExt, TryFutureExt, TryStreamExt};
use pin_project::pin_project;
use std::fmt::Display;
use std::fs::File;
use std::future::Future;
use std::io::{BufReader, Error, ErrorKind, Result};
use std::net::SocketAddr;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::net::{TcpListener, TcpStream};
use tokio_rustls::{Accept, TlsAcceptor};
use tokio_rustls::server::TlsStream;

// Load public certificate from file.
pub fn load_certs<P: AsRef<Path> + Display>(filename: P) -> Result<Vec<rustls::Certificate>> {
    // Open certificate file.
    let certfile = File::open(filename.as_ref())
        .map_err(|e| {
            Error::new(ErrorKind::Other, format!("failed to open {}: {}", filename, e))
        })?;
    let mut reader = BufReader::new(certfile);

    // Load and return certificate.
    rustls::internal::pemfile::certs(&mut reader).map_err(|_| {
        Error::new(ErrorKind::Other, "failed to load certificate")
    })
}

// Load private key from file.
pub fn load_private_key<P: AsRef<Path> + Display>(filename: P) -> Result<rustls::PrivateKey> {
    // Open keyfile.
    let keyfile = File::open(filename.as_ref())
        .map_err(|e| {
            Error::new(ErrorKind::Other, format!("failed to open {}: {}", filename, e))
        })?;
    let mut reader = BufReader::new(keyfile);

    // Load and return a single private key.
    let keys = rustls::internal::pemfile::rsa_private_keys(&mut reader)
        .map_err(|_| {
            Error::new(ErrorKind::Other, "failed to load private key")
        })?;
    if keys.len() != 1 {
        return Err(Error::new(ErrorKind::Other,"expected a single private key"));
    }
    Ok(keys[0].clone())
}

#[pin_project]
pub struct HyperAcceptor {
    #[pin]
    tcp: TcpListener,
    acceptor: TlsAcceptor,
    pending: Option<Accept<TcpStream>>,
}

impl hyper::server::accept::Accept for HyperAcceptor {
    type Conn = TlsStream<TcpStream>;
    type Error = Error;

    fn poll_accept(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
    ) -> Poll<Option<std::result::Result<Self::Conn, Self::Error>>> {
        let mut this = self.project();
        loop {
            if let Some(mut fut_accept) = this.pending.take() {
                match Pin::new(&mut fut_accept).poll(cx) {
                    Poll::Pending => {
                        *this.pending = Some(fut_accept);
                        return Poll::Pending;
                    }
                    Poll::Ready(try_c) => {
                        let try_c = try_c.map_err(|e| {
                            println!("[!] Voluntary server halt due to client-connection error...");
                            // Errors could be handled here, instead of server aborting.
                            // Ok(None)
                            Error::new(ErrorKind::Other, format!("TLS Error: {:?}", e))
                        });
                        return Poll::Ready(Some(try_c));
                    }
                }
            }
            match futures::ready!(this.tcp.poll_accept(cx)) {
                Err(e) => {
                    let e = Error::new(ErrorKind::Other, format!("Incoming failed: {:?}", e));
                    return Poll::Ready(Some(Err(e)))
                }
                Ok((s, _)) => {
                    *this.pending = Some(this.acceptor.accept(s));
                }
            }
        }
    }
}

pub async fn acceptor<P: AsRef<Path> + Display>(
    addr: &SocketAddr,
    certs: P,
    key: P,
) -> Result<HyperAcceptor> {
    // Load public certificate.
    let certs = load_certs(certs)?;
    // Load private key.
    let key = load_private_key(key)?;
    // Do not use client certificate authentication.
    let mut cfg = rustls::ServerConfig::new(rustls::NoClientAuth::new());
    // Select a certificate to use.
    cfg.set_single_cert(certs, key)
        .map_err(|e| {
            Error::new(ErrorKind::Other, format!("{}", e))
        })?;
    // Configure ALPN to accept HTTP/2, HTTP/1.1 in that order.
    cfg.set_protocols(&[b"h2".to_vec(), b"http/1.1".to_vec()]);
    let cfg = std::sync::Arc::new(cfg);

    // Create a TCP listener via tokio.
    let tcp = TcpListener::bind(addr).await?;
    let tls_acceptor = TlsAcceptor::from(cfg);
    let acceptor = HyperAcceptor {
        tcp: tcp,
        acceptor: tls_acceptor,
        pending: None,
    };

    Ok(acceptor)
}