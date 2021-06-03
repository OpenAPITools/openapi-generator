# -*- coding: utf-8 -*-

"""
Twisted-specific convenience helpers.
"""

from __future__ import absolute_import, division, print_function

from OpenSSL.SSL import FILETYPE_PEM
from twisted.internet import ssl

from ._core import Certificate, DHParameters, Key, parse_file


# mypy hack: Import typing information without actually importing anything.
MYPY = False
if MYPY:  # pragma: nocover
    from typing import Any, List

    from ._core import AbstractPEMObject


def certificateOptionsFromPEMs(pemObjects, **kw):
    # type: (List[AbstractPEMObject], **Any) -> ssl.CerticateOptions
    """
    Load a CertificateOptions from the given collection of PEM objects
    (already-loaded private keys and certificates).

    In those PEM objects, identify one private key and its corresponding
    certificate to use as the primary certificate.  Then use the rest of the
    certificates found as chain certificates.  Raise a ValueError if no
    certificate matching a private key is found.

    :return: A TLS context factory using *pemObjects*
    :rtype: `twisted.internet.ssl.CertificateOptions`_

    .. _`twisted.internet.ssl.CertificateOptions`: \
        https://twistedmatrix.com/documents/current/api/\
        twisted.internet.ssl.CertificateOptions.html
    """
    keys = [key for key in pemObjects if isinstance(key, Key)]
    if not len(keys):
        raise ValueError("Supplied PEM file(s) does *not* contain a key.")
    if len(keys) > 1:
        raise ValueError("Supplied PEM file(s) contains *more* than one key.")

    privateKey = ssl.KeyPair.load(str(keys[0]), FILETYPE_PEM)

    certs = [cert for cert in pemObjects if isinstance(cert, Certificate)]
    if not len(certs):
        raise ValueError("*At least one* certificate is required.")
    certificates = [ssl.Certificate.loadPEM(str(certPEM)) for certPEM in certs]

    certificatesByFingerprint = dict(
        [
            (certificate.getPublicKey().keyHash(), certificate)
            for certificate in certificates
        ]
    )

    if privateKey.keyHash() not in certificatesByFingerprint:
        raise ValueError(
            "No certificate matching {fingerprint} found.".format(
                fingerprint=privateKey.keyHash()
            )
        )

    primaryCertificate = certificatesByFingerprint.pop(privateKey.keyHash())

    if "dhParameters" in kw:
        raise TypeError(
            "Passing DH parameters as a keyword argument instead of a "
            "PEM object is not supported anymore."
        )

    dhparams = [o for o in pemObjects if isinstance(o, DHParameters)]
    if len(dhparams) > 1:
        raise ValueError(
            "Supplied PEM file(s) contain(s) *more* than one set of DH "
            "parameters."
        )
    elif len(dhparams) == 1:
        kw["dhParameters"] = ssl.DiffieHellmanParameters(str(dhparams[0]))

    ctxFactory = ssl.CertificateOptions(
        privateKey=privateKey.original,
        certificate=primaryCertificate.original,
        extraCertChain=[
            chain.original for chain in certificatesByFingerprint.values()
        ],
        **kw
    )

    return ctxFactory


def certificateOptionsFromFiles(*pemFiles, **kw):
    # type: (*str, **Any) -> ssl.CertificateOptions
    """
    Read all files named by *pemFiles*, and parse them using
    :func:`certificateOptionsFromPEMs`.
    """
    pems = []  # type: List[AbstractPEMObject]
    for pemFile in pemFiles:
        pems += parse_file(pemFile)

    return certificateOptionsFromPEMs(pems, **kw)
