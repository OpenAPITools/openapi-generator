import hashlib
import hmac

from ._compat import constant_time_compare
from .encoding import _base64_alphabet
from .encoding import base64_decode
from .encoding import base64_encode
from .encoding import want_bytes
from .exc import BadSignature


class SigningAlgorithm(object):
    """Subclasses must implement :meth:`get_signature` to provide
    signature generation functionality.
    """

    def get_signature(self, key, value):
        """Returns the signature for the given key and value."""
        raise NotImplementedError()

    def verify_signature(self, key, value, sig):
        """Verifies the given signature matches the expected
        signature.
        """
        return constant_time_compare(sig, self.get_signature(key, value))


class NoneAlgorithm(SigningAlgorithm):
    """Provides an algorithm that does not perform any signing and
    returns an empty signature.
    """

    def get_signature(self, key, value):
        return b""


class HMACAlgorithm(SigningAlgorithm):
    """Provides signature generation using HMACs."""

    #: The digest method to use with the MAC algorithm. This defaults to
    #: SHA1, but can be changed to any other function in the hashlib
    #: module.
    default_digest_method = staticmethod(hashlib.sha1)

    def __init__(self, digest_method=None):
        if digest_method is None:
            digest_method = self.default_digest_method
        self.digest_method = digest_method

    def get_signature(self, key, value):
        mac = hmac.new(key, msg=value, digestmod=self.digest_method)
        return mac.digest()


class Signer(object):
    """This class can sign and unsign bytes, validating the signature
    provided.

    Salt can be used to namespace the hash, so that a signed string is
    only valid for a given namespace. Leaving this at the default value
    or re-using a salt value across different parts of your application
    where the same signed value in one part can mean something different
    in another part is a security risk.

    See :ref:`the-salt` for an example of what the salt is doing and how
    you can utilize it.

    .. versionadded:: 0.14
        ``key_derivation`` and ``digest_method`` were added as arguments
        to the class constructor.

    .. versionadded:: 0.18
        ``algorithm`` was added as an argument to the class constructor.
    """

    #: The digest method to use for the signer.  This defaults to
    #: SHA1 but can be changed to any other function in the hashlib
    #: module.
    #:
    #: .. versionadded:: 0.14
    default_digest_method = staticmethod(hashlib.sha1)

    #: Controls how the key is derived. The default is Django-style
    #: concatenation. Possible values are ``concat``, ``django-concat``
    #: and ``hmac``. This is used for deriving a key from the secret key
    #: with an added salt.
    #:
    #: .. versionadded:: 0.14
    default_key_derivation = "django-concat"

    def __init__(
        self,
        secret_key,
        salt=None,
        sep=".",
        key_derivation=None,
        digest_method=None,
        algorithm=None,
    ):
        self.secret_key = want_bytes(secret_key)
        self.sep = want_bytes(sep)
        if self.sep in _base64_alphabet:
            raise ValueError(
                "The given separator cannot be used because it may be"
                " contained in the signature itself. Alphanumeric"
                " characters and `-_=` must not be used."
            )
        self.salt = "itsdangerous.Signer" if salt is None else salt
        if key_derivation is None:
            key_derivation = self.default_key_derivation
        self.key_derivation = key_derivation
        if digest_method is None:
            digest_method = self.default_digest_method
        self.digest_method = digest_method
        if algorithm is None:
            algorithm = HMACAlgorithm(self.digest_method)
        self.algorithm = algorithm

    def derive_key(self):
        """This method is called to derive the key. The default key
        derivation choices can be overridden here. Key derivation is not
        intended to be used as a security method to make a complex key
        out of a short password. Instead you should use large random
        secret keys.
        """
        salt = want_bytes(self.salt)
        if self.key_derivation == "concat":
            return self.digest_method(salt + self.secret_key).digest()
        elif self.key_derivation == "django-concat":
            return self.digest_method(salt + b"signer" + self.secret_key).digest()
        elif self.key_derivation == "hmac":
            mac = hmac.new(self.secret_key, digestmod=self.digest_method)
            mac.update(salt)
            return mac.digest()
        elif self.key_derivation == "none":
            return self.secret_key
        else:
            raise TypeError("Unknown key derivation method")

    def get_signature(self, value):
        """Returns the signature for the given value."""
        value = want_bytes(value)
        key = self.derive_key()
        sig = self.algorithm.get_signature(key, value)
        return base64_encode(sig)

    def sign(self, value):
        """Signs the given string."""
        return want_bytes(value) + want_bytes(self.sep) + self.get_signature(value)

    def verify_signature(self, value, sig):
        """Verifies the signature for the given value."""
        key = self.derive_key()
        try:
            sig = base64_decode(sig)
        except Exception:
            return False
        return self.algorithm.verify_signature(key, value, sig)

    def unsign(self, signed_value):
        """Unsigns the given string."""
        signed_value = want_bytes(signed_value)
        sep = want_bytes(self.sep)
        if sep not in signed_value:
            raise BadSignature("No %r found in value" % self.sep)
        value, sig = signed_value.rsplit(sep, 1)
        if self.verify_signature(value, sig):
            return value
        raise BadSignature("Signature %r does not match" % sig, payload=value)

    def validate(self, signed_value):
        """Only validates the given signed value. Returns ``True`` if
        the signature exists and is valid.
        """
        try:
            self.unsign(signed_value)
            return True
        except BadSignature:
            return False
