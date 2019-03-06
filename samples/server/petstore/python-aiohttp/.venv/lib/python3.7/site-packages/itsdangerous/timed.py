import time
from datetime import datetime

from ._compat import text_type
from .encoding import base64_decode
from .encoding import base64_encode
from .encoding import bytes_to_int
from .encoding import int_to_bytes
from .encoding import want_bytes
from .exc import BadSignature
from .exc import BadTimeSignature
from .exc import SignatureExpired
from .serializer import Serializer
from .signer import Signer


class TimestampSigner(Signer):
    """Works like the regular :class:`.Signer` but also records the time
    of the signing and can be used to expire signatures. The
    :meth:`unsign` method can raise :exc:`.SignatureExpired` if the
    unsigning failed because the signature is expired.
    """

    def get_timestamp(self):
        """Returns the current timestamp. The function must return an
        integer.
        """
        return int(time.time())

    def timestamp_to_datetime(self, ts):
        """Used to convert the timestamp from :meth:`get_timestamp` into
        a datetime object.
        """
        return datetime.utcfromtimestamp(ts)

    def sign(self, value):
        """Signs the given string and also attaches time information."""
        value = want_bytes(value)
        timestamp = base64_encode(int_to_bytes(self.get_timestamp()))
        sep = want_bytes(self.sep)
        value = value + sep + timestamp
        return value + sep + self.get_signature(value)

    def unsign(self, value, max_age=None, return_timestamp=False):
        """Works like the regular :meth:`.Signer.unsign` but can also
        validate the time. See the base docstring of the class for
        the general behavior. If ``return_timestamp`` is ``True`` the
        timestamp of the signature will be returned as a naive
        :class:`datetime.datetime` object in UTC.
        """
        try:
            result = Signer.unsign(self, value)
            sig_error = None
        except BadSignature as e:
            sig_error = e
            result = e.payload or b""
        sep = want_bytes(self.sep)

        # If there is no timestamp in the result there is something
        # seriously wrong. In case there was a signature error, we raise
        # that one directly, otherwise we have a weird situation in
        # which we shouldn't have come except someone uses a time-based
        # serializer on non-timestamp data, so catch that.
        if sep not in result:
            if sig_error:
                raise sig_error
            raise BadTimeSignature("timestamp missing", payload=result)

        value, timestamp = result.rsplit(sep, 1)
        try:
            timestamp = bytes_to_int(base64_decode(timestamp))
        except Exception:
            timestamp = None

        # Signature is *not* okay. Raise a proper error now that we have
        # split the value and the timestamp.
        if sig_error is not None:
            raise BadTimeSignature(
                text_type(sig_error), payload=value, date_signed=timestamp
            )

        # Signature was okay but the timestamp is actually not there or
        # malformed. Should not happen, but we handle it anyway.
        if timestamp is None:
            raise BadTimeSignature("Malformed timestamp", payload=value)

        # Check timestamp is not older than max_age
        if max_age is not None:
            age = self.get_timestamp() - timestamp
            if age > max_age:
                raise SignatureExpired(
                    "Signature age %s > %s seconds" % (age, max_age),
                    payload=value,
                    date_signed=self.timestamp_to_datetime(timestamp),
                )

        if return_timestamp:
            return value, self.timestamp_to_datetime(timestamp)
        return value

    def validate(self, signed_value, max_age=None):
        """Only validates the given signed value. Returns ``True`` if
        the signature exists and is valid."""
        try:
            self.unsign(signed_value, max_age=max_age)
            return True
        except BadSignature:
            return False


class TimedSerializer(Serializer):
    """Uses :class:`TimestampSigner` instead of the default
    :class:`.Signer`.
    """

    default_signer = TimestampSigner

    def loads(self, s, max_age=None, return_timestamp=False, salt=None):
        """Reverse of :meth:`dumps`, raises :exc:`.BadSignature` if the
        signature validation fails. If a ``max_age`` is provided it will
        ensure the signature is not older than that time in seconds. In
        case the signature is outdated, :exc:`.SignatureExpired` is
        raised. All arguments are forwarded to the signer's
        :meth:`~TimestampSigner.unsign` method.
        """
        s = want_bytes(s)
        last_exception = None
        for signer in self.iter_unsigners(salt):
            try:
                base64d, timestamp = signer.unsign(s, max_age, return_timestamp=True)
                payload = self.load_payload(base64d)
                if return_timestamp:
                    return payload, timestamp
                return payload
            # If we get a signature expired it means we could read the
            # signature but it's invalid.  In that case we do not want to
            # try the next signer.
            except SignatureExpired:
                raise
            except BadSignature as err:
                last_exception = err
        raise last_exception

    def loads_unsafe(self, s, max_age=None, salt=None):
        load_kwargs = {"max_age": max_age}
        load_payload_kwargs = {}
        return self._loads_unsafe_impl(s, salt, load_kwargs, load_payload_kwargs)
