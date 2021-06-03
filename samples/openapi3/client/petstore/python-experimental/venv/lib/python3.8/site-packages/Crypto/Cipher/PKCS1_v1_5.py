# -*- coding: utf-8 -*-
#
#  Cipher/PKCS1-v1_5.py : PKCS#1 v1.5
#
# ===================================================================
# The contents of this file are dedicated to the public domain.  To
# the extent that dedication to the public domain is not available,
# everyone is granted a worldwide, perpetual, royalty-free,
# non-exclusive license to exercise all rights associated with the
# contents of this file for any purpose whatsoever.
# No rights are reserved.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ===================================================================

__all__ = [ 'new', 'PKCS115_Cipher' ]

from Crypto.Util.number import ceil_div, bytes_to_long, long_to_bytes
from Crypto.Util.py3compat import bord, _copy_bytes
import Crypto.Util.number
from Crypto import Random

class PKCS115_Cipher:
    """This cipher can perform PKCS#1 v1.5 RSA encryption or decryption.
    Do not instantiate directly. Use :func:`Crypto.Cipher.PKCS1_v1_5.new` instead."""

    def __init__(self, key, randfunc):
        """Initialize this PKCS#1 v1.5 cipher object.

        :Parameters:
         key : an RSA key object
          If a private half is given, both encryption and decryption are possible.
          If a public half is given, only encryption is possible.
         randfunc : callable
          Function that returns random bytes.
        """

        self._key = key
        self._randfunc = randfunc

    def can_encrypt(self):
        """Return True if this cipher object can be used for encryption."""
        return self._key.can_encrypt()

    def can_decrypt(self):
        """Return True if this cipher object can be used for decryption."""
        return self._key.can_decrypt()

    def encrypt(self, message):
        """Produce the PKCS#1 v1.5 encryption of a message.

        This function is named ``RSAES-PKCS1-V1_5-ENCRYPT``, and it is specified in
        `section 7.2.1 of RFC8017
        <https://tools.ietf.org/html/rfc8017#page-28>`_.

        :param message:
            The message to encrypt, also known as plaintext. It can be of
            variable length, but not longer than the RSA modulus (in bytes) minus 11.
        :type message: bytes/bytearray/memoryview

        :Returns: A byte string, the ciphertext in which the message is encrypted.
            It is as long as the RSA modulus (in bytes).

        :Raises ValueError:
            If the RSA key length is not sufficiently long to deal with the given
            message.
        """

        # See 7.2.1 in RFC8017
        modBits = Crypto.Util.number.size(self._key.n)
        k = ceil_div(modBits,8) # Convert from bits to bytes
        mLen = len(message)

        # Step 1
        if mLen > k - 11:
            raise ValueError("Plaintext is too long.")
        # Step 2a
        ps = []
        while len(ps) != k - mLen - 3:
            new_byte = self._randfunc(1)
            if bord(new_byte[0]) == 0x00:
                continue
            ps.append(new_byte)
        ps = b"".join(ps)
        assert(len(ps) == k - mLen - 3)
        # Step 2b
        em = b'\x00\x02' + ps + b'\x00' + _copy_bytes(None, None, message)
        # Step 3a (OS2IP)
        em_int = bytes_to_long(em)
        # Step 3b (RSAEP)
        m_int = self._key._encrypt(em_int)
        # Step 3c (I2OSP)
        c = long_to_bytes(m_int, k)
        return c

    def decrypt(self, ciphertext, sentinel):
        r"""Decrypt a PKCS#1 v1.5 ciphertext.

        This function is named ``RSAES-PKCS1-V1_5-DECRYPT``, and is specified in
        `section 7.2.2 of RFC8017
        <https://tools.ietf.org/html/rfc8017#page-29>`_.

        :param ciphertext:
            The ciphertext that contains the message to recover.
        :type ciphertext: bytes/bytearray/memoryview

        :param sentinel:
            The object to return whenever an error is detected.
        :type sentinel: any type

        :Returns: A byte string. It is either the original message or the ``sentinel`` (in case of an error).

        :Raises ValueError:
            If the ciphertext length is incorrect
        :Raises TypeError:
            If the RSA key has no private half (i.e. it cannot be used for
            decyption).

        .. warning::
            You should **never** let the party who submitted the ciphertext know that
            this function returned the ``sentinel`` value.
            Armed with such knowledge (for a fair amount of carefully crafted but invalid ciphertexts),
            an attacker is able to recontruct the plaintext of any other encryption that were carried out
            with the same RSA public key (see `Bleichenbacher's`__ attack).

            In general, it should not be possible for the other party to distinguish
            whether processing at the server side failed because the value returned
            was a ``sentinel`` as opposed to a random, invalid message.

            In fact, the second option is not that unlikely: encryption done according to PKCS#1 v1.5
            embeds no good integrity check. There is roughly one chance
            in 2\ :sup:`16` for a random ciphertext to be returned as a valid message
            (although random looking).

            It is therefore advisabled to:

            1. Select as ``sentinel`` a value that resembles a plausable random, invalid message.
            2. Not report back an error as soon as you detect a ``sentinel`` value.
               Put differently, you should not explicitly check if the returned value is the ``sentinel`` or not.
            3. Cover all possible errors with a single, generic error indicator.
            4. Embed into the definition of ``message`` (at the protocol level) a digest (e.g. ``SHA-1``).
               It is recommended for it to be the rightmost part ``message``.
            5. Where possible, monitor the number of errors due to ciphertexts originating from the same party,
               and slow down the rate of the requests from such party (or even blacklist it altogether).

            **If you are designing a new protocol, consider using the more robust PKCS#1 OAEP.**

            .. __: http://www.bell-labs.com/user/bleichen/papers/pkcs.ps

        """

        # See 7.2.1 in RFC3447
        modBits = Crypto.Util.number.size(self._key.n)
        k = ceil_div(modBits,8) # Convert from bits to bytes

        # Step 1
        if len(ciphertext) != k:
            raise ValueError("Ciphertext with incorrect length.")
        # Step 2a (O2SIP)
        ct_int = bytes_to_long(ciphertext)
        # Step 2b (RSADP)
        m_int = self._key._decrypt(ct_int)
        # Complete step 2c (I2OSP)
        em = long_to_bytes(m_int, k)
        # Step 3
        sep = em.find(b'\x00', 2)
        if  not em.startswith(b'\x00\x02') or sep < 10:
            return sentinel
        # Step 4
        return em[sep + 1:]


def new(key, randfunc=None):
    """Create a cipher for performing PKCS#1 v1.5 encryption or decryption.

    :param key:
      The key to use to encrypt or decrypt the message. This is a `Crypto.PublicKey.RSA` object.
      Decryption is only possible if *key* is a private RSA key.
    :type key: RSA key object

    :param randfunc:
      Function that return random bytes.
      The default is :func:`Crypto.Random.get_random_bytes`.
    :type randfunc: callable

    :returns: A cipher object `PKCS115_Cipher`.
    """

    if randfunc is None:
        randfunc = Random.get_random_bytes
    return PKCS115_Cipher(key, randfunc)

