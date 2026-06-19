# coding: utf-8

from pydantic import BaseModel

class TokenModel(BaseModel):
    """Defines a token model."""

    sub: str
