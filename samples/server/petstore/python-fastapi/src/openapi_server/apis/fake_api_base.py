# coding: utf-8

from typing import ClassVar, Dict, List, Tuple  # noqa: F401



class BaseFakeApi:
    subclasses: ClassVar[Tuple] = ()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        BaseFakeApi.subclasses = BaseFakeApi.subclasses + (cls,)
    def fake_query_param_default(
        self,
        has_default: str,
        no_default: str,
    ) -> None:
        """"""
        ...
