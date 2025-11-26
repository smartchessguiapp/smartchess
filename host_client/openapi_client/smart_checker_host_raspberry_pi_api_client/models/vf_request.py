from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

T = TypeVar("T", bound="VFRequest")


@_attrs_define
class VFRequest:
    """
    Attributes:
        enable (bool):
        fbase (float):
        vbase (float):
        vboost (float):
    """

    enable: bool
    fbase: float
    vbase: float
    vboost: float
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        enable = self.enable

        fbase = self.fbase

        vbase = self.vbase

        vboost = self.vboost

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update(
            {
                "enable": enable,
                "fbase": fbase,
                "vbase": vbase,
                "vboost": vboost,
            }
        )

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        enable = d.pop("enable")

        fbase = d.pop("fbase")

        vbase = d.pop("vbase")

        vboost = d.pop("vboost")

        vf_request = cls(
            enable=enable,
            fbase=fbase,
            vbase=vbase,
            vboost=vboost,
        )

        vf_request.additional_properties = d
        return vf_request

    @property
    def additional_keys(self) -> list[str]:
        return list(self.additional_properties.keys())

    def __getitem__(self, key: str) -> Any:
        return self.additional_properties[key]

    def __setitem__(self, key: str, value: Any) -> None:
        self.additional_properties[key] = value

    def __delitem__(self, key: str) -> None:
        del self.additional_properties[key]

    def __contains__(self, key: str) -> bool:
        return key in self.additional_properties
