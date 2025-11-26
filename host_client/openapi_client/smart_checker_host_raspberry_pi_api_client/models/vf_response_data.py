from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="VFResponseData")


@_attrs_define
class VFResponseData:
    """
    Attributes:
        vf_enable (bool | Unset):
        vf_fbase (float | Unset):
        vf_vbase (float | Unset):
        vf_vboost (float | Unset):
    """

    vf_enable: bool | Unset = UNSET
    vf_fbase: float | Unset = UNSET
    vf_vbase: float | Unset = UNSET
    vf_vboost: float | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        vf_enable = self.vf_enable

        vf_fbase = self.vf_fbase

        vf_vbase = self.vf_vbase

        vf_vboost = self.vf_vboost

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if vf_enable is not UNSET:
            field_dict["vf_enable"] = vf_enable
        if vf_fbase is not UNSET:
            field_dict["vf_fbase"] = vf_fbase
        if vf_vbase is not UNSET:
            field_dict["vf_vbase"] = vf_vbase
        if vf_vboost is not UNSET:
            field_dict["vf_vboost"] = vf_vboost

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        vf_enable = d.pop("vf_enable", UNSET)

        vf_fbase = d.pop("vf_fbase", UNSET)

        vf_vbase = d.pop("vf_vbase", UNSET)

        vf_vboost = d.pop("vf_vboost", UNSET)

        vf_response_data = cls(
            vf_enable=vf_enable,
            vf_fbase=vf_fbase,
            vf_vbase=vf_vbase,
            vf_vboost=vf_vboost,
        )

        vf_response_data.additional_properties = d
        return vf_response_data

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
