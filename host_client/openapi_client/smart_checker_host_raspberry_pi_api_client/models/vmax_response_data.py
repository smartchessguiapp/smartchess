from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="VmaxResponseData")


@_attrs_define
class VmaxResponseData:
    """
    Attributes:
        vmax_applied (float | Unset):
        vf_enable (bool | Unset):
    """

    vmax_applied: float | Unset = UNSET
    vf_enable: bool | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        vmax_applied = self.vmax_applied

        vf_enable = self.vf_enable

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if vmax_applied is not UNSET:
            field_dict["vmax_applied"] = vmax_applied
        if vf_enable is not UNSET:
            field_dict["vf_enable"] = vf_enable

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        vmax_applied = d.pop("vmax_applied", UNSET)

        vf_enable = d.pop("vf_enable", UNSET)

        vmax_response_data = cls(
            vmax_applied=vmax_applied,
            vf_enable=vf_enable,
        )

        vmax_response_data.additional_properties = d
        return vmax_response_data

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
