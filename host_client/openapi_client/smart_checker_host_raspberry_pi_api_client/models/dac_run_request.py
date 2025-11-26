from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="DACRunRequest")


@_attrs_define
class DACRunRequest:
    """
    Attributes:
        freq_hz (float | Unset):
        acc_s (float | Unset):
    """

    freq_hz: float | Unset = UNSET
    acc_s: float | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        freq_hz = self.freq_hz

        acc_s = self.acc_s

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if freq_hz is not UNSET:
            field_dict["freq_hz"] = freq_hz
        if acc_s is not UNSET:
            field_dict["acc_s"] = acc_s

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        freq_hz = d.pop("freq_hz", UNSET)

        acc_s = d.pop("acc_s", UNSET)

        dac_run_request = cls(
            freq_hz=freq_hz,
            acc_s=acc_s,
        )

        dac_run_request.additional_properties = d
        return dac_run_request

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
