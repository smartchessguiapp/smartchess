from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="CaptureStartRequest")


@_attrs_define
class CaptureStartRequest:
    """
    Attributes:
        sample_rate_hz (float | Unset):
        max_duration_s (float | Unset):
    """

    sample_rate_hz: float | Unset = UNSET
    max_duration_s: float | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        sample_rate_hz = self.sample_rate_hz

        max_duration_s = self.max_duration_s

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if sample_rate_hz is not UNSET:
            field_dict["sample_rate_hz"] = sample_rate_hz
        if max_duration_s is not UNSET:
            field_dict["max_duration_s"] = max_duration_s

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        sample_rate_hz = d.pop("sample_rate_hz", UNSET)

        max_duration_s = d.pop("max_duration_s", UNSET)

        capture_start_request = cls(
            sample_rate_hz=sample_rate_hz,
            max_duration_s=max_duration_s,
        )

        capture_start_request.additional_properties = d
        return capture_start_request

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
