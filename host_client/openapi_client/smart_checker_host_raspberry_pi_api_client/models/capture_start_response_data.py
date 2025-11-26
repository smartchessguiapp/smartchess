from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="CaptureStartResponseData")


@_attrs_define
class CaptureStartResponseData:
    """
    Attributes:
        capture_id (str | Unset):
        sample_rate_hz (float | Unset):
    """

    capture_id: str | Unset = UNSET
    sample_rate_hz: float | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        capture_id = self.capture_id

        sample_rate_hz = self.sample_rate_hz

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if capture_id is not UNSET:
            field_dict["capture_id"] = capture_id
        if sample_rate_hz is not UNSET:
            field_dict["sample_rate_hz"] = sample_rate_hz

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        capture_id = d.pop("capture_id", UNSET)

        sample_rate_hz = d.pop("sample_rate_hz", UNSET)

        capture_start_response_data = cls(
            capture_id=capture_id,
            sample_rate_hz=sample_rate_hz,
        )

        capture_start_response_data.additional_properties = d
        return capture_start_response_data

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
