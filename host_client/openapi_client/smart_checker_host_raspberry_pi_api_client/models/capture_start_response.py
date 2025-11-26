from __future__ import annotations

from collections.abc import Mapping
from typing import TYPE_CHECKING, Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

if TYPE_CHECKING:
    from ..models.capture_start_response_data import CaptureStartResponseData


T = TypeVar("T", bound="CaptureStartResponse")


@_attrs_define
class CaptureStartResponse:
    """
    Attributes:
        ok (bool | Unset):  Example: True.
        data (CaptureStartResponseData | Unset):
    """

    ok: bool | Unset = UNSET
    data: CaptureStartResponseData | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        ok = self.ok

        data: dict[str, Any] | Unset = UNSET
        if not isinstance(self.data, Unset):
            data = self.data.to_dict()

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if ok is not UNSET:
            field_dict["ok"] = ok
        if data is not UNSET:
            field_dict["data"] = data

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        from ..models.capture_start_response_data import CaptureStartResponseData

        d = dict(src_dict)
        ok = d.pop("ok", UNSET)

        _data = d.pop("data", UNSET)
        data: CaptureStartResponseData | Unset
        if isinstance(_data, Unset):
            data = UNSET
        else:
            data = CaptureStartResponseData.from_dict(_data)

        capture_start_response = cls(
            ok=ok,
            data=data,
        )

        capture_start_response.additional_properties = d
        return capture_start_response

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
