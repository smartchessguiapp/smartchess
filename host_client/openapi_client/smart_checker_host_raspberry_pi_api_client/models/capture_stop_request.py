from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="CaptureStopRequest")


@_attrs_define
class CaptureStopRequest:
    """
    Attributes:
        capture_id (str):
        return_data (bool | Unset):
        save_csv (bool | Unset):
    """

    capture_id: str
    return_data: bool | Unset = UNSET
    save_csv: bool | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        capture_id = self.capture_id

        return_data = self.return_data

        save_csv = self.save_csv

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update(
            {
                "capture_id": capture_id,
            }
        )
        if return_data is not UNSET:
            field_dict["return_data"] = return_data
        if save_csv is not UNSET:
            field_dict["save_csv"] = save_csv

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        capture_id = d.pop("capture_id")

        return_data = d.pop("return_data", UNSET)

        save_csv = d.pop("save_csv", UNSET)

        capture_stop_request = cls(
            capture_id=capture_id,
            return_data=return_data,
            save_csv=save_csv,
        )

        capture_stop_request.additional_properties = d
        return capture_stop_request

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
