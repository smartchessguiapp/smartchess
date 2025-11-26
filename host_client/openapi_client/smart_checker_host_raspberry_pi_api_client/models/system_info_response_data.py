from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="SystemInfoResponseData")


@_attrs_define
class SystemInfoResponseData:
    """
    Attributes:
        hostname (str | Unset):
        ip_address (str | Unset):
        model (str | Unset):
        fs_hz (int | Unset):
        capture_hz (int | Unset):
        lut_size (int | Unset):
    """

    hostname: str | Unset = UNSET
    ip_address: str | Unset = UNSET
    model: str | Unset = UNSET
    fs_hz: int | Unset = UNSET
    capture_hz: int | Unset = UNSET
    lut_size: int | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        hostname = self.hostname

        ip_address = self.ip_address

        model = self.model

        fs_hz = self.fs_hz

        capture_hz = self.capture_hz

        lut_size = self.lut_size

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if hostname is not UNSET:
            field_dict["hostname"] = hostname
        if ip_address is not UNSET:
            field_dict["ip_address"] = ip_address
        if model is not UNSET:
            field_dict["model"] = model
        if fs_hz is not UNSET:
            field_dict["fs_hz"] = fs_hz
        if capture_hz is not UNSET:
            field_dict["capture_hz"] = capture_hz
        if lut_size is not UNSET:
            field_dict["lut_size"] = lut_size

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        hostname = d.pop("hostname", UNSET)

        ip_address = d.pop("ip_address", UNSET)

        model = d.pop("model", UNSET)

        fs_hz = d.pop("fs_hz", UNSET)

        capture_hz = d.pop("capture_hz", UNSET)

        lut_size = d.pop("lut_size", UNSET)

        system_info_response_data = cls(
            hostname=hostname,
            ip_address=ip_address,
            model=model,
            fs_hz=fs_hz,
            capture_hz=capture_hz,
            lut_size=lut_size,
        )

        system_info_response_data.additional_properties = d
        return system_info_response_data

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
