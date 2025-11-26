from __future__ import annotations

from collections.abc import Mapping
from typing import TYPE_CHECKING, Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

if TYPE_CHECKING:
    from ..models.dac_run_response_data import DACRunResponseData


T = TypeVar("T", bound="DACRunResponse")


@_attrs_define
class DACRunResponse:
    """
    Attributes:
        ok (bool | Unset):  Example: True.
        data (DACRunResponseData | Unset):
    """

    ok: bool | Unset = UNSET
    data: DACRunResponseData | Unset = UNSET
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
        from ..models.dac_run_response_data import DACRunResponseData

        d = dict(src_dict)
        ok = d.pop("ok", UNSET)

        _data = d.pop("data", UNSET)
        data: DACRunResponseData | Unset
        if isinstance(_data, Unset):
            data = UNSET
        else:
            data = DACRunResponseData.from_dict(_data)

        dac_run_response = cls(
            ok=ok,
            data=data,
        )

        dac_run_response.additional_properties = d
        return dac_run_response

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
