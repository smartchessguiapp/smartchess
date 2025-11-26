from __future__ import annotations

from collections.abc import Mapping
from typing import TYPE_CHECKING, Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

if TYPE_CHECKING:
    from ..models.dac_command_response_data import DACCommandResponseData


T = TypeVar("T", bound="DACCommandResponse")


@_attrs_define
class DACCommandResponse:
    """
    Attributes:
        ok (bool | Unset):  Example: True.
        data (DACCommandResponseData | Unset):
    """

    ok: bool | Unset = UNSET
    data: DACCommandResponseData | Unset = UNSET
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
        from ..models.dac_command_response_data import DACCommandResponseData

        d = dict(src_dict)
        ok = d.pop("ok", UNSET)

        _data = d.pop("data", UNSET)
        data: DACCommandResponseData | Unset
        if isinstance(_data, Unset):
            data = UNSET
        else:
            data = DACCommandResponseData.from_dict(_data)

        dac_command_response = cls(
            ok=ok,
            data=data,
        )

        dac_command_response.additional_properties = d
        return dac_command_response

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
