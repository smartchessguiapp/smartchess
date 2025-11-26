from __future__ import annotations

from collections.abc import Mapping
from typing import TYPE_CHECKING, Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

if TYPE_CHECKING:
    from ..models.dac_status import DACStatus


T = TypeVar("T", bound="DACStatusWrapperData")


@_attrs_define
class DACStatusWrapperData:
    """
    Attributes:
        dac (DACStatus | Unset):
    """

    dac: DACStatus | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        dac: dict[str, Any] | Unset = UNSET
        if not isinstance(self.dac, Unset):
            dac = self.dac.to_dict()

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if dac is not UNSET:
            field_dict["dac"] = dac

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        from ..models.dac_status import DACStatus

        d = dict(src_dict)
        _dac = d.pop("dac", UNSET)
        dac: DACStatus | Unset
        if isinstance(_dac, Unset):
            dac = UNSET
        else:
            dac = DACStatus.from_dict(_dac)

        dac_status_wrapper_data = cls(
            dac=dac,
        )

        dac_status_wrapper_data.additional_properties = d
        return dac_status_wrapper_data

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
