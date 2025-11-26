from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="DACStatus")


@_attrs_define
class DACStatus:
    """
    Attributes:
        cur_freq (float | Unset):
        target_freq (float | Unset):
        cmd_freq (float | Unset):
        acc_time (float | Unset):
        vf_enable (bool | Unset):
        vf_fbase (float | Unset):
        vf_vbase (float | Unset):
        vf_vboost (float | Unset):
        vmax_applied (float | Unset):
        output_enabled (bool | Unset):
    """

    cur_freq: float | Unset = UNSET
    target_freq: float | Unset = UNSET
    cmd_freq: float | Unset = UNSET
    acc_time: float | Unset = UNSET
    vf_enable: bool | Unset = UNSET
    vf_fbase: float | Unset = UNSET
    vf_vbase: float | Unset = UNSET
    vf_vboost: float | Unset = UNSET
    vmax_applied: float | Unset = UNSET
    output_enabled: bool | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        cur_freq = self.cur_freq

        target_freq = self.target_freq

        cmd_freq = self.cmd_freq

        acc_time = self.acc_time

        vf_enable = self.vf_enable

        vf_fbase = self.vf_fbase

        vf_vbase = self.vf_vbase

        vf_vboost = self.vf_vboost

        vmax_applied = self.vmax_applied

        output_enabled = self.output_enabled

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if cur_freq is not UNSET:
            field_dict["cur_freq"] = cur_freq
        if target_freq is not UNSET:
            field_dict["target_freq"] = target_freq
        if cmd_freq is not UNSET:
            field_dict["cmd_freq"] = cmd_freq
        if acc_time is not UNSET:
            field_dict["acc_time"] = acc_time
        if vf_enable is not UNSET:
            field_dict["vf_enable"] = vf_enable
        if vf_fbase is not UNSET:
            field_dict["vf_fbase"] = vf_fbase
        if vf_vbase is not UNSET:
            field_dict["vf_vbase"] = vf_vbase
        if vf_vboost is not UNSET:
            field_dict["vf_vboost"] = vf_vboost
        if vmax_applied is not UNSET:
            field_dict["vmax_applied"] = vmax_applied
        if output_enabled is not UNSET:
            field_dict["output_enabled"] = output_enabled

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        cur_freq = d.pop("cur_freq", UNSET)

        target_freq = d.pop("target_freq", UNSET)

        cmd_freq = d.pop("cmd_freq", UNSET)

        acc_time = d.pop("acc_time", UNSET)

        vf_enable = d.pop("vf_enable", UNSET)

        vf_fbase = d.pop("vf_fbase", UNSET)

        vf_vbase = d.pop("vf_vbase", UNSET)

        vf_vboost = d.pop("vf_vboost", UNSET)

        vmax_applied = d.pop("vmax_applied", UNSET)

        output_enabled = d.pop("output_enabled", UNSET)

        dac_status = cls(
            cur_freq=cur_freq,
            target_freq=target_freq,
            cmd_freq=cmd_freq,
            acc_time=acc_time,
            vf_enable=vf_enable,
            vf_fbase=vf_fbase,
            vf_vbase=vf_vbase,
            vf_vboost=vf_vboost,
            vmax_applied=vmax_applied,
            output_enabled=output_enabled,
        )

        dac_status.additional_properties = d
        return dac_status

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
