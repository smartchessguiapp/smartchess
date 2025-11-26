from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="InverterStatus")


@_attrs_define
class InverterStatus:
    """
    Attributes:
        connected (bool | Unset):
        run_state (str | Unset):
        output_freq_hz (float | Unset):
        output_current_a (float | Unset):
        status_word (int | Unset):
        alarm_code (int | Unset):
        last_update_ms_ago (int | Unset):
    """

    connected: bool | Unset = UNSET
    run_state: str | Unset = UNSET
    output_freq_hz: float | Unset = UNSET
    output_current_a: float | Unset = UNSET
    status_word: int | Unset = UNSET
    alarm_code: int | Unset = UNSET
    last_update_ms_ago: int | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        connected = self.connected

        run_state = self.run_state

        output_freq_hz = self.output_freq_hz

        output_current_a = self.output_current_a

        status_word = self.status_word

        alarm_code = self.alarm_code

        last_update_ms_ago = self.last_update_ms_ago

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if connected is not UNSET:
            field_dict["connected"] = connected
        if run_state is not UNSET:
            field_dict["run_state"] = run_state
        if output_freq_hz is not UNSET:
            field_dict["output_freq_hz"] = output_freq_hz
        if output_current_a is not UNSET:
            field_dict["output_current_a"] = output_current_a
        if status_word is not UNSET:
            field_dict["status_word"] = status_word
        if alarm_code is not UNSET:
            field_dict["alarm_code"] = alarm_code
        if last_update_ms_ago is not UNSET:
            field_dict["last_update_ms_ago"] = last_update_ms_ago

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        connected = d.pop("connected", UNSET)

        run_state = d.pop("run_state", UNSET)

        output_freq_hz = d.pop("output_freq_hz", UNSET)

        output_current_a = d.pop("output_current_a", UNSET)

        status_word = d.pop("status_word", UNSET)

        alarm_code = d.pop("alarm_code", UNSET)

        last_update_ms_ago = d.pop("last_update_ms_ago", UNSET)

        inverter_status = cls(
            connected=connected,
            run_state=run_state,
            output_freq_hz=output_freq_hz,
            output_current_a=output_current_a,
            status_word=status_word,
            alarm_code=alarm_code,
            last_update_ms_ago=last_update_ms_ago,
        )

        inverter_status.additional_properties = d
        return inverter_status

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
