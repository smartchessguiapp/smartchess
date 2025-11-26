from __future__ import annotations

from collections.abc import Mapping
from typing import Any, TypeVar, cast

from attrs import define as _attrs_define
from attrs import field as _attrs_field

from ..types import UNSET, Unset

T = TypeVar("T", bound="CaptureDataResponseData")


@_attrs_define
class CaptureDataResponseData:
    """
    Attributes:
        capture_id (str | Unset):
        sample_rate_hz (float | Unset):
        columns (list[str] | Unset):
        rows (list[list[float]] | Unset):
        csv_path (str | Unset):
        download_url (str | Unset):
    """

    capture_id: str | Unset = UNSET
    sample_rate_hz: float | Unset = UNSET
    columns: list[str] | Unset = UNSET
    rows: list[list[float]] | Unset = UNSET
    csv_path: str | Unset = UNSET
    download_url: str | Unset = UNSET
    additional_properties: dict[str, Any] = _attrs_field(init=False, factory=dict)

    def to_dict(self) -> dict[str, Any]:
        capture_id = self.capture_id

        sample_rate_hz = self.sample_rate_hz

        columns: list[str] | Unset = UNSET
        if not isinstance(self.columns, Unset):
            columns = self.columns

        rows: list[list[float]] | Unset = UNSET
        if not isinstance(self.rows, Unset):
            rows = []
            for rows_item_data in self.rows:
                rows_item = rows_item_data

                rows.append(rows_item)

        csv_path = self.csv_path

        download_url = self.download_url

        field_dict: dict[str, Any] = {}
        field_dict.update(self.additional_properties)
        field_dict.update({})
        if capture_id is not UNSET:
            field_dict["capture_id"] = capture_id
        if sample_rate_hz is not UNSET:
            field_dict["sample_rate_hz"] = sample_rate_hz
        if columns is not UNSET:
            field_dict["columns"] = columns
        if rows is not UNSET:
            field_dict["rows"] = rows
        if csv_path is not UNSET:
            field_dict["csv_path"] = csv_path
        if download_url is not UNSET:
            field_dict["download_url"] = download_url

        return field_dict

    @classmethod
    def from_dict(cls: type[T], src_dict: Mapping[str, Any]) -> T:
        d = dict(src_dict)
        capture_id = d.pop("capture_id", UNSET)

        sample_rate_hz = d.pop("sample_rate_hz", UNSET)

        columns = cast(list[str], d.pop("columns", UNSET))

        _rows = d.pop("rows", UNSET)
        rows: list[list[float]] | Unset = UNSET
        if _rows is not UNSET:
            rows = []
            for rows_item_data in _rows:
                rows_item = cast(list[float], rows_item_data)

                rows.append(rows_item)

        csv_path = d.pop("csv_path", UNSET)

        download_url = d.pop("download_url", UNSET)

        capture_data_response_data = cls(
            capture_id=capture_id,
            sample_rate_hz=sample_rate_hz,
            columns=columns,
            rows=rows,
            csv_path=csv_path,
            download_url=download_url,
        )

        capture_data_response_data.additional_properties = d
        return capture_data_response_data

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
