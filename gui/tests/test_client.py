import json
import responses
import pytest
from host_client.client import SmartCheckerClient

BASE = "http://127.0.0.1:8000/api/v1"

@responses.activate
def test_modbus_read():
    client = SmartCheckerClient(BASE)
    responses.add(responses.POST, f"{BASE}/modbus/read",
                  json={"request_id": "r1", "address": 40001, "quantity": 1, "values": [1234], "raw": "0x01"}, status=200)
    res = client.modbus_read(40001)
    assert res["request_id"] == "r1"
    assert res["values"][0] == 1234

@responses.activate
def test_get_di():
    client = SmartCheckerClient(BASE)
    responses.add(responses.GET, f"{BASE}/di",
                  json={"ok": True, "data": {"di_byte": 13, "pending_di_byte": 13}}, status=200)
    res = client.get_di()
    assert res["ok"] is True
    assert res["data"]["di_byte"] == 13
