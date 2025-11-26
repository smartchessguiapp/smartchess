"""SmartChecker Host client (minimal)
Provides a thin REST + WebSocket wrapper to talk to Raspberry Pi HIL Service.
"""
import json
import logging
import threading
from typing import Any, Dict, Optional

import requests
import websocket

logger = logging.getLogger(__name__)


class SmartCheckerClient:
    """Minimal client for Host <-> RPi HIL Service communication.

    Usage:
        client = SmartCheckerClient(base_url="http://192.168.50.2:8000/api/v1")
        client.modbus_read(40001, quantity=1)
    """

    def __init__(self, base_url: str, ws_url: Optional[str] = None, timeout: float = 5.0):
        self.base_url = base_url.rstrip("/")
        self.ws_url = ws_url
        self.timeout = timeout
        self._ws = None  # type: Optional[websocket.WebSocketApp]
        self._ws_thread = None  # type: Optional[threading.Thread]
        self._ws_callbacks = {}

    # --- REST helpers ---
    def _url(self, path: str) -> str:
        return f"{self.base_url}{path}"

    def _post(self, path: str, payload: Dict[str, Any]) -> Dict[str, Any]:
        url = self._url(path)
        logger.debug("POST %s %s", url, payload)
        r = requests.post(url, json=payload, timeout=self.timeout)
        r.raise_for_status()
        return r.json()

    def _get(self, path: str, params: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        url = self._url(path)
        logger.debug("GET %s %s", url, params)
        r = requests.get(url, params=params or {}, timeout=self.timeout)
        r.raise_for_status()
        return r.json()

    # --- API methods (subset) ---
    def modbus_read(self, address: int, quantity: int = 1, unit_id: int = 1, fmt: str = "uint16") -> Dict[str, Any]:
        payload = {"address": address, "quantity": quantity, "unit_id": unit_id, "format": fmt}
        return self._post("/modbus/read", payload)

    def modbus_write(self, address: int, value: int, unit_id: int = 1, fmt: str = "uint16") -> Dict[str, Any]:
        payload = {"address": address, "value": value, "unit_id": unit_id, "format": fmt}
        return self._post("/modbus/write", payload)

    def get_di(self) -> Dict[str, Any]:
        return self._get("/di")

    def put_di_pending(self, di_byte: int) -> Dict[str, Any]:
        return self._post("/di/pending", {"di_byte": di_byte})

    def start_di(self, di_byte: Optional[int] = None) -> Dict[str, Any]:
        payload = {"di_byte": di_byte} if di_byte is not None else {}
        return self._post("/di/start", payload)

    def stop_di(self) -> Dict[str, Any]:
        return self._post("/di/stop", {})

    def get_dac_status(self) -> Dict[str, Any]:
        return self._get("/dac/status")

    def put_dac_command(self, freq_hz: float, acc_s: float) -> Dict[str, Any]:
        return self._post("/dac/command", {"freq_hz": freq_hz, "acc_s": acc_s})

    def post_dac_run(self, freq_hz: Optional[float] = None, acc_s: Optional[float] = None) -> Dict[str, Any]:
        payload = {}
        if freq_hz is not None:
            payload["freq_hz"] = freq_hz
        if acc_s is not None:
            payload["acc_s"] = acc_s
        return self._post("/dac/run", payload)

    def post_dac_stop(self, acc_s: Optional[float] = None) -> Dict[str, Any]:
        payload = {"acc_s": acc_s} if acc_s is not None else {}
        return self._post("/dac/stop", payload)

    def capture_start(self, sample_rate_hz: Optional[float] = None, max_duration_s: Optional[float] = None) -> Dict[str, Any]:
        payload = {}
        if sample_rate_hz is not None:
            payload["sample_rate_hz"] = sample_rate_hz
        if max_duration_s is not None:
            payload["max_duration_s"] = max_duration_s
        return self._post("/capture/start", payload)

    def capture_stop(self, capture_id: str, return_data: bool = True, save_csv: bool = True) -> Dict[str, Any]:
        payload = {"capture_id": capture_id, "return_data": return_data, "save_csv": save_csv}
        return self._post("/capture/stop", payload)

    # --- WebSocket ---
    def ws_on_message(self, ws, message):
        try:
            obj = json.loads(message)
        except Exception:
            logger.exception("Invalid WS message: %s", message)
            return
        etype = obj.get("type")
        if etype and etype in self._ws_callbacks:
            try:
                self._ws_callbacks[etype](obj)
            except Exception:
                logger.exception("WS callback failed for %s", etype)

    def ws_on_error(self, ws, error):
        logger.error("WebSocket error: %s", error)

    def ws_on_close(self, ws, close_status_code, close_msg):
        logger.info("WebSocket closed: %s %s", close_status_code, close_msg)

    def ws_on_open(self, ws):
        logger.info("WebSocket opened")

    def register_ws_callback(self, event_type: str, fn):
        self._ws_callbacks[event_type] = fn

    def ws_connect(self, ws_url: Optional[str] = None):
        url = ws_url or self.ws_url
        if not url:
            raise ValueError("WebSocket URL not configured")
        self._ws = websocket.WebSocketApp(url,
                                         on_message=self.ws_on_message,
                                         on_error=self.ws_on_error,
                                         on_close=self.ws_on_close,
                                         on_open=self.ws_on_open)
        self._ws_thread = threading.Thread(target=self._ws.run_forever, daemon=True)
        self._ws_thread.start()

    def ws_close(self):
        if self._ws:
            self._ws.close()
        if self._ws_thread:
            self._ws_thread.join(timeout=1.0)


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    c = SmartCheckerClient("http://127.0.0.1:8000/api/v1")
    try:
        print("Health:", c._get("/health"))
    except Exception as e:
        logger.error("Example run failed: %s", e)
