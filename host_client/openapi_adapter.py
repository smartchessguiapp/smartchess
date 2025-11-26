"""
Compatibility adapter that wraps the generated OpenAPI client and provides
simple dict-based methods similar to the original minimal `SmartCheckerClient`.

This file provides a small migration surface so existing GUI code can call
into typed generated client functions while receiving familiar dict payloads.
"""
from __future__ import annotations

from typing import Any, Dict, Optional

from .openapi_client.smart_checker_host_raspberry_pi_api_client.client import Client as APIClient
from .openapi_client.smart_checker_host_raspberry_pi_api_client.api.default import (
    get_di as api_get_di,
    put_di_pending as api_put_di_pending,
    post_di_start as api_post_di_start,
    post_di_stop as api_post_di_stop,
    get_dac_status as api_get_dac_status,
)
from .openapi_client.smart_checker_host_raspberry_pi_api_client.models import (
    DISetPendingRequest,
    DIStartRequest,
    PostDiStopBody,
)


class OpenAPIAdapter:
    """Adapter that exposes a small set of sync methods returning plain dicts.

    Example:
        adapter = OpenAPIAdapter(base_url="http://192.168.50.2:8000/api/v1")
        di = adapter.get_di()
    """

    def __init__(self, base_url: str, timeout: Optional[float] = None):
        # pass base_url into generated client
        self.client = APIClient(base_url=base_url)
        if timeout is not None:
            # runtime: set httpx timeout via underlying client if needed
            try:
                from httpx import Timeout

                self.client = self.client.with_timeout(Timeout(timeout))
            except Exception:
                pass

    def get_di(self) -> Optional[Dict[str, Any]]:
        res = api_get_di.sync(client=self.client)
        if res is None:
            return None
        # generated model exposes to_dict()
        return res.to_dict()

    def put_di_pending(self, di_byte: int) -> Optional[Dict[str, Any]]:
        body = DISetPendingRequest(di_byte=di_byte)
        res = api_put_di_pending.sync(client=self.client, body=body)
        return res.to_dict() if res is not None else None

    def start_di(self, di_byte: Optional[int] = None) -> Optional[Dict[str, Any]]:
        if di_byte is None:
            body = DIStartRequest()
        else:
            body = DIStartRequest(di_byte=di_byte)
        res = api_post_di_start.sync(client=self.client, body=body)
        return res.to_dict() if res is not None else None

    def stop_di(self) -> Optional[Dict[str, Any]]:
        body = PostDiStopBody()
        res = api_post_di_stop.sync(client=self.client, body=body)
        return res.to_dict() if res is not None else None

    def get_dac_status(self) -> Optional[Dict[str, Any]]:
        res = api_get_dac_status.sync(client=self.client)
        return res.to_dict() if res is not None else None
