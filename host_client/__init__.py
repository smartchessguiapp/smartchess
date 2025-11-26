"""host_client package

Exports compatibility adapter and the original minimal client.
"""
from .client import SmartCheckerClient
from .openapi_adapter import OpenAPIAdapter

__all__ = ["SmartCheckerClient", "OpenAPIAdapter"]
__version__ = "0.1.0"
