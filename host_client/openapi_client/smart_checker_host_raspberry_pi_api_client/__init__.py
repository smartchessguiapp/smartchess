"""A client library for accessing SmartChecker Host ⇄ Raspberry Pi API"""

from .client import AuthenticatedClient, Client

__all__ = (
    "AuthenticatedClient",
    "Client",
)
