"""Raspberry Pi specific host_client package.

Place Raspberry Pi / HIL-specific modules here (HAL bindings, wiringPi wrappers,
AD/DA drivers, deployment helpers). This is a placeholder so humans can clearly
see the separation between PC GUI code (under `gui.pc`) and Pi code.

API guidance (short):
- Provide a thin adapter module (e.g. `hal_adapter.py`) exposing the same
  high-level functions the PC mock server provides (status updates, io control)
- Keep REST/WS server code that runs on the Pi under this package.
"""

__all__ = []
