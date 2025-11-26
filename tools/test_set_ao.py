# tools/test_set_ao.py
# Simple script: connect to mock_server websocket, POST AO1, and print incoming broadcasts
import time
import json
import socket
import requests

try:
    from websocket import create_connection
    WS_CLIENT = 'websocket-client'
except Exception:
    WS_CLIENT = None

url_ws = 'ws://127.0.0.1:8000/ws/v1'
url_post = 'http://127.0.0.1:8000/api/v1/io/ao'
payload = {'channel':'ao1','value':4.20,'mode':'0-10V'}

print('Using ws client:', WS_CLIENT)

ws = None
if WS_CLIENT:
    try:
        print('Connecting to', url_ws)
        ws = create_connection(url_ws, timeout=5)
        # ensure recv() does not block indefinitely; use small timeout and respect overall deadline
        try:
            ws.settimeout(1)
        except Exception:
            # some websocket implementations may not support settimeout; ignore
            pass
        print('WS connected')
    except Exception as e:
        print('WS connect failed:', e)
        ws = None

# Send POST
try:
    r = requests.post(url_post, json=payload, timeout=3)
    print('POST', r.status_code, r.text)
except Exception as e:
    print('POST failed:', e)
    # ensure we still capture result to log file

# If ws connected, read up to 6 messages or 8 seconds total
if ws:
    deadline = time.time() + 8
    try:
        while time.time() < deadline:
            try:
                msg = ws.recv()
            except Exception as e:
                # timeout is expected while waiting for broadcasts; continue until overall deadline
                if isinstance(e, socket.timeout) or 'timed out' in str(e).lower():
                    # no message this interval
                    #print('WS recv timeout')
                    continue
                print('WS recv error:', e)
                break
            if not msg:
                break
            print('WS message:', msg)
            try:
                j = json.loads(msg)
                # search for ao1/ao2 keys anywhere in message
                if 'ao1' in msg or 'ao2' in msg or (isinstance(j, dict) and ('ao1' in j or 'ao2' in j)):
                    print('>>> Found ao in message')
                    break
            except Exception:
                pass
    finally:
        try:
            ws.close()
        except Exception:
            pass
        print('WS closed')
        # write simple run-log for CI / inspection
        try:
            with open('tools/test_set_ao.log', 'a', encoding='utf-8') as f:
                f.write(time.strftime('[%Y-%m-%d %H:%M:%S]'))
                f.write(' POST ' + str(payload) + ' -> ' + (str(r.status_code) + ' ' + (r.text if 'r' in locals() else '')) + '\n')
        except Exception:
            pass
else:
    print('No WS client available to listen for broadcasts; rely on POST response.')
    try:
        with open('tools/test_set_ao.log', 'a', encoding='utf-8') as f:
            f.write(time.strftime('[%Y-%m-%d %H:%M:%S]'))
            f.write(' POST ' + str(payload) + ' -> ' + (str(r.status_code) + ' ' + (r.text if 'r' in locals() else '')) + '\n')
    except Exception:
        pass
