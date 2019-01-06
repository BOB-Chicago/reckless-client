// Create a function that can be used to send websocket messages 
const getWebSocketSend = config => {
	const ws = new WebSocket(config.url);
	ws.onopen = config.on_open;
	ws.onclose = config.on_close;
	ws.onmessage = config.on_message;
	send = ws.send.bind(ws);
	return send;
}
