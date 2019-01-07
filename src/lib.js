const types = require("./bridge.bs");

// Create a function that can be used to send websocket messages 
const getWebSocketSend = config_ => {
	const config = websocket_configToJs(config_);
	const ws = new WebSocket(config.url);
	ws.onopen = config.on_open;
	ws.onclose = config.on_close;
	ws.onmessage = config.on_message;
	send = ws.send.bind(ws);
	return send;
}


// We rely on the browser's event bus
const makeEventBus = () => {
	const register = f => {
		const h = e => f(e.payload);
		document.addEventListener("appEvent", h);
	}

	const emit = payload => {
		const ev = new Event("appEvent");
		ev.payload = payload;
		document.dispatchEvent(ev);
	}
	
	return { register, emit };
}


// Concatenate Uint8Arrays
const uint8ArrayConcat = xs => {
	if (xs.length === 0) {
		return new Uint8Array(0);
	} else if (xs.length === 1) {
		return xs[0];
	} else {
		const as = xs.map(arr => Array.from(arr));
		return new Uint8Array(
			Array.prototype.concat.apply(as[0], as.slice(1))
		);
	}

}

// Encode a string into a Uint8Array
const encoder = new TextEncoder();
const encode = encoder.encode.bind(encoder); 


module.exports = {
	getWebSocketSend: getWebSocketSend,
	makeEventBus: makeEventBus,
	uint8ArrayConcat: uint8ArrayConcat,
	encode: encode
}

