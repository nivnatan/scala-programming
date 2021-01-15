var message = document.getElementById("message");
var button = document.getElementById("send");
var username = document.getElementById("username");
var output = document.getElementById("output");

ws = new WebSocket("ws://localhost:8080/chat?room=2&name=3");

button.addEventListener("click", function () {
  ws.send(message.value);
});

ws.onmessage = function(event) {
    output.innerHTML +=
        "<p><strong>" + username.value + ":: </strong>" + event.data + "</p>";
}