function appendOutput(cls, text, id) {
    $(id).append('<pre class="' + cls + '">' + text + '</pre>');
    if (id === '#chat-output')
    {
        $(id)[0].scrollTop = $(id)[0].scrollHeight;
    }
    $('#line').focus();
    $('#line')[0].scrollIntoView({block: "end", behavior: "instant"});
}

$(document).ready(function () {
    var port = window.location.port
    var hostName = window.location.hostname;
    var uri = "ws://"+hostName+":"+port+"/play"
    var ws = new WebSocket(uri);

    appendOutput('stderr', 'Opening WebSockets connection...\n', '#console-output');

    ws.onerror = function(event) {
        appendOutput('stderr', 'WebSockets error: ' + event.data + '\n', '#console-output');
    };

    ws.onopen = function() {
        appendOutput('stderr', 'WebSockets connection successful!\n', '#console-output');
    };

    ws.onclose = function() {
        appendOutput('stderr', 'WebSockets connection closed.\n', '#console-output');
    };

    ws.onmessage = function(event) {
        try
        {
            var chatMsg = $.parseJSON(event.data);
            if (chatMsg.msgType === 'chat')
            {
                appendOutput('stdout',chatMsg.text.concat('\n'), '#chat-output');
            }
        }
        catch (e) {
            appendOutput('stdout', event.data, '#console-output');
        }
    };

    $('#console-input').submit(function () {
        var line = $('#line').val();
        ws.send(line + '\n');
        appendOutput('stdin', line + '\n');
        $('#line').val('');
        return false;
    });
});
