$("#reportIncident").submit(function(event) {
    event.preventDefault();
    event.returnValue = false;
    
    const Url = "http://localhost:8080/api/server:reportIncident";
    var distance = document.forms["reportIncident"].elements["inputDistance"].value;
    
    $.post(Url, distance, function(data, status) {
        
    });
    
    document.forms["reportIncident"].elements["inputDistance"].value = "";
});

function GetActionInfo() {
    const Url = "http://localhost:8080/api/server:getActionInfo";

    $.get(Url, function(data, Status) {
        if(data) {
            const lines = JSON.parse(data);

            lines.forEach(element => {
                var previousText = document.getElementById("actionLogs").value;
                const newLog = `${CurrentTime()}: ${element}`;
                document.getElementById("actionLogs").value = `${newLog}\n${previousText}`;
            });
        }
    });

    setTimeout(GetActionInfo, 1000);
}

function CurrentTime() {
    const now = new Date();
    const h = String(now.getHours()).padStart(2, '0');
    const m = String(now.getMinutes()).padStart(2, '0');
    const s = String(now.getSeconds()).padStart(2, '0');
    const ms = String(now.getMilliseconds()).padStart(3, '0');
    return `${h}:${m}:${s}.${ms}`;
}

GetActionInfo();