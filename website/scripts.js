$("#reportIncident").submit(function(event) {
    event.preventDefault();
    event.returnValue = false;
    
    const Url = "http://localhost:8080/api/server:reportIncident";
    var distance = document.forms["reportIncident"].elements["inputDistance"].value;
    
    $.post(Url, distance, function(data, status) {
        
    });
    
    document.forms["reportIncident"].elements["inputDistance"].value = "";
});

function GetFireTrucks() {
    const Url = "http://localhost:8080/api/server:getFireTrucks";

    $.get(Url, function(data, Status) {
        if(data) {
            const firetrucks = JSON.parse(data);
            LogMessage(firetrucks);
        }
    });

    setTimeout(GetFireTrucks, 1000);
}

function LogMessage(obj) {
    const newLog = `${CurrentTime()}: ${JSON.stringify(obj)}`;
    const previousText = document.getElementById("actionLogs").value;
    document.getElementById("actionLogs").value = `${newLog}\n${previousText}`;
}

function CurrentTime() {
    const now = new Date();
    const h = String(now.getHours()).padStart(2, '0');
    const m = String(now.getMinutes()).padStart(2, '0');
    const s = String(now.getSeconds()).padStart(2, '0');
    const ms = String(now.getMilliseconds()).padStart(3, '0');
    return `${h}:${m}:${s}.${ms}`;
}

GetFireTrucks();