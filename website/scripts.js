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
            var previousText = document.getElementById("actionLogs").value;
            document.getElementById("actionLogs").value = data + "\r\n" + previousText;
        }
    });

    setTimeout(GetActionInfo, 1000);
}

GetActionInfo();