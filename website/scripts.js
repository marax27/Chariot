$("#reportIncident").submit(function(event) {
    event.preventDefault();
    event.returnValue = false;
    
    const Url = "http://localhost:8080/api/server:reportIncident";
    var distance = document.forms["reportIncident"].elements["inputDistance"].value;
    
    $.post(Url, distance, function(data, status) {
        alert(distance + "\n" + data);
    })
    
    document.forms["reportIncident"].elements["inputDistance"].value = "";
});