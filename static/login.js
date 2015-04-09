$(function() {
    var logname = $("#logname");
    var logpw = $("#logpw");
    var logbut = $("#logbut");
    var logresponse = $("#logresponse");
    logbut.attr("unselectable", "on");

    function doLogin() {
	if(logpw.val() == "" && logname.val() == "") {
	    logresponse.html("tell me your login info");
	} else if(logname.val() == "") {
	    logresponse.html("tell me your name");
	} else if(logpw.val() == "") {
	    logresponse.html("tell me your password (shh)");
	} else {
	    logresponse.html("trying to log you in");
	    var dataObj = {
		name: logname.val(),
		pass: logpw.val()
	    };
	    $.ajax({
		type: "POST",
		url: "/login/submit",
		dataType: "json",
		data: {
		    dat: dataObj
		},
		success: function(data) {
		    logresponse.html(data);
		},
		error: function() {
		    console.log("nooo error");
		}
	    });
	}
    };

    logbut.on("mouseenter", function() {
	logbut.removeClass("logbutidle");
	logbut.addClass("logbuthover");
    });
    logbut.on("mouseout", function() {
	logbut.removeClass("logbutpress");
	logbut.removeClass("logbuthover");
	logbut.addClass("logbutidle");
    });
    logbut.on("mousedown", function() {
	logbut.removeClass("logbuthover");
	logbut.addClass("logbutpress");
    });
    logbut.on("mouseup", function() {
	logbut.removeClass("logbutpress");
	logbut.addClass("logbuthover");
	doLogin();
    });
    $(document).keypress(function(event) {
	if(event.keyCode == 13) doLogin();
    });
});
