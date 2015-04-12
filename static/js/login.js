$(function() {
    var logname = $("#logname");
    logname.val("");
    logname.focus();
    var logpw = $("#logpw");
    var logbut = $("#logbut");
    var logresponse = $("#logresponse");
    logbut.attr("unselectable", "on");

    doLogin = function() {
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
		    logresponse.html(data[0]);
		    if(data[1] == true)	{
			window.location.href = "/";
		    }
		    else {
			logpw.val("");
			logpw.focus()
		    }
		},
		error: function() {
		    console.log("nooo error");
		}
	    });
	}
    };
    registerButton(logbut, doLogin);
    $(document).keypress(function(event) {
	if(event.keyCode == 13) doLogin();
    });
});
