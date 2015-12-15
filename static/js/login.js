$(function() {
  username = $("#lUsername");
  username.val("");

  submitbutton = $("#lSubmitbutton");
  submitbutton.attr("unselectable", "on");
  registerButton(submitbutton, doLogin);

  pw = $("#lPw");
  responsefield = $("#lResponsefield");

  $(document).keypress(function(event) {
    if(event.keyCode == 13) doLogin();
  });

  username.focus();
});

doLogin = function() {
  if(pw.val() == "" && username.val() == "") {
    responsefield.html("enter login info");
  } else if(username.val() == "") {
    responsefield.html("enter userusername");
  } else if(pw.val() == "") {
    responsefield.html("enter password");
  } else {
    responsefield.html("trying to log you in");
    var dataObj = {
      name: username.val(),
      pass: pw.val()
    };
    $.ajax({
      type: "POST",
      url: "/login/submit",
      dataType: "json",
      data: dataObj,
      success: function(resp) {
	responsefield.html(resp);
	if(resp[1] == true)	{
	  window.location.href = "/";
	}
	else {
	  pw.val("");
	  pw.focus()
	}
      },
      error: function() {
	responsefield.html("json error");
	console.log("json error in login.js");
      }
    });
  }
};
