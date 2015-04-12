$(function() {
    tit = $("#edittitle");
    cat = $("#editcategories");
    cont = $("#editarea");
    type = $("#edittype");
    access = $("#editaccess");

    preview = $("#editpreview");
    submitBut = $("#submitbutton");

    registerButton(submitBut, submit);
    tit.on("keyup", submitEdit);
    cat.on("keyup", submitEdit);
    cont.on("keyup", submitEdit);
});

submitEdit = function() {
    var dataObj = {
	title: tit.val(),
	categories: cat.val(),
	content: cont.val()
    };
    $.ajax({
	type: "POST",
	url: "/edit/preview",
	dataType: "json",
	data: {
	    dat: dataObj
	},
	success: function(data) {
	    console.log("Got json!");
	    preview.html(data);
	},
	error: function() {
	    console.log("nooo error");
	}
    });
};

submit = function() {
    if(!validateNew()) {
	console.log("validateNew not successful");
	return;
    }
    var dataObj = {
	title: tit.val(),
	categories: cat.val(),
	content: cont.val(),
	type: type.val(),
	access: access.val()
    }
    $.ajax({
	type: "POST",
	url: "/edit/submit",
	dataType: "json",
	data: {
	    dat: dataObj
	},
	success: function(data) {
	    console.log("Got json!");
	    console.log(data);
	},
	error: function() {
	    console.log("nooo error");
	}
    });
}

validateNew = function() {
    return cont.val() != "" && type.val() != "" && access.val() != ""
}
