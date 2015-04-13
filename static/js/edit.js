$(function() {
    tit = $("#edittitle");
    cat = $("#editcategories");
    cont = $("#editarea");
    type = $("#edittype");
    access = $("#editaccess");

    preview = $("#editpreview");
    editList = $("#editlist");
    submitBut = $("#submitbutton");

    registerButton(submitBut, submit);
    tit.on("keyup", submitEdit);
    cat.on("keyup", submitEdit);
    cont.on("keyup", submitEdit);
    editList.on("change", loadPost);
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

loadPost = function(e) {
    var selected = $(this).find("option:selected");
    var dataObj = {
	id: selected.attr("id")
    }
    if(dataObj["id"] == 0) {
	return;
    }
    $.ajax({
	type: "POST",
	url: "/edit/loadpost",
	dataType: "json",
	data: {
	    dat: dataObj
	},
	success: function(data) {
	    console.log(data);
	},
	error: function() {
	    console.log("nooo error in loadPost json");
	}
    });
}

validateNew = function() {
    return cont.val() != "" && type.val() != "" && access.val() != ""
}
