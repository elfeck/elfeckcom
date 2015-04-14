$(function() {
    tit = $("#edittitle");
    cat = $("#editcategories");
    cont = $("#editarea");
    type = $("#edittype");
    access = $("#editaccess");

    preview = $("#editpreview");
    editList = $("#editlist");
    submitBut = $("#submitbutton");

    editdel = $("#editdelete");

    editid = $("#editid");
    editdc = $("#editdc");

    editresp = $("#editresp");

    registerButton(submitBut, submit);
    tit.on("keyup", previewSubmit);
    cat.on("keyup", previewSubmit);
    cont.on("keyup", previewSubmit);
    cont.on("change", previewSubmit);
    editList.on("change", loadPost);
    editdel.on("keyup", function() {
	console.log(editdel.val());
	if(editdel.val() == "DEL") {
	    editdel.addClass("editdeldanger");
	} else {
	    editdel.removeClass("editdeldanger");
	}
    });
});

previewSubmit = function() {
    var dataObj = {
	title: tit.val(),
	categories: cat.val(),
	content: cont.val(),
	ptype: type.val(),
	access: access.val()
    };
    $.ajax({
	type: "POST",
	url: "/edit/preview",
	dataType: "json",
	data: {
	    dat: dataObj
	},
	success: function(data) {
	    preview.html(data);
	},
	error: function() {
	    console.log("nooo error");
	}
    });
};

submit = function() {
    if(!validateMinimal()) {
	console.log("validateNew not successful");
	return;
    }
    selId = getSelectedId();
    t = -1;
    if(selId == 0) t = 0;
    if(selId != 0) t = 1;
    if(selId != 0 && editdel.val() == "DEL") t = 2;
    console.log(t);
    var dataObj = {
	// t=0 create
	// t=1 update
	// t=2 delete
	submitType: t,
	pid: selId,
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
	    editresp.html(data);
	    editdel.val("");
	},
	error: function() {
	    console.log("nooo error");
	}
    });
}

loadPost = function() {
    var selId = getSelectedId();
    if(selId == 0) {
	return;
    }
    var dataObj = {
	id: selId
    }
    $.ajax({
	type: "POST",
	url: "/edit/loadpost",
	dataType: "json",
	data: {
	    dat: dataObj
	},
	success: function(data) {
	    editid.html(data[0]);
	    editdc.html(procTime(data[1]["crtDate"]));
	    cont.html(data[1]["content"]);
	    tit.val(procTitle(data[1]["title"]));
	    cat.val(procCat(data[1]["categories"]));
	    type.val(data[1]["ptype"]);
	    access.val(data[1]["access"]);
	    cont.change();
	},
	error: function() {
	    console.log("nooo error in loadPost json");
	}
    });
}

getSelectedId = function() {
    var selected = editList.find("option:selected");
    return selected.attr("id");
}

validateMinimal = function() {
    return cont.val() != "" && isInt(type.val()) && isInt(access.val());
}

isInt = function(value) {
    return !isNaN(value) &&
	parseInt(Number(value)) == value &&
	!isNaN(parseInt(value, 10));
}

procTime = function(time) {
    return time.replace("T", " ").substring(0, 16);
}

procTitle = function(title) {
    if(title == null) return "";
    else return title;
}

procCat = function(cats) {
    if(cats == null) return "";
    else return cats.join(", ");
}
