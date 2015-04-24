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

    clearAll();
    editList.focus();

    registerButton(submitBut, submit);
    tit.on("keyup", previewSubmit);
    cat.on("keyup", previewSubmit);
    cont.on("keyup", previewSubmit);
    type.on("keyup", previewSubmit);
    access.on("keyup", previewSubmit);
    cont.on("change", previewSubmit);
    editList.on("change", loadPost);
    editdel.on("keyup", function() {
	if(editdel.val() == "DEL") editdel.addClass("editdeldanger");
	else editdel.removeClass("editdeldanger");
    });
});

previewSubmit = function() {
    dataObj = packData();
    $.ajax({
	type: "POST",
	url: "/edit/preview",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    preview.html(data);
	},
	error: function() { jsonError("errorJson in previewSubmit"); }
    });
};

submit = function() {
    if(!validateMinimal()) {
	responde("ney: misng info");
	return;
    }
    selId = getSelectedId();
    t = -1;
    if(selId == 0) t = 0;
    if(selId != 0) t = 1;
    if(selId != 0 && editdel.val() == "DEL") t = 2;
    dataObj = packData();
    dataObj["pid"] = selId;
    dataObj["submitType"] = t;
    $.ajax({
	type: "POST",
	url: "/edit/submit",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    responde(data);
	    editdel.val("");
	    editdel.removeClass("editdeldanger");
	},
	error: function() { jsonError("errorJson in submit"); }
    });
}

loadPost = function() {
    var selId = getSelectedId();
    if(selId == 0) {
	clearAll();
	cont.change();
	return;
    }
    var dataObj = { pid: selId }
    $.ajax({
	type: "POST",
	url: "/edit/loadpost",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    editid.val(data[0]);
	    editdc.val(procTime(data[1]["crtDate"]));
	    cont.val(data[1]["content"]);
	    tit.val(procTitle(data[1]["title"]));
	    cat.val(procCat(data[1]["categories"]));
	    type.val(data[1]["ptype"]);
	    access.val(data[1]["access"]);
	    cont.change();
	},
	error: function() { jsonError("errorJson in loadPost"); }
    });
}

packData = function() {
    var t = type.val();
    var a = access.val();
    if(t == "") t = "0";
    if(a == "") a = "5";
    var dataObj = {
	title: tit.val(),
	categories: cat.val(),
	content: cont.val(),
	type: t,
	access: a
    }
    return dataObj;
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

jsonError = function(m) {
    console.log(m);
    responde("ney: error json");
}

responde = function(m) {
    if(m.substring(0, 3) == "yey") {
	editresp.addClass("editrespyey");
	editresp.removeClass("editrespney");
    } else {
	editresp.removeClass("editrespyey");
	editresp.addClass("editrespney");
    }
    console.log(m.substring(0, 3));
    editresp.html(m);
}

clearAll = function() {
    tit.val(""); cat.val(""); cont.val("");
    type.val(""); access.val("");
    editid.val("");  editdc.val(""); editresp.text("");
}
