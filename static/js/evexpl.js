$(function() {
    region = $("#xRegion");
    region.on("keyup", validateRegionH);

    sites = [$("#s_0")];
    sites[0].on("keyup", addNextSite);
    sites[0].on("keyup", validateSiteH);
    sites[0].on("keydown", handleSiteHotkey);

    types = [$("#t_0")];
    types[0].on("keyup", validateTypeH);
    types[0].on("keydown", handleTypeHotkey);

    list = $("#xList");
    list.on("change", submitViewVisit);

    submitbutton = $("#xSubmitbutton");
    registerButton(submitbutton, submit);

    deletefield = $("#xDeletefield");
    deletefield.on("keyup", function() {
	console.log(deletefield.val());
	if(deletefield.val() == "DEL") {
	    deletefield.addClass("delDanger");
	} else {
	    deletefield.removeClass("delDanger");
	}
    });

    entryid = $("#xEntryid");
    entrydate = $("#xEntrydate");
    responsefield = $("#xResponsefield");
    container = $("#xContainer");

    region.focus();
    setKeybindings();
});

handleSiteHotkey = function(event) {
    if(event.altKey) {
	if(event.keyCode == 82) $(this).val("Relic");
	if(event.keyCode == 68) $(this).val("Data");
	if(event.keyCode == 67) $(this).val("Combat");
	if(event.keyCode == 87) $(this).val("Wormhole");
	if(event.keyCode == 71) $(this).val("Gas");
    }
}

handleTypeHotkey = function(event) {
    var index = -1;
    for(var i = 0; i < sites.length; ++i) {
	if($(this)[0] == types[i][0]) index = i;
    }
    if(event.altKey) {
	if(sites[index].val() == "Relic") {
	    if(event.keyCode == 81) $(this).val("Monument Site");
	    if(event.keyCode == 84) $(this).val("Temple Site");
	    if(event.keyCode == 83) $(this).val("Science Outpost");
	    if(event.keyCode == 67) $(this).val("Crystal Quarry");
	}
	if(sites[index].val() == "Data") {
	    if(event.keyCode == 84) $(this).val("Sparking Transmitter");
	    if(event.keyCode == 83) $(this).val("Survey Site");
	    if(event.keyCode == 67) $(this).val("Command Center");
	    if(event.keyCode == 68) $(this).val("Data Mining Site");

	    if(event.keyCode == 49) $(this).val("Limited Sleeper Cache");
	    if(event.keyCode == 50) $(this).val("Standard Sleeper Cache");
	    if(event.keyCode == 51) $(this).val("Superior Sleeper Cache");
	}
    }
}

setKeybindings = function() {
    $(document).keypress(function(event) {
	if(event.altKey) event.preventDefault();
	if(event.keyCode == 13) submit();
    });
}

submitViewVisit = function() {
    var selId = getSelectedId();
    if(selId == 0) {
	reset();
	return;
    }
    var dataObj = { eid: selId }
    $.ajax({
	type: "POST",
	url: "/evexpl/loadvisit",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    console.log(data);
	    reset();
	    region.val(data[1]["region"]);
	    entryid.val(data[0]);
	    entrydate.val(procTime(data[1]["crtDate"]));
	    if(data[1]["sites"].length == 1 && data[1]["sites"][0][0] == "") {
		// Nothing
	    } else {
		for(var i = 0; i < data[1]["sites"].length; ++i) {
		    sites[i].val(data[1]["sites"][i][0]);
		    types[i].val(data[1]["sites"][i][1]);
		    addNextSite_();
		}
	    }
	},
	error: function() { jsonError("errorJson in loadPost"); }
    });
}

submit = function() {
    if(!validateAll()) {
	respond("ney: misng info");
	return;
    }
    var selId = getSelectedId();
    var t = -1;
    if(selId == 0) t = 0;
    if(selId != 0) t = 1;
    if(selId != 0 && deletefield.val() == "DEL") t = 2;
    dataObj = packData();
    dataObj["eid"] = selId;
    dataObj["submitType"] = t;
    console.log(dataObj);
    $.ajax({
	type: "POST",
	url: "/evexpl/submit",
	dataType: "json",
	data: { dat: dataObj },
	success: function(data) {
	    respond(data);
	    deletefield.val("");
	    deletefield.removeClass("deletefielddanger");
	    updateList(t, selId, dataObj);
	    if(t == 0 || t == 2) {
		reset();
		resetSelected();
	    }
	    else submitViewVisit();
	},
	error: function() { jsonError("errorJson in submit"); }
    });
}

updateList = function(submitType, selId, data) {
    if(submitType == 0) {
	var id = queryMaxId() + 1;
	$("#0").after('<option id="' + id + '" class="listEntry">[' +
		      data["region"] + ' at ' + currTime() + ']</option>');
    }
    if(submitType == 1) {
	var oldT = $("#" + selId).text();
	var ind = oldT.lastIndexOf(" ") + 1;
	var oldTime = oldT.substr(ind).substr(0, 5);
	console.log(oldTime);
	$("#" + selId).html('[' + data["region"] + ' at ' +
			    oldTime + ']</option>');
    }
    if(submitType == 2) {
	$("#" + selId).remove();
    }
}

queryMaxId = function() {
    var eles = $(".listEntry");
    if(eles.length == 0) return 1;
    else return parseInt($(eles[0]).attr("id"));
}

addNextSite = function(event) {
    var regex = new RegExp("^[a-zA-Z0-9\b]+$");
    var key = String.fromCharCode(!event.charCode ? event.which :
				  event.charCode);
    if (regex.test(key)) addNextSite_();
}

addNextSite_ = function() {
    var curr = sites.length;
    container.append('<input class="stdinput xSite" id="s_' + curr +
		     '"></input>');
    container.append('<input class="stdinput xType" id="t_' + curr +
		     '"></input>');
    sites.push($("#s_" + curr));
    types.push($("#t_" + curr));
    sites[curr - 1].off();
    sites[curr - 1].on("keyup", validateSiteH);
    sites[curr - 1].on("keydown", handleSiteHotkey);
    sites[curr].on("keyup", addNextSite);
    sites[curr].on("keyup", validateSiteH);
    sites[curr].on("keydown", handleSiteHotkey);
    types[curr].on("keyup", validateTypeH);
    types[curr].on("keydown", handleTypeHotkey);
}

reset = function() {
    for(var i = 1; i < sites.length; ++i) sites[i].remove();
    for(var i = 1; i < types.length; ++i) types[i].remove();
    sites = [sites[0]];
    sites[0].val("");
    sites[0].off();
    sites[0].on("keyup", addNextSite);
    sites[0].on("keyup", validateSiteH);
    sites[0].on("keydown", handleSiteHotkey);
    sites[0].focus();
    sites[0].removeClass("wrong");
    types = [types[0]];
    types[0].val("");
    types[0].removeClass("wrong");
    entryid.val("");
    entrydate.val("");
    eles = $(".listEntry");
}

resetSelected = function() {
    for(var i = 0; i < eles.length; ++i) {
	$(eles[i]).prop("selected", false);
    }
    $("#0").prop("selected", true);
}

getSelectedId = function() {
    var selected = list.find("option:selected");
    return selected.attr("id");
}

packData = function() {
    var ssites = "";
    var ttypes = "";
    for(var i = 0; i < sites.length - 1; ++i) {
	if(ssites != "" && sites[i].val() != "") {
	    ssites += ",";
	    ttypes += ",";
	}
	ssites += sites[i].val();
	ttypes += types[i].val();
    }
    var dataObj = {
	name: "",
	region: region.val(),
	sites: ssites,
	types: ttypes
    };
    return dataObj;
}

respond = function(m) {
    if(m.substring(0, 3) == "yey") {
	responsefield.addClass("responseYey");
	responsefield.removeClass("responseNey");
	setTimeout(function() {
	    responsefield.html("");
	}, 2000);
    } else {
	responsefield.removeClass("responseYey");
	responsefield.addClass("responseNey");
    }
    console.log(m.substring(0, 3));
    responsefield.html(m);
}

procTime = function(time) {
    return time.replace("T", " ").substring(0, 16);
}

currTime = function() {
    d = new Date();
    return d.getUTCHours() + ":" + d.getUTCMinutes();
}
