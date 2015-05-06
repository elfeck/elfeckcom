$(function() {
    container = $("#contleft");
    reg = $("#everegion");
    reg.focus();
    sites = [$("#s_0")];
    types = [$("#t_0")];

    eveList = $("#evelist");
    submitBut = $("#submitbutton");
    editdel = $("#editdelete");
    editid = $("#editid");
    editdc = $("#editdc");
    editresp = $("#editresp");

    reg.on("keyup", validateRegionH);
    sites[0].on("keyup", addNextSite);
    sites[0].on("keyup", validateSiteH);
    sites[0].on("keyup", handleSiteHotkey);
    types[0].on("keyup", validateTypeH);
    types[0].on("keyup", handleTypeHotkey);
    registerButton(submitBut, submit);
    eveList.on("change", loadVisit);
    editdel.on("keyup", function() {
	if(editdel.val() == "DEL") editdel.addClass("editdeldanger");
	else editdel.removeClass("editdeldanger");
    });
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

submit = function() {
    if(!validateAll()) {
	respond("ney: misng info");
	return;
    }
    selId = getSelectedId();
    t = -1;
    if(selId == 0) t = 0;
    if(selId != 0) t = 1;
    if(selId != 0 && editdel.val() == "DEL") t = 2;
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
	    editdel.val("");
	    editdel.removeClass("editdeldanger");
	},
	error: function() { jsonError("errorJson in submit"); }
    });
    reset();
}

loadVisit = function() {
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
	    reg.val(data[1]["region"]);
	    editid.val(data[0]);
	    editdc.val(procTime(data[1]["crtDate"]));
	    for(var i = 0; i < data[1]["sites"].length; ++i) {
		sites[i].val(data[1]["sites"][i][0]);
		types[i].val(data[1]["sites"][i][1]);
		addNextSite_();
	    }
	},
	error: function() { jsonError("errorJson in loadPost"); }
    });
}

addNextSite = function(event) {
    var regex = new RegExp("^[a-zA-Z0-9\b]+$");
    var key = String.fromCharCode(!event.charCode ? event.which :
				  event.charCode);
    if (regex.test(key)) addNextSite_();
}

addNextSite_ = function() {
    var curr = sites.length;
    container.append('<input class="editin evesite" id="s_' + curr +
		     '"></input>');
    container.append('<input class="editin evetype" id="t_' + curr +
		     '"></input>');
    sites.push($("#s_" + curr));
    types.push($("#t_" + curr));
    sites[curr - 1].off();
    sites[curr - 1].on("keyup", validateSiteH);
    sites[curr - 1].on("keyup", handleSiteHotkey);
    sites[curr].on("keyup", addNextSite);
    sites[curr].on("keyup", validateSiteH);
    sites[curr].on("keyup", handleSiteHotkey);
    types[curr].on("keyup", validateTypeH);
    types[curr].on("keyup", handleTypeHotkey);
}

reset = function(event) {
    for(var i = 1; i < sites.length; ++i) sites[i].remove();
    for(var i = 1; i < types.length; ++i) types[i].remove();
    sites = [sites[0]];
    sites[0].val("");
    sites[0].off();
    sites[0].on("keyup", addNextSite);
    sites[0].on("keyup", validateSiteH);
    sites[0].on("keyup", handleSiteHotkey);
    sites[0].focus();
    sites[0].removeClass("wrong");
    types = [types[0]];
    types[0].val("");
    types[0].removeClass("wrong");
    editid.val("");
    editdc.val("");
}

setKeybindings = function() {
    $(document).keypress(function(event) {
	if(event.altKey) event.preventDefault();
	if(event.keyCode == 13) submit();
    });
}

getSelectedId = function() {
    var selected = eveList.find("option:selected");
    return selected.attr("id");
}


packData = function() {
    var ssites = "";
    var ttypes = "";
    for(var i = 0; i < sites.length - 1; ++i) {
	ssites += sites[i].val();
	ttypes += types[i].val();
	if(i < sites.length - 2) {
	    ssites += ",";
	    ttypes += ",";
	}
    }
    var dataObj = {
	name: "",
	region: reg.val(),
	sites: ssites,
	types: ttypes
    };
    return dataObj;
}

allowedRegions = [
    "Branch", "Cache", "Catch", "Cloud Ring", "Cobalt Edge", "Curse",
    "Deklein", "Delve", "Detroid", "Esoteria", "Etherium Reach", "Fade",
    "Feythabolis", "Fountain", "Geminate", "Great Wildlands", "Immensea",
    "Impass", "Insmother", "The Kalevala Expanse", "Malpais", "Oasa", "Omist",
    "Outer Passage", "Outer Ring", "Paragon Soul", "Period Basis",
    "Perrigen Falls", "Providence", "Pure Blind", "Querious", "Scalding Pass",
    "The Spire", "Stain", "Syndicate", "Tenal", "Tenerifis", "Tribute",
    "Vale of the Silent", "Venal", "Wicked Creek"
];

allowedSites = [
    "Combat", "Data", "Relic", "Wormhole", "Gas"
];

allowedTypes = [
    "Monument Site", "Temple Site", "Science Outpost", "Crystal Quarry",

    "Sparking Transmitter", "Survey Site", "Command Center",
    "Data Mining Site",

    "Limited Sleeper Cache", "Standard Sleeper Cache",
    "Superior Sleeper Cache",

    ""
];

validateAll = function() {
    if(!validateRegion(reg)) return false;
    for(var i = 0; i < sites.length; i++) {
	if(!validateSite(sites[i])) return false;
	if(!validateType(types[i])) return false;
    }
    return true;
}

validateRegion = function(input) {
    if(allowedRegions.indexOf(input.val()) < 0) {
	input.addClass("wrong");
	return false;
    } else {
	input.removeClass("wrong");
	return true;
    }
};
validateRegionH = function() { validateRegion($(this)) };


validateSite = function(input) {
    var index = -1;
    for(var i = 0; i < sites.length; ++i) {
	if(input[0] == sites[i][0]) index = i;
    }
    bothEmpty = input.val() == "" && types[index].val() == "";
    if(!bothEmpty && allowedSites.indexOf(input.val()) < 0) {
	input.addClass("wrong");
	return false;
    } else {
	input.removeClass("wrong");
	return true;
    }
};
validateSiteH = function() { validateSite($(this)) };

validateType = function(input) {
    if(allowedTypes.indexOf(input.val()) < 0) {
	input.addClass("wrong");
	return false;
    } else {
	input.removeClass("wrong");
	return true;
    }
};
validateTypeH = function() { validateType($(this)) };

respond = function(m) {
    if(m.substring(0, 3) == "yey") {
	editresp.addClass("editrespyey");
	editresp.removeClass("editrespney");
	setTimeout(function() {
	    editresp.html("");
	}, 2000);
    } else {
	editresp.removeClass("editrespyey");
	editresp.addClass("editrespney");
    }
    console.log(m.substring(0, 3));
    editresp.html(m);
}

procTime = function(time) {
    return time.replace("T", " ").substring(0, 16);
}
