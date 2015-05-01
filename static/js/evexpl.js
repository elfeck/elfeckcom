$(function() {
    container = $("#contleft");
    reg = $("#everegion");
    reg.focus();
    sites = [$("#s_0")];
    types = [$("#t_0")];

    eveList = $("#evelist");

    reg.on("keyup", validateRegionH);
    sites[0].on("keyup", addNextSite);
    sites[0].on("keyup", validateSiteH);
    types[0].on("keyup", validateTypeH);
    setKeybindings();
});

submit = function() {
    console.log(validateAll());
    if(validateAll()) {
	console.log(packData());
    }
    reset();
}

addNextSite = function(event) {
    var regex = new RegExp("^[a-zA-Z0-9\b]+$");
    var key = String.fromCharCode(!event.charCode ? event.which :
				  event.charCode);
    if (regex.test(key)) {
	var curr = sites.length;
	container.append('<input class="editin evesite" id="s_' + curr +
			 '"></input>');
	container.append('<input class="editin evetype" id="t_' + curr +
			 '"></input>');
	sites.push($("#s_" + curr));
	types.push($("#t_" + curr));
	sites[curr - 1].off();
	sites[curr - 1].on("keyup", validateSiteH);
	sites[curr].on("keyup", addNextSite);
	sites[curr].on("keyup", validateSiteH);
	types[curr].on("keyup", validateTypeH);
    }
}

reset = function(event) {
    for(var i = 1; i < sites.length; ++i) sites[i].remove();
    for(var i = 1; i < types.length; ++i) types[i].remove();
    sites = [sites[0]];
    sites[0].val("");
    sites[0].off();
    sites[0].on("keyup", addNextSite);
    sites[0].on("keyup", validateSiteH);
    sites[0].focus();
    sites[0].removeClass("wrong");
    types = [types[0]];
    types[0].val("");
    types[0].removeClass("wrong");
}

setKeybindings = function() {
    $(document).keypress(function(event) {
	if(event.keyCode == 13) submit();
    });
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

    "Limited Sleeper Cache", "Normal Sleeper Cache",
    "Superior Sleeper Cache",

    ""
];

packData = function() {
    var siteData = [];
    for(var i = 0; i < sites.length; ++i) {
	if(sites[i].val() == "") continue;
	siteData.push(site = {
	    name: sites[i].val(),
	    type: types[i].val()
	});
    }
    var dataObj = {
	region: reg.val(),
	siteData: siteData
    };
    return dataObj;
}

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
