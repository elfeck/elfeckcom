$(function() {
  fileinput = $("#fileinput");
  filename = $("#filename");
  responsefield = $("#responsefield");
  access = $("#access");

  fileList = $("#filelist");
  fileList.on("change", loadFile);

  deletefield = $("#deletefield");
  deletefield.on("keyup", function() {
    if(deletefield.val() == "DEL") {
      deletefield.addClass("delDanger");
    } else {
      deletefield.removeClass("delDanger");
    }
  });

  submitbutton = $("#submitbutton");
  registerButton(submitbutton, submit);

  loadChoices();
  clearAll();
});


submit = function() {
  var sel = fileList.find("option:selected");
  var t = -1;
  if(sel[0] != null) {
    if(deletefield.val() == "DEL") t = 2;
  }
  if($(sel[0]).html() == "[New upload]") {
    t = 1;
  }
  if(t == 1 && (filename.val() == "" || fileinput.val() == ""
		|| !isInt(access.val()))) {
    respond("ney: misng info");
    return;
  }
  if(t < 0) {
    respond("ney: no actn");
    return;
  }
  var formData = new FormData();
  if(t == 2) {
    formData.append("filename", $(sel[0]).html());
  } else {
    formData.append("filename", filename.val());
    formData.append("access", access.val());
    formData.append("file", fileinput[0].files[0]);
  }
  formData.append("submitType", t);
  $.ajax({
    type: "POST",
    url: "upload/submit",
    xhr: function() {
      var myXhr = $.ajaxSettings.xhr();
      if(myXhr.upload){
        myXhr.upload.addEventListener('progress',function() {

	}, false);
      }
      return myXhr;
    },
    success: function(data) {
      respond(data);
      loadChoices();
      deletefield.val("");
      deletefield.removeClass("delDanger");
      clearAll();
      //console.log(data);
    },
    error: function() {
      respond("ney: errorJson");
    },
    data: formData,
    cache: false,
    contentType: false,
    processData: false
  });
};

clearAll = function() {
  fileinput.val("");
  filename.val("");
  access.val("");
}

loadChoices = function() {
  var dataObj = { }
  $.ajax({
    type: "POST",
    url: "upload/loadchoices",
    dataType: "json",
    data: dataObj,
    success: function(data) {
      fileList.empty();
      fileList.append($("<option>", {
	text: "[New upload]",
	selected: "selected" }));
      for(var i = 0; i < data.length; i++) {
	//console.log(data[i]);
	fileList.append($("<option>", {
	  text: data[i]
	}))
      }
      fileList.focus();
    },
    error: function() {
      respond("ney: errorJson");
    }
  });
}

loadFile = function() {
  var sel = fileList.find("option:selected");
  if($(sel[0]).html() == "[New upload]") {
    clearAll();
  } else {
    var fileName = $(sel[0]).html();
    var acc = fileName.substring(0, 3)[1];
    access.val(acc);
    filename.val(fileName.substring(3, fileName.length));
  }
}

respond = function(m) {
  if(m.substring(0, 3) == "yey") {
    responsefield.addClass("responseYey");
    responsefield.removeClass("responseNey");
  } else {
    responsefield.removeClass("responseYey");
    responsefield.addClass("responseNey");
  }
  responsefield.text(m);
  window.setTimeout(function() {
    responsefield.text("");
  }, 1000);
}

isInt = function(value) {
  return !isNaN(value) &&
    parseInt(Number(value)) == value &&
    !isNaN(parseInt(value, 10));
}
