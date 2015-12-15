$(function() {
  fileinput = $("#fileinput");
  filename = $("#filename");
  responsefield = $("#responsefiled");
  access = $("#access");

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
});


submit = function() {
  if(filename.val() == "" || fileinput.val() == "" || !isInt(access.val())) {
    respond("ney: misng info");
    return;
  }
  var formData = new FormData();
  formData.append("filename", filename.val());
  formData.append("access", access.val());
  formData.append("file", fileinput[0].files[0]);
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
      console.log("yey" + data);
    },
    error: function() {
      console.log("ney: error in ajax");
    },
    data: formData,
    cache: false,
    contentType: false,
    processData: false
  });
};

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